use clap::{Parser, Subcommand, ValueEnum};
use facalculo::{
    compute, Category, Graph, IngredientRate, Module, ModuleBuilder, RecipeRate, RecipeRepository,
};
use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec, parse,
    printer::PrinterContext,
};
use petgraph::{prelude::GraphMap, Directed};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::{
    collections::{HashMap, HashSet},
    fs,
    process::Command,
    str::FromStr,
};

#[derive(Parser, Debug)]
#[command()]
struct Args {
    #[arg(long)]
    render: bool,
    #[arg(long)]
    total: bool,
    #[arg(long)]
    out: bool,
    #[arg(long, value_parser = 1..=3, default_value_t = 1)]
    asm: i64,
    #[arg(long)]
    debug: bool,
    #[arg(long, value_enum)]
    belt: Option<Belt>,
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Clone, Debug, ValueEnum)]
enum Belt {
    Transport = 15,
    Fast = 30,
    Express = 45,
}

#[derive(Debug, Subcommand)]
enum Commands {
    Generate {
        #[arg(short, long, num_args = 1..=2)]
        item: Vec<Vec<String>>,
        #[arg(short, long)]
        rate: Option<Decimal>,
        #[arg(long)]
        expand: bool,
        #[arg(long)]
        import: Vec<String>,
    },
    Render {
        files: Vec<String>,
        #[arg(short, long, num_args = 2)]
        item: Vec<Vec<String>>,
        #[arg(short, long)]
        rate: Option<Decimal>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let b = include_bytes!("data-raw-dump.json");
    let data: Data = serde_json::from_slice(b)?;
    let recipe_rates = calculate_rates(&data, args.asm);
    let belt = args.belt.map(|b| b as i64);
    match args.command {
        None => (),
        Some(Commands::Generate {
            item: items,
            expand,
            import,
            rate,
        }) => {
            if args.debug {
                let data: Value = serde_json::from_slice(b)?;
                println!(
                    "{:?}",
                    data["recipes"]
                        .as_array()
                        .expect("recipes should be an array")
                        .iter()
                        .find(|r| r["key"] == items[0][0])
                        .ok_or(format!("{} was not found", items[0][0]))?
                );
                return Ok(());
            }
            let mut imports = Vec::new();
            for import in import {
                if recipe_rates.get(import.as_str()).is_some() {
                    imports.push(import);
                } else {
                    let module: Module = serde_json::from_slice::<Module>(&fs::read(import)?)?;
                    imports.extend(module.outputs);
                }
            }
            let mut modules = Vec::new();
            let mut required = HashMap::new();
            for item in items {
                let mut iter = item.iter();
                let name = iter.next().unwrap();
                let mut module = ModuleBuilder::new(name.to_owned(), &recipe_rates, &imports);
                module.add(name, expand);
                modules.push(module.build());
                get_recipe(&recipe_rates, name)?;
                if let Some(rate) = iter.next() {
                    required.insert(name.clone(), rate.parse()?);
                }
            }
            let defaults = recipe_rates
                .recipes
                .iter()
                .map(|(k, r)| {
                    (
                        (*k).to_owned(),
                        if let Some(rate) = rate {
                            Ok(rate)
                        } else {
                            Err(r.results[0].rate)
                        },
                    )
                })
                .collect();
            let graphs: Vec<_> = modules
                .iter()
                .cloned()
                .map(|m| Graph::from_module(m, &required, &defaults, &recipe_rates, belt, 0))
                .collect();
            let out = compute::render(&graphs, &HashMap::new(), &HashSet::new())?;
            if args.render {
                let g = parse(&out)?;
                exec(
                    g,
                    &mut PrinterContext::default(),
                    vec![
                        Format::Svg.into(),
                        CommandArg::Output("out.svg".to_string()),
                    ],
                )?;
                Command::new("open").arg("out.svg").spawn()?;
            }
            if args.total {
                for (key, required) in compute::total(&graphs) {
                    println!(
                        "{} {key}/s{}",
                        facalculo::round_string(required),
                        if let Some(recipe) = recipe_rates.get(key) {
                            format!(
                                " ({})",
                                facalculo::round_string(required / recipe.results[0].rate)
                            )
                        } else {
                            String::new()
                        }
                    );
                }
            }
            if args.out {
                for module in modules {
                    println!("{}", serde_json::to_string_pretty(&module)?);
                }
            }
        }
        Some(Commands::Render {
            files,
            item: items,
            rate,
        }) => {
            let modules = files
                .into_iter()
                .map(
                    |f| -> Result<(String, Module), Box<dyn std::error::Error>> {
                        let module: Module = serde_json::from_slice(&fs::read(f)?)?;
                        Ok((module.name.clone(), module))
                    },
                )
                .collect::<Result<Vec<_>, _>>()?;
            let modules: HashMap<_, _> = modules.into_iter().collect();
            let outputs: HashSet<_> = modules.values().flat_map(|m| m.outputs.clone()).collect();
            let rates = items
                .into_iter()
                .map(|items| {
                    if let [item, rate] = &items[..] {
                        Ok((item.clone(), rate.parse()?))
                    } else {
                        unimplemented!()
                    }
                })
                .collect::<Result<HashMap<_, Decimal>, Box<dyn std::error::Error>>>()?;
            let mut required = HashMap::new();
            let defaults = recipe_rates
                .recipes
                .iter()
                .map(|(k, r)| {
                    (
                        (*k).to_owned(),
                        if let Some(rate) = rate {
                            Ok(rate)
                        } else {
                            Err(r.results[0].rate)
                        },
                    )
                })
                .collect();
            for o in outputs {
                if let Some(rate) = rates.get(&o) {
                    required.insert(o.clone(), *rate);
                }
            }
            let mut graph = GraphMap::<&str, (), Directed>::new();
            for (node, module) in &modules {
                for input in module.inputs.keys() {
                    if node != input {
                        graph.add_edge(node, input, ());
                    }
                }
            }

            // Sort modules so outputs are processed before inputs
            let mut index = 2;
            let mut imports: HashMap<String, Vec<(usize, Decimal)>> = HashMap::new();
            let mut graphs = Vec::new();
            let mut used_imports = HashSet::new();
            for module in petgraph::algo::toposort(&graph, None).unwrap() {
                let graph = Graph::from_module(
                    modules[module].clone(),
                    &required,
                    &defaults,
                    &recipe_rates,
                    belt,
                    index,
                );
                for (import, dependencies) in &graph.imports {
                    for (_, rate) in dependencies {
                        *required.entry(import.clone()).or_default() += rate;
                    }
                    imports
                        .entry(import.clone())
                        .or_default()
                        .extend(dependencies);
                    used_imports.insert(import.clone());
                }
                index += graph.graph.node_count();
                graphs.push(graph);
            }
            let out = compute::render(&graphs, &imports, &used_imports)?;
            let g = parse(&out)?;
            exec(
                g,
                &mut PrinterContext::default(),
                vec![
                    Format::Svg.into(),
                    CommandArg::Output("out.svg".to_string()),
                ],
            )?;
            Command::new("open").arg("out.svg").spawn()?;
        }
    };
    Ok(())
}

fn get_recipe<'a>(recipes: &'a RecipeRepository, name: &str) -> Result<&'a RecipeRate, String> {
    if let Some(recipe) = recipes.get(name) {
        Ok(recipe)
    } else {
        let mut found = false;
        for k in recipes.recipes.keys() {
            if k.contains(name) {
                if !found {
                    found = true;
                    eprintln!("{name} was not found. Similar items:");
                }
                eprintln!("{}", k);
            }
        }
        Err(format!("{name} was not found"))
    }
}

#[derive(Debug, Deserialize)]
struct Data<'a> {
    recipe: HashMap<&'a str, Recipe>,
    #[serde(borrow)]
    resource: HashMap<&'a str, Resource>,
}

#[derive(Debug, Deserialize)]
struct Recipe {
    category: Option<Category>,
    energy_required: Option<Decimal>,
    ingredients: Option<Value>,
    results: Option<Value>,
}

#[derive(Clone, Debug, Deserialize)]
struct Ingredient {
    amount: Decimal,
    name: String,
}

#[derive(Clone, Debug, Deserialize)]
struct RecipeResult {
    amount: Decimal,
    name: String,
}

#[derive(Debug, Deserialize)]
struct Resource {
    minable: Mineable,
}

#[derive(Debug, Deserialize)]
struct Mineable {
    mining_time: Decimal,
}

fn calculate_rates(data: &Data, asm: i64) -> RecipeRepository {
    let mut recipe_rates: HashMap<_, _> = data
        .recipe
        .iter()
        .filter_map(|(key, r)| {
            let energy_required = r
                .energy_required
                .unwrap_or(Decimal::from_str("0.5").unwrap());
            let (category, Some(Value::Array(ingredients)), Some(Value::Array(results))) =
                (r.category, &r.ingredients, &r.results)
            else {
                return None;
            };
            let speed = match r.category {
                None
                | Some(Category::Crafting)
                | Some(Category::CraftingWithFluid)
                | Some(Category::Electronics)
                | Some(Category::ElectronicsWithFluid)
                | Some(Category::Pressing) => match asm {
                    1 => Decimal::from_str("0.5").unwrap(),
                    2 => Decimal::from_str("0.75").unwrap(),
                    3 => Decimal::from_str("1.25").unwrap(),
                    _ => unimplemented!(),
                },
                Some(Category::Smelting) => Decimal::new(2, 0),
                _ => Decimal::ONE,
            };
            let rate = RecipeRate {
                category,
                key: (*key).to_owned(),
                ingredients: ingredients
                    .iter()
                    .cloned()
                    .filter_map(|i| {
                        let i: Ingredient = serde_json::from_value(i).ok()?;
                        Some(IngredientRate {
                            rate: i.amount / energy_required * speed,
                            name: i.name,
                        })
                    })
                    .collect(),
                results: results
                    .iter()
                    .cloned()
                    .filter_map(|i| {
                        let i: RecipeResult = serde_json::from_value(i).ok()?;
                        Some(IngredientRate {
                            rate: i.amount / energy_required * speed,
                            name: i.name,
                        })
                    })
                    .collect(),
            };
            Some(((*key).to_owned(), rate))
        })
        .collect();
    // Add mining recipes
    for (key, resource) in &data.resource {
        let rate = RecipeRate {
            category: None,
            key: (*key).to_owned(),
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: resource.minable.mining_time * Decimal::from_str("0.5").unwrap(),
                name: (*key).to_owned(),
            }],
        };
        recipe_rates.insert((*key).to_owned(), rate);
    }
    // Add water pumping
    recipe_rates.insert(
        "water".to_owned(),
        RecipeRate {
            category: None,
            key: "water".to_owned(),
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: Decimal::new(1200, 0),
                name: String::from("water"),
            }],
        },
    );
    RecipeRepository {
        recipes: recipe_rates,
        recipe_outputs: HashMap::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[]);
        builder.add("advanced-circuit", true);
        let graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().results[0].rate,
            )]),
            &HashMap::new(),
            &recipe_rates,
            None,
            0,
        );
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1 advanced-circuit",
                "0.167 coal",
                "0.167 copper-cable",
                "0.250 copper-cable",
                "0.333 copper-ore",
                "0.500 copper-ore",
                "0.267 copper-plate",
                "0.400 copper-plate",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.083 plastic-bar",
            ]
        );
        let mut edges: Vec<_> = graph
            .edge_indices()
            .map(|i| {
                let (ix, iy) = graph.edge_endpoints(i).unwrap();
                (graph[ix].clone(), graph[i].clone(), graph[iy].clone())
            })
            .collect();
        edges.sort_by_key(|(x, e, y)| (x.name.to_owned(), y.name.to_owned(), e.required));
        let edges: Vec<_> = edges
            .into_iter()
            .map(|(x, e, y)| {
                format!(
                    "{} -> {} -> {}",
                    x.to_string(),
                    e.to_string(),
                    y.to_string()
                )
            })
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.167 copper-cable -> 0.167 cp -> 0.267 copper-plate",
                "0.250 copper-cable -> 0.250 cp -> 0.400 copper-plate",
                "0.267 copper-plate -> 0.167 co -> 0.333 copper-ore",
                "0.400 copper-plate -> 0.250 co -> 0.500 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.250 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }

    #[test]
    fn group_all_advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[]);
        builder.add("advanced-circuit", true);
        let mut graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().results[0].rate,
            )]),
            &HashMap::new(),
            &recipe_rates,
            None,
            0,
        );
        graph.group_nodes(Vec::new());
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1 advanced-circuit",
                "0.167 coal",
                "0.417 copper-cable",
                "0.833 copper-ore",
                "0.667 copper-plate",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.083 plastic-bar",
            ]
        );
        let mut edges: Vec<_> = graph
            .edge_indices()
            .map(|i| {
                let (ix, iy) = graph.edge_endpoints(i).unwrap();
                (graph[ix].clone(), graph[i].clone(), graph[iy].clone())
            })
            .collect();
        edges.sort_by_key(|(x, e, y)| (x.name.to_owned(), y.name.to_owned(), e.required));
        let edges: Vec<_> = edges
            .into_iter()
            .map(|(x, e, y)| {
                format!(
                    "{} -> {} -> {}",
                    x.to_string(),
                    e.to_string(),
                    y.to_string()
                )
            })
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.417 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.417 copper-cable -> 0.417 cp -> 0.667 copper-plate",
                "0.667 copper-plate -> 0.417 co -> 0.833 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.417 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }

    #[test]
    fn group_advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[]);
        builder.add("advanced-circuit", true);
        let mut graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates
                    .recipes
                    .get("advanced-circuit")
                    .unwrap()
                    .results[0]
                    .rate,
            )]),
            &HashMap::new(),
            &recipe_rates,
            None,
            0,
        );
        graph.group_nodes(vec![String::from("copper-plate")]);
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1 advanced-circuit",
                "0.167 coal",
                "0.167 copper-cable",
                "0.250 copper-cable",
                "0.833 copper-ore",
                "0.667 copper-plate",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.083 plastic-bar",
            ]
        );
        let mut edges: Vec<_> = graph
            .edge_indices()
            .map(|i| {
                let (ix, iy) = graph.edge_endpoints(i).unwrap();
                (graph[ix].clone(), graph[i].clone(), graph[iy].clone())
            })
            .collect();
        edges.sort_by_key(|(x, e, y)| (x.name.to_owned(), y.name.to_owned(), e.required));
        let edges: Vec<_> = edges
            .into_iter()
            .map(|(x, e, y)| {
                format!(
                    "{} -> {} -> {}",
                    x.to_string(),
                    e.to_string(),
                    y.to_string()
                )
            })
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.167 copper-cable -> 0.167 cp -> 0.667 copper-plate",
                "0.250 copper-cable -> 0.250 cp -> 0.667 copper-plate",
                "0.667 copper-plate -> 0.417 co -> 0.833 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.250 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }
}
