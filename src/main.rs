use clap::{Parser, Subcommand, ValueEnum};
use facalculo::{compute, Category, Graph, IngredientRate, Module, RecipeRate};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, str::FromStr};

#[derive(Parser, Debug)]
#[command()]
struct Args {
    #[arg(short, long, num_args = 1..=2)]
    items: Vec<Vec<String>>,
    #[arg(short, long)]
    rate: Option<Decimal>,
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
        item: Vec<String>,
        #[arg(long)]
        expand: bool,
        #[arg(long)]
        import: Vec<String>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let b = include_bytes!("space-age-2.0.11.json");
    if args.debug {
        let data: Value = serde_json::from_slice(b)?;
        println!(
            "{:?}",
            data["recipes"]
                .as_array()
                .expect("recipes should be an array")
                .iter()
                .find(|r| r["key"] == args.items[0][0])
                .ok_or(format!("{} was not found", args.items[0][0]))?
        );
        return Ok(());
    }
    let data: Data = serde_json::from_slice(b)?;
    let recipe_rates = calculate_rates(&data, args.asm);
    let belt = args.belt.map(|b| b as i64);
    if let Some(Commands::Generate {
        item,
        expand,
        import,
    }) = args.command
    {
        let mut iter = item.iter();
        let name = iter.next().unwrap();
        let recipe = get_recipe(&recipe_rates, name)?;
        let required = if let Some(rate) = iter.next() {
            rate.parse()?
        } else if let Some(rate) = args.rate {
            rate
        } else {
            let rate = recipe.results[0].rate;
            eprintln!(
                "Using {} for {name} (1 assembler)",
                facalculo::round_string(rate)
            );
            rate
        };
        let imports: Vec<_> = import.iter().map(String::as_str).collect();
        let mut m = Module::default();
        m.add(&recipe_rates, name, expand, &imports);
        let graph = Graph::from_module(
            m,
            HashMap::from_iter([(name.clone(), required)]),
            &recipe_rates,
            belt,
        );
        if args.render {
            compute::render(&graph)?;
        }
        if args.total {
            for (key, required) in compute::total(&graph) {
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
            println!("{}", serde_json::to_string_pretty(&graph.to_raw())?);
        }
    }
    Ok(())
}

fn get_recipe<'a>(
    recipes: &'a HashMap<&str, RecipeRate<'a>>,
    name: &str,
) -> Result<&'a RecipeRate<'a>, String> {
    if let Some(recipe) = recipes.get(name) {
        Ok(recipe)
    } else {
        let mut found = false;
        for k in recipes.keys() {
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
    #[serde(borrow)]
    recipes: Vec<Recipe<'a>>,
    #[serde(borrow)]
    resources: Vec<Resource<'a>>,
}

#[derive(Debug, Deserialize)]
struct Recipe<'a> {
    category: Category,
    #[serde(borrow)]
    key: &'a str,
    energy_required: Decimal,
    ingredients: Vec<Ingredient>,
    results: Vec<RecipeResult>,
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
struct Resource<'a> {
    #[serde(borrow)]
    key: &'a str,
    mining_time: Decimal,
}

fn calculate_rates<'a>(data: &'a Data<'a>, asm: i64) -> HashMap<&str, RecipeRate<'a>> {
    let mut recipe_rates: HashMap<_, _> = data
        .recipes
        .iter()
        .map(|r| {
            let speed = match r.category {
                Category::Crafting
                | Category::CraftingWithFluid
                | Category::Electronics
                | Category::ElectronicsWithFluid => match asm {
                    1 => Decimal::from_str("0.5").unwrap(),
                    2 => Decimal::from_str("0.75").unwrap(),
                    3 => Decimal::from_str("1.25").unwrap(),
                    _ => unimplemented!(),
                },
                Category::Smelting => Decimal::new(2, 0),
                _ => Decimal::ONE,
            };
            let rate = RecipeRate {
                category: r.category,
                key: r.key,
                ingredients: r
                    .ingredients
                    .iter()
                    .cloned()
                    .map(|i| IngredientRate {
                        rate: i.amount / r.energy_required * speed,
                        name: i.name,
                    })
                    .collect(),
                results: r
                    .results
                    .iter()
                    .cloned()
                    .map(|i| IngredientRate {
                        rate: i.amount / r.energy_required * speed,
                        name: i.name,
                    })
                    .collect(),
            };
            (r.key, rate)
        })
        .collect();
    // Add oil recipes
    let advanced_oil_processing = data
        .recipes
        .iter()
        .find(|r| r.key == "advanced-oil-processing")
        .expect("advanced-oil-processing should exist");
    for result in &advanced_oil_processing.results {
        recipe_rates.insert(
            &result.name,
            RecipeRate {
                category: advanced_oil_processing.category,
                key: &result.name,
                ingredients: advanced_oil_processing
                    .ingredients
                    .iter()
                    .cloned()
                    .map(|i| IngredientRate {
                        rate: i.amount / advanced_oil_processing.energy_required,
                        name: i.name,
                    })
                    .collect(),
                results: vec![IngredientRate {
                    rate: result.amount / advanced_oil_processing.energy_required,
                    name: result.name.clone(),
                }],
            },
        );
    }
    // Add mining recipes
    for resource in &data.resources {
        let rate = RecipeRate {
            category: Category::Mining,
            key: resource.key,
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: resource.mining_time * Decimal::from_str("0.5").unwrap(),
                name: resource.key.to_owned(),
            }],
        };
        recipe_rates.insert(resource.key, rate);
    }
    // Add water pumping
    recipe_rates.insert(
        "water",
        RecipeRate {
            category: Category::Mining,
            key: "water",
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: Decimal::new(1200, 0),
                name: String::from("water"),
            }],
        },
    );
    recipe_rates
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn advanced_circuit() {
        let b = include_bytes!("space-age-2.0.11.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut graph = Graph::new(&recipe_rates);
        graph.add(
            recipe_rates["advanced-circuit"].results[0].rate,
            "advanced-circuit",
            None,
            true,
            &[],
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
                "6.061 crude-oil",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.152 petroleum-gas",
                "0.083 plastic-bar",
                "0.001 water",
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
                "0.152 petroleum-gas -> 3.030 co -> 6.061 crude-oil",
                "0.152 petroleum-gas -> 1.515 w -> 0.001 water",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
                "0.083 plastic-bar -> 1.667 pg -> 0.152 petroleum-gas",
            ]
        );
    }

    #[test]
    fn group_all_advanced_circuit() {
        let b = include_bytes!("space-age-2.0.11.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut graph = Graph::new(&recipe_rates);
        graph.add(
            recipe_rates["advanced-circuit"].results[0].rate,
            "advanced-circuit",
            None,
            true,
            &[],
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
                "6.061 crude-oil",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.152 petroleum-gas",
                "0.083 plastic-bar",
                "0.001 water",
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
                "0.152 petroleum-gas -> 3.030 co -> 6.061 crude-oil",
                "0.152 petroleum-gas -> 1.515 w -> 0.001 water",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
                "0.083 plastic-bar -> 1.667 pg -> 0.152 petroleum-gas",
            ]
        );
    }

    #[test]
    fn group_advanced_circuit() {
        let b = include_bytes!("space-age-2.0.11.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = calculate_rates(&data, 1);
        let mut graph = Graph::new(&recipe_rates);
        graph.add(
            recipe_rates["advanced-circuit"].results[0].rate,
            "advanced-circuit",
            None,
            true,
            &[],
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
                "6.061 crude-oil",
                "0.167 electronic-circuit",
                "0.333 iron-ore",
                "0.267 iron-plate",
                "0.152 petroleum-gas",
                "0.083 plastic-bar",
                "0.001 water",
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
                "0.152 petroleum-gas -> 3.030 co -> 6.061 crude-oil",
                "0.152 petroleum-gas -> 1.515 w -> 0.001 water",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
                "0.083 plastic-bar -> 1.667 pg -> 0.152 petroleum-gas",
            ]
        );
    }
}
