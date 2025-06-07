use clap::{Parser, Subcommand, ValueEnum};
use facalculo::{
    compute,
    data::{self, Data, RepositoryOption},
    graph::{Graph, Import},
    module::NamedModule,
};
use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec, parse,
    printer::PrinterContext,
};
use petgraph::{prelude::GraphMap, Directed};
use rust_decimal::Decimal;
use serde_json::Value;
use std::{
    collections::{HashMap, HashSet},
    fs,
    process::Command,
};

#[derive(Parser, Debug)]
#[command()]
struct Args {
    #[arg(long)]
    render: bool,
    #[arg(long)]
    total: bool,
    #[arg(long)]
    out: Option<String>,
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
        #[arg(long)]
        recipe: Vec<String>,
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
    let recipe_rates = data::calculate_rates(&data, args.asm);
    let _belt = args.belt.map(|b| b as i64);
    match args.command {
        None => (),
        Some(Commands::Generate {
            item: items,
            expand,
            import,
            rate,
            recipe,
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
            for import in &import {
                if recipe_rates.get(import.as_str()).is_some()
                    || data.fluid.contains_key(import.as_str())
                {
                    imports.push(import.clone());
                } else {
                    let module = serde_json::from_slice::<NamedModule>(&fs::read(import)?)?;
                    imports.extend(
                        recipe_rates
                            .get_outputs(&module.module)
                            .into_iter()
                            .map(ToOwned::to_owned),
                    );
                }
            }
            let recipes = recipe.into_iter().collect();
            let mut modules = Vec::new();
            let mut required = HashMap::new();
            for item in items {
                let mut iter = item.iter();
                let name = iter.next().unwrap();
                modules.push(facalculo::generate(
                    name,
                    expand,
                    &import,
                    &recipes,
                    &recipe_rates,
                )?);
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
                .map(|m| Graph::from_module(m, &required, &defaults, &recipe_rates))
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
                                facalculo::round_string(required / recipe.rate().results[0].rate)
                            )
                        } else {
                            String::new()
                        }
                    );
                }
            }
            if args.out.is_some() {
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
            // Parse arguments
            let mut modules = files
                .into_iter()
                .map(|f| -> Result<_, Box<dyn std::error::Error>> {
                    let module: NamedModule = serde_json::from_slice(&fs::read(f)?)?;
                    Ok((module.name.clone(), module))
                })
                .collect::<Result<HashMap<_, _>, _>>()?;
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

            // Sort modules so outputs are processed before inputs
            let mut graph = GraphMap::<&str, (), Directed>::new();
            for (node, module) in &modules {
                graph.add_node(node);
                for input in recipe_rates.get_resource_inputs(&module.module) {
                    if node != input {
                        match recipe_rates.get_options(input) {
                            RepositoryOption::None => {}
                            RepositoryOption::Some(recipe) => {
                                graph.add_edge(node, &recipe.rate().key, ());
                            }
                            RepositoryOption::Multiple(recipes) => {
                                for recipe in recipes {
                                    if modules.contains_key(recipe) {
                                        graph.add_edge(node, recipe, ());
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            let module_order: Vec<_> = petgraph::algo::toposort(&graph, None)
                .unwrap()
                .into_iter()
                .map(ToOwned::to_owned)
                .collect();
            let mut required = HashMap::new();
            let outputs: HashSet<_> = modules
                .values()
                .flat_map(|m| recipe_rates.get_outputs(&m.module))
                .collect();
            for o in &outputs {
                if let Some(rate) = rates.get(*o) {
                    required.insert((*o).to_owned(), *rate);
                }
            }
            let defaults = recipe_rates
                .recipes()
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
            let mut index = 2;
            let mut graphs = Vec::new();
            let mut imports: HashMap<String, Vec<(usize, Decimal)>> = HashMap::new();
            for module in module_order {
                let graph = Graph::from_module(
                    modules.remove(&module).unwrap(),
                    &required,
                    &defaults,
                    &recipe_rates,
                );
                for (import, node) in &graph.imports {
                    // We do not create import nodes for science packs
                    let (import_required, node) = match node {
                        Import::Resource(..) => continue,
                        Import::Node(node) => (graph.graph[*node].required.unwrap(), node),
                        Import::Import(node, required) => (*required, node),
                    };
                    *required.entry(import.clone()).or_default() += import_required;
                    imports
                        .entry(import.clone())
                        .or_default()
                        .push((index + node.index(), import_required));
                }
                index += graph.graph.node_count();
                graphs.push(graph);
            }
            let out = compute::render(
                &graphs,
                &imports,
                &outputs.into_iter().map(ToOwned::to_owned).collect(),
            )?;
            let g = parse(&out)?;
            let format = match args.out.as_deref() {
                Some("pdf") => Format::Pdf,
                Some("svg") | None => Format::Svg,
                _ => unimplemented!(),
            };
            exec(
                g,
                &mut PrinterContext::default(),
                vec![
                    format.into(),
                    CommandArg::Output(format!("out.{:?}", format).to_lowercase()),
                ],
            )?;
            // Uncomment when running on mac
            // Command::new("open").arg("out.svg").spawn()?;
        }
    };
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use facalculo::module::ModuleBuilder;

    #[test]
    fn advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = data::calculate_rates(&data, 1);
        let recipes = get_recipes();
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[], &recipes);
        builder.add("advanced-circuit", true);
        let graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().rate().results[0].rate,
            )]),
            &HashMap::new(),
            &recipe_rates,
        );
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1 advanced-circuit",
                "0.185 basic-oil-processing",
                "0.167 coal",
                "0.167 copper-cable",
                "0.250 copper-cable",
                "0.333 copper-ore",
                "0.500 copper-ore",
                "0.267 copper-plate",
                "0.400 copper-plate",
                "1.852 crude-oil",
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
            .map(|(x, e, y)| format!("{} -> {} -> {}", x, e, y))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.185 basic-oil-processing -> 3.704 co -> 1.852 crude-oil",
                "0.167 copper-cable -> 0.167 cp -> 0.267 copper-plate",
                "0.250 copper-cable -> 0.250 cp -> 0.400 copper-plate",
                "0.267 copper-plate -> 0.167 co -> 0.333 copper-ore",
                "0.400 copper-plate -> 0.250 co -> 0.500 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.250 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 1.667 pg -> 0.185 basic-oil-processing",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }

    #[test]
    fn group_all_advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = data::calculate_rates(&data, 1);
        let recipes = get_recipes();
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[], &recipes);
        builder.add("advanced-circuit", true);
        let mut graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().rate().results[0].rate,
            )]),
            &HashMap::new(),
            &recipe_rates,
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
                "0.185 basic-oil-processing",
                "0.167 coal",
                "0.417 copper-cable",
                "0.833 copper-ore",
                "0.667 copper-plate",
                "1.852 crude-oil",
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
            .map(|(x, e, y)| format!("{} -> {} -> {}", x, e, y))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.417 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.185 basic-oil-processing -> 3.704 co -> 1.852 crude-oil",
                "0.417 copper-cable -> 0.417 cp -> 0.667 copper-plate",
                "0.667 copper-plate -> 0.417 co -> 0.833 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.417 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 1.667 pg -> 0.185 basic-oil-processing",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }

    #[test]
    fn group_advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = data::calculate_rates(&data, 1);
        let recipes = get_recipes();
        let mut builder =
            ModuleBuilder::new("advanced-circuit".to_owned(), &recipe_rates, &[], &recipes);
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
                "0.185 basic-oil-processing",
                "0.167 coal",
                "0.167 copper-cable",
                "0.250 copper-cable",
                "0.833 copper-ore",
                "0.667 copper-plate",
                "1.852 crude-oil",
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
            .map(|(x, e, y)| format!("{} -> {} -> {}", x, e, y))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
                "0.185 basic-oil-processing -> 3.704 co -> 1.852 crude-oil",
                "0.167 copper-cable -> 0.167 cp -> 0.667 copper-plate",
                "0.250 copper-cable -> 0.250 cp -> 0.667 copper-plate",
                "0.667 copper-plate -> 0.417 co -> 0.833 copper-ore",
                "0.167 electronic-circuit -> 0.500 cc -> 0.250 copper-cable",
                "0.167 electronic-circuit -> 0.167 ip -> 0.267 iron-plate",
                "0.267 iron-plate -> 0.167 io -> 0.333 iron-ore",
                "0.083 plastic-bar -> 1.667 pg -> 0.185 basic-oil-processing",
                "0.083 plastic-bar -> 0.083 c -> 0.167 coal",
            ]
        );
    }

    fn get_recipes() -> HashSet<String> {
        [
            "iron-plate",
            "copper-cable",
            "plastic-bar",
            "iron-ore",
            "copper-plate",
            "copper-ore",
            "basic-oil-processing",
            "crude-oil",
            "coal",
        ]
        .into_iter()
        .map(ToOwned::to_owned)
        .collect::<HashSet<_>>()
    }
}
