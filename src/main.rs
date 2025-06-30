use clap::{Parser, Subcommand, ValueEnum};
use facalculo::{
    compute::{self, RenderArgs},
    data::{self, Data},
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
        details: bool,
        #[arg(long)]
        production: Option<String>,
        #[arg(long)]
        energy: bool,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    let b = include_bytes!("data-raw-dump.json");
    let data: Data = serde_json::from_slice(b)?;
    let recipe_rates = data::calculate_rates(&data);
    let _belt = args.belt.map(|b| b as i64);
    match args.command {
        None => (),
        Some(Commands::Generate {
            item: items,
            expand,
            import,
            recipe,
        }) => {
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
            let graphs: Vec<_> = modules
                .iter()
                .cloned()
                .map(|m| Graph::from_module(m, &required, &recipe_rates, args.asm))
                .collect();
            let out = compute::render(
                &graphs,
                &RenderArgs::new(HashMap::new(), HashSet::new(), false),
                &mut [],
                &mut [],
            )?;
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
            details,
            production,
            energy,
        }) => {
            // Parse arguments
            let modules = files
                .into_iter()
                .map(|f| -> Result<_, Box<dyn std::error::Error>> {
                    let module: NamedModule = serde_json::from_slice(&fs::read(f)?)?;
                    Ok((module.name.clone(), module))
                })
                .collect::<Result<HashMap<_, _>, _>>()?;
            let mut rates = items
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
            let mut outputs = HashMap::new();
            let mut active_set = HashSet::new();
            for (node, module) in &modules {
                for output in recipe_rates.get_outputs(&module.module) {
                    if outputs.insert(output, node).is_some() {
                        return Err(format!("multiple modules are exporting {output}").into());
                    }
                    if rates.contains_key(output) {
                        active_set.insert(node.clone());
                    }
                }
            }
            let mut graph = GraphMap::<&str, (), Directed>::new();
            for (node, module) in &modules {
                graph.add_node(node);
                for input in recipe_rates.get_resource_inputs(&module.module) {
                    // Ignore resources
                    if node != input {
                        if let Some(export_node) = outputs.get(input) {
                            graph.add_edge(node, export_node, ());
                        }
                    }
                }
            }
            let module_order: Vec<_> = petgraph::algo::toposort(&graph, None)
                .unwrap()
                .into_iter()
                .map(ToOwned::to_owned)
                .collect();
            let mut graphs = Vec::new();
            let mut imports: HashMap<String, Vec<(String, usize, Decimal)>> = HashMap::new();
            let production = if let Some(production) = production {
                let (rate, unit) = production.split_at(production.len() - 1);
                Some(
                    rate.parse::<Decimal>()
                        .expect("production should be a timescale")
                        * match unit {
                            "s" => Decimal::ONE,
                            "m" => Decimal::from(60),
                            "h" => Decimal::from(3400),
                            _ => return Err(format!("{unit} is an invalid unit").into()),
                        },
                )
            } else {
                None
            };
            let mut total_production = HashMap::new();
            let mut structure_count = HashMap::new();
            let mut total_energy = HashMap::new();
            for module in module_order {
                if !active_set.contains(&module) {
                    continue;
                }
                active_set.extend(graph.neighbors(&module).map(ToOwned::to_owned));
                let graph =
                    Graph::from_module(modules[&module].clone(), &rates, &recipe_rates, args.asm);
                for (import, node) in &graph.imports {
                    // We do not create import nodes for science packs
                    let (import_required, node) = match node {
                        Import::Resource(..) => continue,
                        Import::Node(node) => (graph.graph[*node].required.unwrap(), node),
                        Import::Import(node, required) => (*required, node),
                    };
                    *rates.entry(import.clone()).or_default() += import_required;
                    imports.entry(import.clone()).or_default().push((
                        graph.name.clone(),
                        node.index(),
                        import_required,
                    ));
                }
                if let Some(production) = production {
                    for (item, rate) in &graph.production {
                        *total_production.entry(item.clone()).or_default() += production * rate;
                    }
                }
                if energy {
                    for (structure, count) in &graph.structures {
                        *structure_count.entry(structure.clone()).or_default() += count;
                    }
                    for (structure, energy) in &graph.energy {
                        *total_energy.entry(structure.clone()).or_default() += energy;
                    }
                }
                graphs.push(graph);
            }
            let out = compute::render(
                &graphs,
                &RenderArgs::new(
                    imports,
                    outputs.into_keys().map(ToOwned::to_owned).collect(),
                    details,
                ),
                &mut total_production.into_iter().collect::<Vec<_>>(),
                &mut structure_count
                    .into_iter()
                    .map(|t| {
                        let energy = total_energy[&t.0];
                        (t.0, t.1, energy)
                    })
                    .collect::<Vec<_>>(),
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
                    CommandArg::Output(format!("out.{format:?}").to_lowercase()),
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
    use rust_decimal::prelude::FromPrimitive;

    #[test]
    fn advanced_circuit() {
        let b = include_bytes!("data-raw-dump.json");
        let data: Data = serde_json::from_slice(b).unwrap();
        let recipe_rates = data::calculate_rates(&data);
        let recipes = get_recipes();
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[], &recipes);
        builder.add("advanced-circuit", true);
        let graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().rate().results[0].rate
                    * Decimal::from_f64(0.5).unwrap(),
            )]),
            &recipe_rates,
            1,
        );
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1.000 advanced-circuit",
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
            .map(|(x, e, y)| format!("{x} -> {e} -> {y}"))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1.000 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1.000 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1.000 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
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
        let recipe_rates = data::calculate_rates(&data);
        let recipes = get_recipes();
        let mut builder = ModuleBuilder::new(String::new(), &recipe_rates, &[], &recipes);
        builder.add("advanced-circuit", true);
        let mut graph = Graph::from_module(
            builder.build(),
            &HashMap::from_iter([(
                "advanced-circuit".to_owned(),
                recipe_rates.get("advanced-circuit").unwrap().rate().results[0].rate
                    * Decimal::from_f64(0.5).unwrap(),
            )]),
            &recipe_rates,
            1,
        );
        graph.group_nodes(Vec::new());
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1.000 advanced-circuit",
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
            .map(|(x, e, y)| format!("{x} -> {e} -> {y}"))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1.000 advanced-circuit -> 0.333 cc -> 0.417 copper-cable",
                "1.000 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1.000 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
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
        let recipe_rates = data::calculate_rates(&data);
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
                    .rate
                    * Decimal::from_f64(0.5).unwrap(),
            )]),
            &recipe_rates,
            1,
        );
        graph.group_nodes(vec![String::from("copper-plate")]);
        let graph = graph.graph;
        let mut nodes: Vec<_> = graph.node_weights().collect();
        nodes.sort_by_key(|n| (&n.name, n.required));
        let nodes: Vec<_> = nodes.into_iter().map(ToString::to_string).collect();
        assert_eq!(
            nodes,
            vec![
                "1.000 advanced-circuit",
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
            .map(|(x, e, y)| format!("{x} -> {e} -> {y}"))
            .collect();
        assert_eq!(
            edges,
            vec![
                "1.000 advanced-circuit -> 0.333 cc -> 0.167 copper-cable",
                "1.000 advanced-circuit -> 0.167 ec -> 0.167 electronic-circuit",
                "1.000 advanced-circuit -> 0.167 pb -> 0.083 plastic-bar",
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
