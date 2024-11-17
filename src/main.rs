use clap::Parser;
use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec, parse,
    printer::PrinterContext,
};
use petgraph::{dot::Dot, graph::NodeIndex, visit::Dfs, Graph};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, fmt::Display, process::Command};

#[derive(Parser, Debug)]
#[command()]
struct Args {
    name: String,
    #[arg(short, long)]
    rate: Option<Decimal>,
    #[arg(long)]
    debug: bool,
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
                .find(|r| r["key"] == args.name)
                .ok_or(format!("{} was not found", args.name))?
        );
        return Ok(());
    }
    let data: Data = serde_json::from_slice(b)?;
    let recipes: HashMap<&str, &Recipe> = data.recipes.iter().map(|r| (r.key, r)).collect();
    let Some(recipe) = recipes.get(args.name.as_str()) else {
        for k in recipes.keys() {
            if k.contains(&args.name) {
                println!("{}", k);
            }
        }
        return Ok(());
    };
    let recipe_rates: HashMap<&str, RecipeRate> =
        data.recipes.iter().map(|r| (r.key, r.to_rate())).collect();
    fn build_node(
        required: Decimal,
        key: &str,
        recipe_rates: &HashMap<&str, RecipeRate>,
        graph: &mut Graph<Node, String>,
    ) -> NodeIndex {
        let node = graph.add_node(Node {
            required,
            name: key.to_owned(),
        });
        if let Some(recipe) = recipe_rates.get(key) {
            let ratio = required / recipe.results[0].rate;
            for i in &recipe.ingredients {
                let n = build_node(ratio * i.rate, &i.name, recipe_rates, graph);
                graph.add_edge(node, n, String::new());
            }
        }
        node
    }
    let mut graph = Graph::new();
    let root = build_node(
        args.rate.unwrap_or(Decimal::ONE / recipe.energy_required),
        recipe.key,
        &recipe_rates,
        &mut graph,
    );
    let dot = Dot::new(&graph);
    let g = parse(&dot.to_string())?;
    exec(
        g,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("out.svg".to_string()),
        ],
    )?;
    Command::new("open").arg("out.svg").spawn()?;
    let mut dfs = Dfs::new(&graph, root);
    let mut total = HashMap::new();
    while let Some(nx) = dfs.next(&graph) {
        if let Some(recipe) = recipe_rates.get(graph[nx].name.as_str()) {
            let ratio = graph[nx].required / recipe.results[0].rate;
            for i in &recipe.ingredients {
                *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
            }
        }
    }
    for (key, required) in total {
        println!(
            "{} {key}/s{}",
            round_string(&required),
            if let Some(recipe) = recipe_rates.get(key) {
                format!(" ({})", round_string(&(required / recipe.results[0].rate)))
            } else {
                String::new()
            }
        );
    }
    Ok(())
}

#[derive(Debug, Deserialize)]
struct Data<'a> {
    #[serde(borrow)]
    recipes: Vec<Recipe<'a>>,
}

#[derive(Debug, Deserialize)]
struct Recipe<'a> {
    #[serde(borrow)]
    key: &'a str,
    energy_required: Decimal,
    ingredients: Vec<Ingredient>,
    results: Vec<RecipeResult>,
}

impl<'a> Recipe<'a> {
    fn to_rate(&self) -> RecipeRate<'a> {
        RecipeRate {
            key: self.key,
            ingredients: self
                .ingredients
                .iter()
                .cloned()
                .map(|i| IngredientRate {
                    rate: i.amount / self.energy_required,
                    name: i.name,
                })
                .collect(),
            results: self
                .results
                .iter()
                .cloned()
                .map(|i| IngredientRate {
                    rate: i.amount / self.energy_required,
                    name: i.name,
                })
                .collect(),
        }
    }
}

impl Display for IngredientRate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", round_string(&self.rate), self.name)
    }
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

struct RecipeRate<'a> {
    key: &'a str,
    ingredients: Vec<IngredientRate>,
    results: Vec<IngredientRate>,
}

impl Display for RecipeRate<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "recipe - {} {}: {} / s",
            round_string(&self.results[0].rate),
            self.key,
            self.ingredients
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

struct IngredientRate {
    rate: Decimal,
    name: String,
}

fn round_string(d: &Decimal) -> String {
    d.round_dp(3).to_string()
}

struct Node {
    required: Decimal,
    name: String,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", round_string(&self.required), self.name)
    }
}
