use clap::Parser;
use facalculo::{compute, Graph, IngredientRate, RecipeRate};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Parser, Debug)]
#[command()]
struct Args {
    #[arg(short, long, num_args = 1..=2)]
    items: Vec<Vec<String>>,
    #[arg(long)]
    group: bool,
    #[arg(long)]
    render: bool,
    #[arg(long)]
    total: bool,
    #[arg(long)]
    out: bool,
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
                .find(|r| r["key"] == args.items[0][0])
                .ok_or(format!("{} was not found", args.items[0][0]))?
        );
        return Ok(());
    }
    let data: Data = serde_json::from_slice(b)?;
    let recipes: HashMap<&str, &Recipe> = data.recipes.iter().map(|r| (r.key, r)).collect();
    let recipe_rates: HashMap<_, _> = data.recipes.iter().map(|r| (r.key, r.to_rate())).collect();
    let mut graph = Graph::new(&recipe_rates);
    for item in args.items {
        let mut iter = item.iter();
        let name = iter.next().unwrap();
        let recipe = get_recipe(&recipes, name)?;
        let required = if let Some(rate) = iter.next() {
            rate.parse()?
        } else {
            let rate = recipe.results[0].amount / recipe.energy_required;
            eprintln!(
                "Using {} for {name} (1 assembler)",
                facalculo::round_string(&rate)
            );
            rate
        };
        graph.add(required, name);
    }
    if args.group {
        graph = graph.group_nodes();
    }
    if args.render {
        compute::render(&graph)?;
    }
    if args.total {
        for (key, required) in compute::total(&graph) {
            println!(
                "{} {key}/s{}",
                facalculo::round_string(&required),
                if let Some(recipe) = recipe_rates.get(key) {
                    format!(
                        " ({})",
                        facalculo::round_string(&(required / recipe.results[0].rate))
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
    Ok(())
}

fn get_recipe<'a>(
    recipes: &HashMap<&str, &'a Recipe<'a>>,
    name: &str,
) -> Result<&'a Recipe<'a>, String> {
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
