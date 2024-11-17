use clap::Parser;
use facalculo::{compute, Graph, IngredientRate, RecipeRate};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::collections::HashMap;

#[derive(Parser, Debug)]
#[command()]
struct Args {
    name: String,
    #[arg(short, long)]
    rate: Option<Decimal>,
    #[arg(long)]
    render: bool,
    #[arg(long)]
    total: bool,
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
    let recipe_rates: HashMap<_, _> = data.recipes.iter().map(|r| (r.key, r.to_rate())).collect();
    let graph = Graph::new(
        args.rate.unwrap_or(Decimal::ONE / recipe.energy_required),
        args.name.as_str(),
        &recipe_rates,
    );
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
