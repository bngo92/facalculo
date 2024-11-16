use clap::Parser;
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::{collections::HashMap, fmt::Display};

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
        recipe: Option<&RecipeRate>,
        recipe_rates: &HashMap<&str, RecipeRate>,
    ) -> Node {
        if let Some(recipe) = recipe {
            let ratio = required / recipe.results[0].rate;
            Node {
                required,
                name: key.to_owned(),
                children: recipe
                    .ingredients
                    .iter()
                    .map(|i| {
                        build_node(
                            ratio * i.rate,
                            &i.name,
                            recipe_rates.get(&i.name.as_str()),
                            recipe_rates,
                        )
                    })
                    .collect(),
            }
        } else {
            Node {
                required,
                name: key.to_owned(),
                children: Vec::new(),
            }
        }
    }
    let graph = build_node(
        args.rate.unwrap_or(Decimal::ONE / recipe.energy_required),
        recipe.key,
        recipe_rates.get(recipe.key),
        &recipe_rates,
    );
    fn dfs<F>(node: &Node, f: &mut F)
    where
        F: FnMut(usize, &Node),
    {
        fn dfs_inner<F>(depth: usize, node: &Node, f: &mut F)
        where
            F: FnMut(usize, &Node),
        {
            f(depth, node);
            for child in &node.children {
                dfs_inner(depth + 1, child, f);
            }
        }
        dfs_inner(0, node, f);
    }
    let mut total = HashMap::new();
    dfs(&graph, &mut |depth, node| {
        if let Some(recipe) = recipe_rates.get(node.name.as_str()) {
            let ratio = node.required / recipe.results[0].rate;
            let mut ingredients = Vec::new();
            for i in &recipe.ingredients {
                ingredients.push(format!("{} {}", round_string(&(ratio * i.rate)), i.name));
                *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
            }
            println!(
                "{}{} {}: {} / s",
                " ".repeat(2 * depth),
                round_string(&node.required),
                recipe.key,
                ingredients.join(", ")
            );
            println!(
                "{}{recipe} ({})",
                " ".repeat(2 * depth + 2),
                round_string(&ratio)
            );
        } else {
            println!(
                "{}{} {}/s",
                " ".repeat(2 * depth),
                round_string(&node.required),
                node.name
            );
        }
    });
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
    children: Vec<Node>,
}
