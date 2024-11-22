use clap::Parser;
use facalculo::{compute, Graph, IngredientRate, RecipeRate};
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
    group: bool,
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
    let mut graph = Graph::new(&recipe_rates);
    for item in args.items {
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

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
enum Category {
    AdvancedCrafting,
    CaptiveSpawnerProcess,
    Centrifuging,
    Chemistry,
    ChemistryOrCryogenics,
    Crafting,
    CraftingWithFluid,
    CraftingWithFluidOrMetallurgy,
    Crushing,
    Cryogenics,
    CryogenicsOrAssembling,
    CryogenicsOrChemistry,
    Electromagnetics,
    Electronics,
    ElectronicsOrAssembling,
    ElectronicsWithFluid,
    Metallurgy,
    MetallurgyOrAssembling,
    OilProcessing,
    Organic,
    OrganicOrAssembling,
    OrganicOrChemistry,
    OrganicOrHandCrafting,
    Pressing,
    RocketBuilding,
    Recycling,
    RecyclingOrHandCrafting,
    Smelting,
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
                Category::Crafting | Category::CraftingWithFluid | Category::Electronics => {
                    match asm {
                        1 => Decimal::from_str("0.5").unwrap(),
                        2 => Decimal::from_str("0.75").unwrap(),
                        3 => Decimal::from_str("1.25").unwrap(),
                        _ => unimplemented!(),
                    }
                }
                Category::Smelting => Decimal::new(2, 0),
                _ => Decimal::ONE,
            };
            let rate = RecipeRate {
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
    for resource in &data.resources {
        let rate = RecipeRate {
            key: resource.key,
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: resource.mining_time * Decimal::from_str("0.5").unwrap(),
                name: resource.key.to_owned(),
            }],
        };
        recipe_rates.insert(resource.key, rate);
    }
    recipe_rates
}
