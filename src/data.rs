use std::{collections::HashMap, str::FromStr};

use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;

use crate::{module::RecipeRepository, Category, IngredientRate, RecipeRate};

#[derive(Debug, Deserialize)]
pub struct Data<'a> {
    pub fluid: HashMap<&'a str, Value>,
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

pub fn calculate_rates(data: &Data, asm: i64) -> RecipeRepository {
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
            // Ignore recycling for now
            if matches!(
                category,
                Some(Category::Recycling | Category::RecyclingOrHandCrafting)
            ) {
                return None;
            }
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
    let mut recipe_outputs: HashMap<String, Vec<String>> = HashMap::new();
    for (key, recipe) in &recipe_rates {
        for result in &recipe.results {
            recipe_outputs
                .entry(result.name.clone())
                .or_default()
                .push(key.clone());
        }
    }
    RecipeRepository {
        recipes: recipe_rates,
        recipe_outputs,
    }
}
