use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter,
    str::FromStr,
};

use rust_decimal::{prelude::FromPrimitive, Decimal};
use serde_derive::Deserialize;
use serde_json::Value;

use crate::module::{Module, Structure};

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

#[derive(Clone, Copy, Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Category {
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
    Parameters,
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
    let recipe_rates: HashMap<_, _> = data
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
                | Some(Category::AdvancedCrafting)
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
            let ingredients: Vec<_> = ingredients
                .iter()
                .cloned()
                .filter_map(|i| {
                    let i: Ingredient = serde_json::from_value(i).ok()?;
                    Some(IngredientRate {
                        rate: i.amount / energy_required * speed,
                        name: i.name,
                    })
                })
                .collect();
            let rate = RecipeRate {
                category,
                key: (*key).to_owned(),
                results: results
                    .iter()
                    .cloned()
                    .filter_map(|i| {
                        let i: RecipeResult = serde_json::from_value(i).ok()?;
                        // Ignore catalysts for now
                        if ingredients.iter().any(|r| r.name == i.name) {
                            None
                        } else {
                            Some(IngredientRate {
                                rate: i.amount / energy_required * speed,
                                name: i.name,
                            })
                        }
                    })
                    .collect(),
                ingredients,
            };
            Some(((*key).to_owned(), rate))
        })
        .collect();
    let mut resources = HashMap::new();
    // Add mining recipes
    for (key, resource) in &data.resource {
        let rate = RecipeRate {
            category: None,
            key: (*key).to_owned(),
            ingredients: Vec::new(),
            results: vec![IngredientRate {
                rate: resource.minable.mining_time
                    * match *key {
                        "crude-oil" => Decimal::TWO,
                        _ => Decimal::from_str("0.5").unwrap(),
                    },
                name: (*key).to_owned(),
            }],
        };
        resources.insert((*key).to_owned(), rate);
    }
    // Add water pumping
    resources.insert(
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
    for (key, recipe) in &resources {
        for result in &recipe.results {
            recipe_outputs
                .entry(result.name.clone())
                .or_default()
                .push(key.clone());
        }
    }
    let science_rate = Decimal::from_f64((1. + 2.5) / 60.).unwrap();
    RecipeRepository {
        recipes: recipe_rates,
        resources,
        recipe_outputs,
        science: "science".to_owned(),
        science_recipe: RecipeRate {
            category: None,
            key: "science".to_owned(),
            ingredients: [
                "automation-science-pack",
                "logistic-science-pack",
                "military-science-pack",
                "chemical-science-pack",
                "production-science-pack",
                "utility-science-pack",
            ]
            .into_iter()
            .map(|i| IngredientRate {
                rate: science_rate,
                name: i.to_owned(),
            })
            .collect(),
            results: vec![IngredientRate {
                rate: science_rate,
                name: "science".to_owned(),
            }],
        },
    }
}

pub enum RepositoryOption<'a, T> {
    None,
    Some(T),
    Multiple(&'a Vec<String>),
}

pub struct RecipeRepository {
    pub recipes: HashMap<String, RecipeRate>,
    pub resources: HashMap<String, RecipeRate>,
    pub recipe_outputs: HashMap<String, Vec<String>>,
    science: String,
    pub science_recipe: RecipeRate,
}

impl RecipeRepository {
    pub fn recipes(&self) -> impl Iterator<Item = (&String, &RecipeRate)> {
        self.recipes
            .iter()
            .chain(iter::once((&self.science, &self.science_recipe)))
    }

    pub fn get(&self, key: &str) -> Option<Rate> {
        if key == "science" {
            Some(Rate::Recipe(&self.science_recipe))
        } else if let Some(recipe) = self.recipes.get(key) {
            Some(Rate::Recipe(recipe))
        } else {
            self.resources.get(key).map(Rate::Resource)
        }
    }

    pub fn get_options(&self, key: &str) -> RepositoryOption<'_, Rate> {
        match self.recipe_outputs.get(key) {
            None => RepositoryOption::None,
            Some(recipes) if recipes.len() > 1 => RepositoryOption::Multiple(recipes),
            Some(_) => RepositoryOption::Some(self.get(key).unwrap()),
        }
    }

    pub fn get_inputs(&self, module: &Module) -> HashSet<&str> {
        let Module::User { structures } = module else {
            return HashSet::new();
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe(recipe) => {
                    inputs.extend(
                        self.recipes[&recipe.name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(
                        self.recipes[&recipe.name]
                            .results
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                }
                Structure::Resource(resource) => {
                    outputs.insert(self.resources[&resource.name].key.as_str());
                }
            }
        }
        inputs.difference(&outputs).copied().collect()
    }

    pub fn get_resource_inputs(&self, module: &Module) -> HashSet<&str> {
        let structures = match module {
            Module::User { structures } => structures,
            Module::AdvancedOilProcessing {} => return HashSet::from(["water", "crude-oil"]),
            Module::Science {} => {
                return HashSet::from([
                    "automation-science-pack",
                    "logistic-science-pack",
                    "military-science-pack",
                    "chemical-science-pack",
                    "production-science-pack",
                    "utility-science-pack",
                    "space-science-pack",
                ])
            }
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe(recipe) => {
                    inputs.extend(
                        self.recipes[&recipe.name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(
                        self.recipes[&recipe.name]
                            .results
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                }
                Structure::Resource(resource) => {
                    inputs.insert(self.resources[&resource.name].key.as_str());
                }
            }
        }
        inputs.difference(&outputs).copied().collect()
    }

    pub fn get_outputs(&self, module: &Module) -> HashSet<&str> {
        let structures = match module {
            Module::User { structures } => structures,
            Module::AdvancedOilProcessing {} => {
                return HashSet::from(["heavy-oil", "light-oil", "petroleum-gas"])
            }
            Module::Science {} => return HashSet::from(["science"]),
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe(recipe) => {
                    inputs.extend(
                        self.recipes[&recipe.name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(
                        self.recipes[&recipe.name]
                            .results
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                }
                Structure::Resource(resource) => {
                    outputs.insert(self.resources[&resource.name].key.as_str());
                }
            }
        }
        outputs.difference(&inputs).copied().collect()
    }
}

#[derive(Clone)]
pub enum Rate<'a> {
    Resource(&'a RecipeRate),
    Recipe(&'a RecipeRate),
}

impl<'a> Rate<'a> {
    pub fn rate(&self) -> &'a RecipeRate {
        match self {
            Rate::Resource(rate) => rate,
            Rate::Recipe(rate) => rate,
        }
    }
}

#[derive(Clone)]
pub struct RecipeRate {
    pub category: Option<Category>,
    pub key: String,
    pub ingredients: Vec<IngredientRate>,
    pub results: Vec<IngredientRate>,
}

impl Display for RecipeRate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "recipe - {} {}: {} / s",
            crate::round_string(self.results[0].rate),
            self.key,
            self.ingredients
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
}

#[derive(Clone)]
pub struct IngredientRate {
    pub rate: Decimal,
    pub name: String,
}

impl Display for IngredientRate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", crate::round_string(self.rate), self.name)
    }
}
