use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    iter,
    str::FromStr,
};

use rust_decimal::{prelude::FromPrimitive, Decimal};
use serde_derive::Deserialize;
use serde_json::Value;

use crate::module::{self, Structure};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Data<'a> {
    recipe: HashMap<&'a str, Recipe>,
    pub fluid: HashMap<&'a str, Value>,
    furnace: HashMap<String, AssemblingMachine>,
    offshore_pump: HashMap<String, AssemblingMachine>,
    assembling_machine: HashMap<String, AssemblingMachine>,
    lab: HashMap<String, AssemblingMachine>,
    rocket_silo: HashMap<String, AssemblingMachine>,
    mining_drill: HashMap<String, AssemblingMachine>,
    #[serde(borrow)]
    resource: HashMap<&'a str, Resource>,
    module: HashMap<&'a str, Module>,
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

#[derive(Clone, Debug, Deserialize)]
pub struct Module {
    pub effect: Effect,
}

#[derive(Clone, Copy, Debug, Default, Deserialize)]
pub struct Effect {
    #[serde(default)]
    pub productivity: Decimal,
    #[serde(default)]
    pub consumption: Decimal,
    #[serde(default)]
    pub pollution: Decimal,
    #[serde(default)]
    pub speed: Decimal,
}

#[derive(Clone, Debug, Deserialize)]
pub struct AssemblingMachine {
    name: String,
    energy_usage: String,
}

impl AssemblingMachine {
    pub fn energy(&self) -> Decimal {
        if self.name == "offshore-pump" {
            return Decimal::ZERO;
        }
        let (n, unit) = self
            .energy_usage
            .chars()
            .partition::<Vec<_>, _>(|c| c.is_ascii_digit());
        let mut n = Decimal::from_str(&n.into_iter().collect::<String>()).unwrap()
            * match unit.into_iter().collect::<String>().as_str() {
                "kW" => Decimal::ONE_THOUSAND,
                "mW" => Decimal::from(1000000),
                _ => unimplemented!(),
            };
        // https://forums.factorio.com/viewtopic.php?t=109602
        if self.name == "assembling-machine-3" {
            n *= Decimal::from(31) / Decimal::from(30);
        }
        n
    }
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
    let mut assembling_machines = data.assembling_machine.clone();
    assembling_machines.extend(data.lab.clone());
    assembling_machines.extend(data.rocket_silo.clone());
    assembling_machines.extend(data.furnace.clone());
    assembling_machines.extend(data.mining_drill.clone());
    assembling_machines.extend(data.offshore_pump.clone());
    RecipeRepository {
        recipes: recipe_rates,
        resources,
        recipe_outputs,
        modules: data
            .module
            .iter()
            .map(|(&name, module)| (name.to_owned(), module.clone()))
            .collect(),
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
                "space-science-pack",
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
        assembling_machines,
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
    pub modules: HashMap<String, Module>,
    science: String,
    pub science_recipe: RecipeRate,
    pub assembling_machines: HashMap<String, AssemblingMachine>,
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

    pub fn get_inputs(&self, module: &module::Module) -> HashSet<&str> {
        let module::Module::User { structures } = module else {
            return HashSet::new();
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe { name, .. } => {
                    inputs.extend(
                        self.recipes[name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(self.recipes[name].results.iter().map(|i| i.name.as_str()));
                }
                Structure::Resource { name, .. } => {
                    outputs.insert(self.resources[name].key.as_str());
                }
            }
        }
        inputs.difference(&outputs).copied().collect()
    }

    pub fn get_resource_inputs(&self, module: &module::Module) -> HashSet<&str> {
        let structures = match module {
            module::Module::User { structures } => structures,
            module::Module::AdvancedOilProcessing {} => {
                return HashSet::from(["water", "crude-oil"])
            }
            module::Module::Science { .. } => {
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
            module::Module::RocketSilo { .. } => {
                return HashSet::from(["low-density-structure", "processing-unit", "rocket-fuel"])
            }
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe { name, .. } => {
                    inputs.extend(
                        self.recipes[name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(self.recipes[name].results.iter().map(|i| i.name.as_str()));
                }
                Structure::Resource { name, .. } => {
                    inputs.insert(self.resources[name].key.as_str());
                }
            }
        }
        inputs.difference(&outputs).copied().collect()
    }

    pub fn get_outputs(&self, module: &module::Module) -> HashSet<&str> {
        let structures = match module {
            module::Module::User { structures } => structures,
            module::Module::AdvancedOilProcessing {} => {
                return HashSet::from(["heavy-oil", "light-oil", "petroleum-gas"])
            }
            module::Module::Science { .. } => return HashSet::from(["science"]),
            module::Module::RocketSilo { .. } => return HashSet::from(["rocket"]),
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        for structure in structures {
            match structure {
                Structure::Recipe { name, .. } => {
                    inputs.extend(
                        self.recipes[name]
                            .ingredients
                            .iter()
                            .map(|i| i.name.as_str()),
                    );
                    outputs.extend(self.recipes[name].results.iter().map(|i| i.name.as_str()));
                }
                Structure::Resource { name, .. } => {
                    outputs.insert(self.resources[name].key.as_str());
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

impl RecipeRate {
    pub fn structure(&self, asm: i64) -> &'static str {
        let asm = match asm {
            1 => "assembling-machine-1",
            2 => "assembling-machine-2",
            3 => "assembling-machine-3",
            _ => unreachable!(),
        };
        if let Some(structure) = match self.key.as_str() {
            "copper-ore" => Some("electric-mining-drill"),
            "iron-ore" => Some("electric-mining-drill"),
            "coal" => Some("electric-mining-drill"),
            "stone" => Some("electric-mining-drill"),
            "water" => Some("offshore-pump"),
            "crude-oil" => Some("pumpjack"),
            "science" => Some("lab"),
            "rocket" => Some(""),
            _ => None,
        } {
            structure
        } else {
            match self.category {
                None => asm,
                Some(Category::AdvancedCrafting) => asm,
                Some(Category::Chemistry) => "chemical-plant",
                Some(Category::ChemistryOrCryogenics) => "chemical-plant",
                Some(Category::Crafting) => asm,
                Some(Category::CraftingWithFluid) => asm,
                Some(Category::Crushing) => "crusher",
                Some(Category::Electronics) => asm,
                Some(Category::ElectronicsWithFluid) => asm,
                Some(Category::OilProcessing) => "oil-refinery",
                Some(Category::OrganicOrAssembling) => asm,
                Some(Category::OrganicOrChemistry) => "chemical-plant",
                Some(Category::Pressing) => asm,
                Some(Category::RocketBuilding) => "rocket-silo",
                Some(Category::Smelting) => "electric-furnace",
                _ => todo!("{}", self.key),
            }
        }
    }
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
