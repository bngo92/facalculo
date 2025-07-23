use core::fmt::{self, Display};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use serde_json::Value;
use std::{
    borrow::Cow,
    cmp::Ordering,
    collections::{HashMap, HashSet},
    str::FromStr,
};

use crate::module::{self, OilProcessing, Structure};

#[derive(Debug, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub struct Data<'a> {
    recipe: HashMap<&'a str, Recipe>,
    furnace: HashMap<String, AssemblingMachine>,
    assembling_machine: HashMap<String, AssemblingMachine>,
    lab: HashMap<String, AssemblingMachine>,
    rocket_silo: HashMap<String, AssemblingMachine>,
    mining_drill: HashMap<String, MiningDrill>,
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
    probability: Option<Decimal>,
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

impl From<(&RecipeRepository, &HashMap<String, i32>)> for Effect {
    fn from((recipes, modules): (&RecipeRepository, &HashMap<String, i32>)) -> Self {
        Effect {
            productivity: modules
                .iter()
                .map(|(m, c)| recipes.modules[m].effect.productivity * Decimal::from(*c))
                .sum(),
            speed: modules
                .iter()
                .map(|(m, c)| recipes.modules[m].effect.speed * Decimal::from(*c))
                .sum(),
            consumption: modules
                .iter()
                .map(|(m, c)| recipes.modules[m].effect.consumption * Decimal::from(*c))
                .sum(),
            pollution: Decimal::ZERO,
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct AssemblingMachine {
    pub name: String,
    pub energy_usage: String,
    pub crafting_speed: Option<Decimal>,
    pub effect_receiver: Option<EffectReceiver>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct EffectReceiver {
    #[serde(default)]
    pub base_effect: HashMap<String, Decimal>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct MiningDrill {
    pub name: String,
    pub mining_speed: Decimal,
    pub energy_usage: String,
    pub resource_drain_rate_percent: Option<Decimal>,
}

impl AssemblingMachine {
    pub fn energy(&self) -> Decimal {
        if self.name == "offshore-pump" {
            return Decimal::ZERO;
        }
        let (n, unit) = self
            .energy_usage
            .chars()
            .partition::<Vec<_>, _>(char::is_ascii_digit);
        let mut n = Decimal::from_str(&n.into_iter().collect::<String>()).unwrap()
            * match unit.into_iter().collect::<String>().as_str() {
                "kW" => Decimal::ONE_THOUSAND,
                "mW" => Decimal::from(1_000_000),
                _ => unimplemented!(),
            };
        // https://forums.factorio.com/viewtopic.php?t=109602
        if self.name == "assembling-machine-3" {
            n *= Decimal::from(31) / Decimal::from(30);
        }
        n
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
    pub assembling_machines: HashMap<String, AssemblingMachine>,
    pub mining_drills: HashMap<String, MiningDrill>,
}

impl RecipeRepository {
    pub fn recipes(&self) -> impl Iterator<Item = (&String, &RecipeRate)> {
        self.recipes.iter()
    }

    pub fn get(&self, key: &str) -> Option<Rate<&RecipeRate>> {
        if let Some(recipe) = self.recipes.get(key) {
            Some(Rate::Recipe(recipe))
        } else {
            self.resources.get(key).map(Rate::Resource)
        }
    }

    pub fn get_options(&self, key: &str) -> RepositoryOption<'_, Rate<&RecipeRate>> {
        match self.recipe_outputs.get(key) {
            None => RepositoryOption::None,
            Some(recipes) if recipes.len() > 1 => RepositoryOption::Multiple(recipes),
            Some(_) => RepositoryOption::Some(self.get(key).unwrap()),
        }
    }

    pub fn get_inputs(&self, module: &module::Module) -> HashSet<&str> {
        let structures = match module {
            module::Module::User { structures } => structures,
            module::Module::OilProcessing {
                process: OilProcessing::AdvancedOilProcessing,
            } => {
                return HashSet::from(["water", "crude-oil"]);
            }
            module::Module::OilProcessing {
                process: OilProcessing::CoalLiquefaction,
            } => {
                return HashSet::from(["coal", "steam", "water"]);
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
                ]);
            }
            module::Module::RocketSilo { .. } => {
                return HashSet::from(["low-density-structure", "processing-unit", "rocket-fuel"]);
            }
        };
        let mut inputs = HashSet::new();
        let mut outputs = HashSet::new();
        let mut resources = HashSet::new();
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
                    resources.insert(self.resources[name].key.as_str());
                }
            }
        }
        inputs
            // Ignore inputs that are produced by another recipe
            .difference(&outputs)
            .copied()
            .collect::<HashSet<_>>()
            // Ignore inputs that are produced by a resource structure
            .difference(&resources)
            .copied()
            .collect()
    }

    pub fn get_outputs(&self, module: &module::Module) -> HashSet<&str> {
        let structures = match module {
            module::Module::User { structures } => structures,
            module::Module::OilProcessing { .. } => {
                return HashSet::from(["heavy-oil", "light-oil", "petroleum-gas"]);
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

#[derive(Clone, Copy)]
pub enum Rate<T> {
    Resource(T),
    Recipe(T),
}

impl<T> Rate<T> {
    pub const fn rate(&self) -> &T {
        match self {
            Self::Resource(rate) | Self::Recipe(rate) => rate,
        }
    }
}

impl<'a, T: Clone> Rate<&'a T> {
    pub const fn to_cow(self) -> Rate<Cow<'a, T>> {
        match self {
            Rate::Resource(rate) => Rate::Resource(Cow::Borrowed(rate)),
            Rate::Recipe(rate) => Rate::Recipe(Cow::Borrowed(rate)),
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
            "copper-ore" | "iron-ore" | "coal" | "stone" | "calcite" => {
                Some("electric-mining-drill")
            }
            "water" | "lava" => Some("offshore-pump"),
            "crude-oil" | "sulfuric-acid" => Some("pumpjack"),
            "science" => Some("lab"),
            "rocket" => Some(""),
            _ => None,
        } {
            structure
        } else {
            match self.category {
                None
                | Some(
                    Category::AdvancedCrafting
                    | Category::Crafting
                    | Category::CraftingWithFluid
                    | Category::Electronics
                    | Category::ElectronicsWithFluid
                    | Category::OrganicOrAssembling
                    | Category::Pressing,
                ) => asm,
                Some(
                    Category::Chemistry
                    | Category::ChemistryOrCryogenics
                    | Category::OrganicOrChemistry,
                ) => "chemical-plant",
                Some(Category::Crushing) => "crusher",
                Some(Category::Metallurgy) => "foundry",
                Some(Category::OilProcessing) => "oil-refinery",
                Some(Category::RocketBuilding) => "rocket-silo",
                Some(Category::Smelting) => "electric-furnace",
                _ => todo!("{}", self.key),
            }
        }
    }
}

impl Display for RecipeRate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", crate::round_string(self.rate), self.name)
    }
}

pub fn calculate_rates(data: &Data) -> RecipeRepository {
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
            let mut ingredients: HashMap<_, _> = ingredients
                .iter()
                .cloned()
                .filter_map(|i| {
                    let i: Ingredient = serde_json::from_value(i).ok()?;
                    Some((i.name, i.amount))
                })
                .collect();
            let mut results: HashMap<_, _> = results
                .iter()
                .cloned()
                .filter_map(|i| {
                    let i: RecipeResult = serde_json::from_value(i).ok()?;
                    Some((i.name, i.probability.unwrap_or(Decimal::ONE) * i.amount))
                })
                .collect();
            let mut ingredients_to_delete = Vec::new();
            let mut results_to_delete = Vec::new();
            for (name, ingredient) in &mut ingredients {
                if let Some(result) = results.get_mut(name) {
                    match ingredient.cmp(&result) {
                        Ordering::Less => {
                            ingredients_to_delete.push(name.clone());
                            *result -= *ingredient;
                        }
                        Ordering::Greater => {
                            results_to_delete.push(name);
                            *ingredient -= *result;
                        }
                        Ordering::Equal => unimplemented!(),
                    }
                }
            }
            let rate = RecipeRate {
                category,
                key: (*key).to_owned(),
                results: results
                    .into_iter()
                    .filter_map(|(name, amount)| {
                        if results_to_delete.contains(&&name) {
                            None
                        } else {
                            Some(IngredientRate {
                                rate: amount / energy_required,
                                name,
                            })
                        }
                    })
                    .collect(),
                ingredients: ingredients
                    .into_iter()
                    .filter_map(|(name, amount)| {
                        if ingredients_to_delete.contains(&name) {
                            None
                        } else {
                            Some(IngredientRate {
                                rate: amount / energy_required,
                                name,
                            })
                        }
                    })
                    .collect(),
            };
            Some(((*key).to_owned(), rate))
        })
        .collect();
    let mut resources = HashMap::new();
    // Add mining recipes
    for (key, resource) in &data.resource {
        let key = if *key == "sulfuric-acid-geyser" {
            "sulfuric-acid"
        } else {
            key
        };
        let results = vec![IngredientRate {
            rate: match key {
                // Pumpjacks have a minimum of 2 fluid per second
                "crude-oil" | "sulfuric-acid" => Decimal::TWO,
                _ => resource.minable.mining_time,
            },
            name: key.to_owned(),
        }];
        let rate = RecipeRate {
            category: None,
            key: key.to_owned(),
            ingredients: results.clone(),
            results,
        };
        resources.insert(key.to_owned(), rate);
    }
    // Add fluids like water pumping
    for fluid in ["water", "lava"] {
        let results = vec![IngredientRate {
            rate: Decimal::ONE,
            name: String::from(fluid),
        }];
        resources.insert(
            fluid.to_owned(),
            RecipeRate {
                category: None,
                key: fluid.to_owned(),
                ingredients: results.clone(),
                results,
            },
        );
    }
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
    let mut mining_drills = data.mining_drill.clone();
    mining_drills.insert(
        "offshore-pump".to_owned(),
        MiningDrill {
            name: "offshore-pump".to_owned(),
            mining_speed: Decimal::from(1200),
            energy_usage: "0kW".to_owned(),
            resource_drain_rate_percent: None,
        },
    );
    let mut assembling_machines = data.assembling_machine.clone();
    assembling_machines.extend(data.lab.clone());
    assembling_machines.extend(data.rocket_silo.clone());
    assembling_machines.extend(data.furnace.clone());
    assembling_machines.extend(mining_drills.iter().map(|(k, m)| {
        (
            k.clone(),
            AssemblingMachine {
                name: m.name.clone(),
                energy_usage: m.energy_usage.clone(),
                crafting_speed: Some(m.mining_speed),
                effect_receiver: None,
            },
        )
    }));
    RecipeRepository {
        recipes: recipe_rates,
        resources,
        recipe_outputs,
        modules: data
            .module
            .iter()
            .map(|(&name, module)| (name.to_owned(), module.clone()))
            .collect(),
        assembling_machines,
        mining_drills,
    }
}
