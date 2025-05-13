#![feature(let_chains)]
use module::{Module, ModuleBuilder, RecipeRepository};
use rust_decimal::Decimal;
use serde_derive::Deserialize;
use std::{collections::HashSet, fmt::Display};

pub mod compute;
pub mod data;
pub mod module;

pub fn generate(
    name: &str,
    expand: bool,
    imports: &[String],
    recipes: &HashSet<String>,
    recipe_rates: &RecipeRepository,
) -> Result<Module, Box<dyn std::error::Error>> {
    let mut module = ModuleBuilder::new(name.to_owned(), recipe_rates, imports, recipes);
    module.add(name, expand);
    get_recipe(recipe_rates, name)?;
    module.build()
}

pub fn get_recipe<'a>(recipes: &'a RecipeRepository, name: &str) -> Result<&'a RecipeRate, String> {
    if let Some(recipe) = recipes.get(name) {
        Ok(recipe)
    } else {
        let mut found = false;
        for k in recipes.recipes.keys() {
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

pub fn round_string(d: Decimal) -> String {
    d.round_dp(3).to_string()
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
            round_string(self.results[0].rate),
            self.key,
            self.ingredients
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", ")
        )
    }
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
    // Added category
    Mining,
}

#[derive(Clone)]
pub struct IngredientRate {
    pub rate: Decimal,
    pub name: String,
}

impl Display for IngredientRate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", round_string(self.rate), self.name)
    }
}
