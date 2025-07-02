use data::{Rate, RecipeRate, RecipeRepository};
use module::{ModuleBuilder, NamedModule};
use rust_decimal::Decimal;
use std::collections::HashSet;

pub mod compute;
pub mod data;
pub mod graph;
pub mod module;
pub mod module_graph;

pub fn generate(
    name: &str,
    expand: bool,
    imports: &[String],
    recipes: &HashSet<String>,
    recipe_rates: &RecipeRepository,
) -> Result<NamedModule, Box<dyn std::error::Error>> {
    let mut recipes = recipes.clone();
    recipes.insert(name.to_owned());
    let mut module = ModuleBuilder::new(name.to_owned(), recipe_rates, imports, &recipes);
    module.add(name, expand);
    get_recipe(recipe_rates, name)?;
    Ok(module.build())
}

pub fn get_recipe<'a>(
    recipes: &'a RecipeRepository,
    name: &str,
) -> Result<Rate<&'a RecipeRate>, String> {
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
                eprintln!("{k}");
            }
        }
        Err(format!("{name} was not found"))
    }
}

pub fn round_string(d: Decimal) -> String {
    d.round_dp(3).to_string()
}
