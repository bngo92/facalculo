use crate::{
    Rate,
    data::{RecipeRate, RecipeRepository, RepositoryOption},
};
use core::error::Error;
use rust_decimal::Decimal;
use serde_derive::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

pub struct ModuleBuilder<'a> {
    name: String,
    repository: &'a RecipeRepository,
    imports: &'a [String],
    recipes: &'a HashSet<String>,
    structures: Vec<Structure>,
}

impl<'a> ModuleBuilder<'_> {
    pub const fn new(
        name: String,
        repository: &'a RecipeRepository,
        imports: &'a [String],
        recipes: &'a HashSet<String>,
    ) -> ModuleBuilder<'a> {
        ModuleBuilder {
            name,
            repository,
            imports,
            recipes,
            structures: Vec::new(),
        }
    }

    pub fn add(&mut self, key: &str, expand: bool) {
        let rate = &self.repository.get(key).unwrap();
        match *rate {
            Rate::Recipe(recipe) => {
                self.structures.push(Structure::Recipe {
                    name: recipe.key.clone(),
                    modules: HashMap::new(),
                });
                if expand {
                    self.add_node(recipe).unwrap();
                }
            }
            Rate::Resource(resource) => {
                self.structures.push(Structure::Resource {
                    name: resource.key.clone(),
                    structure: None,
                });
            }
        }
    }

    fn add_node(&mut self, recipe: &RecipeRate) -> Result<(), Box<dyn Error>> {
        for edge in &recipe.ingredients {
            let edge = &edge.name;
            if !self.imports.contains(edge) {
                let rate = match self.repository.get_options(edge) {
                    RepositoryOption::None => continue,
                    RepositoryOption::Some(rate) => rate,
                    // Users are required to specify a recipe for an item if there are multiple recipes
                    // that produce the item
                    RepositoryOption::Multiple(options) => {
                        let options: HashSet<_> = options.iter().cloned().collect();
                        let recipes: Vec<_> = options.intersection(self.recipes).collect();
                        if recipes.is_empty() {
                            Err(format!(
                                "Multiple recipes were found for {edge} for {}",
                                recipe.key
                            ))?
                        }
                        self.repository.get(recipes[0]).unwrap()
                    }
                };
                match rate {
                    Rate::Recipe(recipe) => {
                        self.structures.push(Structure::Recipe {
                            name: recipe.key.clone(),
                            modules: HashMap::new(),
                        });
                        self.add_node(recipe)?;
                    }
                    Rate::Resource(resource) => {
                        self.structures.push(Structure::Resource {
                            name: resource.key.to_owned(),
                            structure: None,
                        });
                    }
                }
            }
        }
        Ok(())
    }

    pub fn build(self) -> NamedModule {
        NamedModule {
            name: self.name,
            module: Module::User {
                structures: self.structures,
            },
        }
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct NamedModule {
    pub name: String,
    pub module: Module,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "kebab-case")]
pub enum Module {
    User {
        structures: Vec<Structure>,
    },
    AdvancedOilProcessing {},
    Science {
        #[serde(default)]
        modules: HashMap<String, i32>,
        #[serde(default)]
        research_speed: Option<Decimal>,
        #[serde(default)]
        research_time: Option<Decimal>,
    },
    RocketSilo {
        #[serde(default)]
        modules: HashMap<String, i32>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Structure {
    Resource {
        name: String,
        #[serde(default)]
        structure: Option<String>,
    },
    Recipe {
        name: String,
        #[serde(default)]
        modules: HashMap<String, i32>,
    },
}
