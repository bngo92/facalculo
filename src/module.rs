use crate::{
    data::{RecipeRate, RecipeRepository, RepositoryOption},
    Rate,
};
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
    pub fn new(
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
        match rate {
            Rate::Recipe(recipe) => {
                self.structures.push(Structure::Recipe(Recipe {
                    name: recipe.key.clone(),
                }));
                if expand {
                    self.add_node(recipe).unwrap();
                }
            }
            Rate::Resource(resource) => {
                self.structures.push(Structure::Resource(Resource {
                    name: resource.key.clone(),
                }));
            }
        }
    }

    fn add_node(&mut self, recipe: &RecipeRate) -> Result<(), Box<dyn std::error::Error>> {
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
                        self.structures.push(Structure::Recipe(Recipe {
                            name: recipe.key.clone(),
                        }));
                        self.add_node(recipe)?;
                    }
                    Rate::Resource(resource) => {
                        self.structures.push(Structure::Resource(Resource {
                            name: resource.key.to_owned(),
                        }));
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
    },
    RocketSilo {
        #[serde(default)]
        modules: HashMap<String, i32>,
    },
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Structure {
    Resource(Resource),
    Recipe(Recipe),
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Resource {
    pub name: String,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct Recipe {
    pub name: String,
}
