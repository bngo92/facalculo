use crate::{
    data::{RecipeRepository, RepositoryOption},
    Rate,
};
use serde_derive::{Deserialize, Serialize};
use std::collections::HashSet;

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
                self.add_node(key, expand).unwrap();
            }
            Rate::Resource(resource) => {
                self.structures.push(Structure::Resource(Resource {
                    name: resource.key.clone(),
                }));
            }
        }
    }

    fn add_node(&mut self, key: &str, expand: bool) -> Result<(), Box<dyn std::error::Error>> {
        if expand {
            match self.repository.get_options(key) {
                RepositoryOption::None => {}
                RepositoryOption::Some(rate) => match rate {
                    Rate::Recipe(recipe) => {
                        for edge in &recipe.ingredients {
                            let edge = &edge.name;
                            if !self.imports.contains(edge) {
                                self.structures
                                    .push(match self.repository.get(edge).unwrap() {
                                        Rate::Recipe(recipe) => Structure::Recipe(Recipe {
                                            name: recipe.key.clone(),
                                        }),
                                        Rate::Resource(recipe) => Structure::Resource(Resource {
                                            name: recipe.key.clone(),
                                        }),
                                    });
                                self.add_node(edge, expand)?;
                            }
                        }
                    }
                    Rate::Resource(resource) => {
                        if !self.imports.contains(&resource.key) {
                            self.structures.push(Structure::Resource(Resource {
                                name: resource.key.to_owned(),
                            }));
                        }
                    }
                },
                // Users are required to specify a recipe for an item if there are multiple recipes
                // that produce the item
                RepositoryOption::Multiple(options) => {
                    let options: HashSet<_> = options.iter().cloned().collect();
                    let recipes: Vec<_> = options.intersection(self.recipes).collect();
                    if recipes.is_empty() {
                        Err(format!("Multiple recipes were found for {key}"))?
                    }
                    let rate = &self.repository.get(recipes[0]).unwrap();
                    match rate {
                        Rate::Recipe(recipe) => {
                            for edge in &recipe.ingredients {
                                let edge = &edge.name;
                                if !self.imports.contains(edge) {
                                    self.structures.push(Structure::Recipe(Recipe {
                                        name: recipe.key.clone(),
                                    }));
                                    self.add_node(edge, expand)?;
                                }
                            }
                        }
                        Rate::Resource(resource) => {
                            if !self.imports.contains(&resource.key) {
                                self.structures.push(Structure::Resource(Resource {
                                    name: resource.key.to_owned(),
                                }));
                            }
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn build(self) -> Result<NamedModule, Box<dyn std::error::Error>> {
        Ok(NamedModule {
            name: self.name,
            module: Module::User(self.structures),
        })
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct NamedModule {
    pub name: String,
    pub module: Module,
}

#[derive(Clone, Deserialize, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum Module {
    User(Vec<Structure>),
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
