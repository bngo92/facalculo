use crate::{Category, Rate, RecipeRate};
use petgraph::{algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef, Direction};
use rust_decimal::Decimal;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

pub type GraphType = StableGraph<Node, Edge>;

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
    module: Module,
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

pub struct Graph<'a> {
    pub name: String,
    pub graph: GraphType,
    pub recipes: &'a RecipeRepository,
    pub imports: HashMap<String, Vec<(usize, Decimal)>>,
    pub outputs: HashSet<&'a str>,
    index: usize,
}

impl<'a> Graph<'a> {
    pub fn new(recipes: &'a RecipeRepository, index: usize) -> Graph<'a> {
        Graph {
            name: String::new(),
            graph: GraphType::new(),
            recipes,
            imports: HashMap::new(),
            index,
            outputs: HashSet::new(),
        }
    }

    pub fn from_module(
        module: NamedModule,
        required: &HashMap<String, Decimal>,
        defaults: &HashMap<String, Result<Decimal, Decimal>>,
        recipes: &'a RecipeRepository,
        belt: Option<i64>,
        index: usize,
    ) -> Graph<'a> {
        let mut graph = Graph::new(recipes, index);
        graph.name = module.name.clone();
        let outputs = if module.name == "advanced-oil-processing" {
            vec![(
                "petroleum-gas",
                recipes.get("advanced-oil-processing").unwrap(),
            )]
        } else {
            recipes
                .get_outputs(&module)
                .iter()
                .filter_map(|o| recipes.get(o).map(|r| (*o, r)))
                .collect()
        };
        for (item, recipe) in outputs {
            graph.build_module_node(
                &module,
                item,
                *required.get(item).unwrap_or_else(|| match &defaults[item] {
                    Ok(rate) => rate,
                    Err(rate) => {
                        eprintln!(
                            "Using {} for {item} (1 assembler)",
                            crate::round_string(*rate)
                        );
                        rate
                    }
                }),
                recipe.rate(),
                belt,
            );
        }
        graph.outputs = recipes.get_outputs(&module);
        graph
    }

    fn build_module_node(
        &mut self,
        module: &NamedModule,
        ingredient: &str,
        required: Decimal,
        recipe: &RecipeRate,
        belt: Option<i64>,
    ) -> NodeIndex {
        let ratio = required
            / recipe
                .results
                .iter()
                .find(|i| i.name == ingredient)
                .unwrap()
                .rate;
        let node = self.graph.add_node(Node {
            required: Some(ratio),
            name: recipe.key.to_owned(),
        });
        for edge in self.get_ingredients(recipe, ratio, belt) {
            if self.recipes.get_inputs(module).contains(edge.item.as_str()) {
                self.imports
                    .entry(edge.item.to_owned())
                    .or_default()
                    .push((self.index + node.index(), edge.required));
            } else {
                let recipe = match self.recipes.get_options(&edge.item) {
                    RepositoryOption::None => continue,
                    RepositoryOption::Some(recipe) => recipe.rate(),
                    RepositoryOption::Multiple(recipes) => {
                        let recipes: HashSet<_> = recipes.iter().collect();
                        let Module::User(structures) = &module.module;
                        let recipe = structures
                            .iter()
                            .map(|s| match s {
                                Structure::Recipe(r) => &r.name,
                                Structure::Resource(r) => &r.name,
                            })
                            .find(|r| recipes.contains(r))
                            .unwrap();
                        self.recipes.get(recipe).unwrap().rate()
                    }
                };
                let n = self.build_module_node(module, &edge.item, edge.required, recipe, belt);
                self.graph.add_edge(node, n, edge);
            }
        }
        node
    }

    pub fn get_ingredients(
        &self,
        recipe: &RecipeRate,
        ratio: Decimal,
        belt: Option<i64>,
    ) -> Vec<Edge> {
        recipe
            .ingredients
            .iter()
            .map(move |i| {
                let belt = if let None | Some(Category::OilProcessing) =
                    self.recipes.get(i.name.as_str()).and_then(|r| {
                        if let Rate::Recipe(r) = r {
                            r.category
                        } else {
                            None
                        }
                    }) {
                    None
                } else {
                    belt
                };
                Edge {
                    required: ratio * i.rate,
                    belt,
                    item: i.name.to_owned(),
                }
            })
            .collect()
    }

    // TODO: we should not expand group nodes
    pub fn group_nodes(&mut self, items: Vec<String>) {
        // Group items and their dependencies
        // Select one node for each item
        let mut selected_nodes: HashMap<_, NodeIndex> = HashMap::new();
        let nodes = algo::toposort(&self.graph, None).expect("graph should be directed");
        for node in nodes.iter().copied() {
            if !self.graph.contains_node(node) {
                continue;
            }
            if !items.is_empty() && !items.contains(&self.graph[node].name) {
                continue;
            }
            let Some(selected_node) = selected_nodes.get(&self.graph[node].name).copied() else {
                selected_nodes.insert(self.graph[node].name.to_owned(), node);
                continue;
            };

            // Move incoming edges to selected node
            //
            // Suppose we are grouping b
            //
            // a1 a2
            //  |  |
            // b1 b2
            //  |  |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            // b1 b2
            //  |  |
            // c1 c2
            let edges: Vec<_> = self
                .graph
                .edges_directed(node, Direction::Incoming)
                .map(|e| (e.source(), e.id(), e.weight().clone()))
                .collect();
            for (source, id, edge) in edges {
                self.graph.add_edge(source, selected_node, edge);
                self.graph.remove_edge(id);
            }

            // Merge subgraphs with BFS
            // a1 a2
            //   \ |
            // b1 b2
            //  |  |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            // b1 b2
            //     |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            //    b2
            //     |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            //    b2
            //     |
            //    c2
            let mut node_bfs = VecDeque::from_iter([node]);
            let mut selected_bfs = VecDeque::from_iter([selected_node]);
            while let Some(node) = node_bfs.pop_front() {
                let mut edges: Vec<_> = self
                    .graph
                    .edges_directed(node, Direction::Outgoing)
                    .map(|e| (e.target(), e.id(), e.weight().clone()))
                    .collect();
                edges.sort_by_key(|(target, _, _)| &self.graph[*target].name);
                let selected_node = selected_bfs.pop_front().unwrap();
                let mut selected_edges: Vec<_> = self
                    .graph
                    .edges_directed(selected_node, Direction::Outgoing)
                    .map(|e| (e.target(), e.id()))
                    .collect();
                selected_edges.sort_by_key(|(target, _)| &self.graph[*target].name);
                for (edge, selected_edge) in edges.into_iter().zip(selected_edges.into_iter()) {
                    self.graph[selected_edge.1].required += edge.2.required;
                    self.graph.remove_edge(edge.1);
                    node_bfs.push_back(edge.0);
                    selected_bfs.push_back(selected_edge.0);
                }
                // The input node does not have required
                if let Some(required) = self.graph[node].required {
                    *self.graph[selected_node].required.as_mut().unwrap() += required;
                    self.graph.remove_node(node);
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub required: Option<Decimal>,
    pub name: String,
}

impl Node {
    pub fn trim(&self) -> String {
        if let Some(required) = self.required {
            format!("{} {}", crate::round_string(required), trim(&self.name))
        } else {
            self.name.clone()
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(required) = self.required {
            write!(f, "{} {}", crate::round_string(required), self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Edge {
    pub required: Decimal,
    pub belt: Option<i64>,
    pub item: String,
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            crate::round_string(self.required),
            trim(&self.item)
        )?;
        if let Some(belt) = self.belt {
            write!(
                f,
                " ({})",
                crate::round_string(self.required / Decimal::from(belt))
            )?;
        }
        Ok(())
    }
}

fn trim(s: &str) -> String {
    String::from_iter(
        s.split('-')
            .map(|s| s.chars().next().unwrap())
            .collect::<Vec<_>>(),
    )
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
}

impl RecipeRepository {
    pub fn get(&self, key: &str) -> Option<Rate> {
        if let Some(recipe) = self.recipes.get(key) {
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

    pub fn get_inputs(&self, module: &NamedModule) -> HashSet<&str> {
        let Module::User(structures) = &module.module;
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

    pub fn get_resource_inputs(&self, module: &NamedModule) -> HashSet<&str> {
        let Module::User(structures) = &module.module;
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

    pub fn get_outputs(&self, module: &NamedModule) -> HashSet<&str> {
        let Module::User(structures) = &module.module;
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
