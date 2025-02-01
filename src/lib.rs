#![feature(let_chains)]
use petgraph::{algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef, Direction};
use rust_decimal::Decimal;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

pub mod compute;

pub type GraphType = StableGraph<Node, Edge>;

pub struct ModuleBuilder<'a> {
    name: String,
    outputs: Vec<String>,
    nodes: HashMap<String, Vec<String>>,
    recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
    imports: &'a [String],
}

impl<'a> ModuleBuilder<'_> {
    pub fn new(
        name: String,
        recipes: &'a HashMap<&str, RecipeRate>,
        imports: &'a [String],
    ) -> ModuleBuilder<'a> {
        ModuleBuilder {
            name,
            outputs: Vec::new(),
            nodes: HashMap::new(),
            recipes,
            imports,
        }
    }

    pub fn add(&mut self, key: &str, expand: bool) {
        self.outputs.push(key.to_owned());
        self.add_node(key, expand);
    }

    fn add_node(&mut self, key: &str, expand: bool) {
        let mut edges = Vec::new();
        if expand {
            if let Some(recipe) = self.recipes.get(key) {
                for edge in &recipe.ingredients {
                    let edge = &edge.name;
                    if !self.imports.contains(edge) {
                        self.add_node(edge, expand);
                    }
                    edges.push(edge.clone());
                }
            }
        }
        self.nodes.insert(key.to_owned(), edges);
    }

    pub fn build(self) -> Module {
        let mut inputs: HashMap<String, HashSet<String>> = HashMap::new();
        for (item, dependencies) in &self.nodes {
            if let Some(r) = self.recipes.get(item.as_str()) {
                if let Category::Mining = r.category {
                    inputs.entry(item.clone()).or_default().insert(item.clone());
                }
            }
            for d in dependencies {
                if !self.nodes.contains_key(d.as_str()) {
                    inputs.entry(d.clone()).or_default().insert(item.clone());
                }
            }
        }
        Module {
            name: self.name,
            outputs: self.outputs,
            nodes: self.nodes,
            inputs,
        }
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct Module {
    pub name: String,
    pub outputs: Vec<String>,
    pub nodes: HashMap<String, Vec<String>>,
    pub inputs: HashMap<String, HashSet<String>>,
}

pub struct Graph<'a> {
    pub name: String,
    pub graph: GraphType,
    recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
    pub imports: HashMap<String, Vec<(usize, Decimal)>>,
    index: usize,
}

impl<'a> Graph<'a> {
    pub fn new(recipes: &'a HashMap<&'a str, RecipeRate<'a>>, index: usize) -> Graph<'a> {
        Graph {
            name: String::new(),
            graph: GraphType::new(),
            recipes,
            imports: HashMap::new(),
            index,
        }
    }

    pub fn from_module(
        module: Module,
        required: &HashMap<String, Decimal>,
        defaults: &HashMap<String, Result<Decimal, Decimal>>,
        recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
        belt: Option<i64>,
        index: usize,
    ) -> Graph<'a> {
        let mut graph = Graph::new(recipes, index);
        graph.name = module.name.clone();
        for item in &module.outputs {
            if let Some(recipe) = recipes.get(item.as_str()) {
                graph.build_module_node(
                    &module,
                    *required.get(item).unwrap_or_else(|| match &defaults[item] {
                        Ok(rate) => rate,
                        Err(rate) => {
                            eprintln!("Using {} for {item} (1 assembler)", round_string(*rate));
                            rate
                        }
                    }),
                    recipe,
                    belt,
                );
            }
        }
        graph
    }

    fn build_module_node(
        &mut self,
        module: &Module,
        required: Decimal,
        recipe: &RecipeRate,
        belt: Option<i64>,
    ) -> NodeIndex {
        let ratio = required / recipe.results[0].rate;
        let node = self.graph.add_node(Node {
            required: Some(ratio),
            name: recipe.key.to_owned(),
        });
        for edge in self.get_ingredients(recipe, ratio, belt) {
            if let Some(recipe) = self.recipes.get(edge.item.as_str()) {
                if module.nodes.contains_key(&edge.item) {
                    let n = self.build_module_node(module, edge.required, recipe, belt);
                    self.graph.add_edge(node, n, edge);
                } else {
                    self.imports
                        .entry(edge.item.to_owned())
                        .or_default()
                        .push((self.index + node.index(), edge.required));
                }
            }
        }
        node
    }

    fn get_ingredients(
        &self,
        recipe: &RecipeRate<'_>,
        ratio: Decimal,
        belt: Option<i64>,
    ) -> Vec<Edge> {
        recipe
            .ingredients
            .iter()
            .map(move |i| {
                let belt = if let None | Some(Category::OilProcessing) =
                    self.recipes.get(i.name.as_str()).map(|r| r.category)
                {
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

pub fn round_string(d: Decimal) -> String {
    d.round_dp(3).to_string()
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub required: Option<Decimal>,
    pub name: String,
}

impl Node {
    fn trim(&self) -> String {
        if let Some(required) = self.required {
            format!("{} {}", round_string(required), trim(&self.name))
        } else {
            self.name.clone()
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(required) = self.required {
            write!(f, "{} {}", round_string(required), self.name)
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
        write!(f, "{} {}", round_string(self.required), trim(&self.item))?;
        if let Some(belt) = self.belt {
            write!(
                f,
                " ({})",
                round_string(self.required / Decimal::from(belt))
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

pub struct RecipeRate<'a> {
    pub category: Category,
    pub key: &'a str,
    pub ingredients: Vec<IngredientRate>,
    pub results: Vec<IngredientRate>,
}

impl Display for RecipeRate<'_> {
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
