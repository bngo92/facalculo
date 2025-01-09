#![feature(let_chains)]
use petgraph::{algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef, Direction};
use rust_decimal::Decimal;
use serde_derive::{Deserialize, Serialize};
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

pub mod compute;

pub type GraphType = StableGraph<Node, Edge>;

pub struct Graph<'a> {
    pub graph: GraphType,
    root: NodeIndex,
    recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
}

impl<'a> Graph<'a> {
    pub fn new(recipes: &'a HashMap<&'a str, RecipeRate<'a>>) -> Graph<'a> {
        let mut graph = GraphType::new();
        let root = graph.add_node(Node {
            required: None,
            name: String::new(),
        });
        Graph {
            graph,
            root,
            recipes,
        }
    }

    pub fn add(&mut self, required: Decimal, key: &str, belt: Option<i64>) {
        let node = build_node(required, key, self.recipes, &mut self.graph, belt);
        self.graph.add_edge(
            self.root,
            node,
            Edge {
                required,
                belt,
                item: key.to_owned(),
            },
        );
    }

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
                let required = self.graph[node].required.unwrap();
                *self.graph[selected_node].required.as_mut().unwrap() += required;
                self.graph.remove_node(node);
            }
        }
    }
}

impl Graph<'_> {
    pub fn to_raw(self) -> RawGraph {
        RawGraph {
            graph: self.graph,
            root: self.root,
        }
    }
}

fn build_node(
    required: Decimal,
    key: &str,
    recipes: &HashMap<&str, RecipeRate>,
    graph: &mut GraphType,
    belt: Option<i64>,
) -> NodeIndex {
    let node = if let Some(recipe) = recipes.get(key) {
        let ratio = required / recipe.results[0].rate;
        let node = graph.add_node(Node {
            required: Some(ratio),
            name: key.to_owned(),
        });
        for i in &recipe.ingredients {
            let belt = if let None | Some(Category::OilProcessing) =
                recipes.get(i.name.as_str()).map(|r| r.category)
            {
                None
            } else {
                belt
            };
            let n = build_node(ratio * i.rate, &i.name, recipes, graph, belt);
            graph.add_edge(
                node,
                n,
                Edge {
                    required: ratio * i.rate,
                    belt,
                    item: i.name.to_owned(),
                },
            );
        }
        node
    } else {
        graph.add_node(Node {
            required: None,
            name: key.to_owned(),
        })
    };
    node
}

pub fn round_string(d: Decimal) -> String {
    d.round_dp(3).to_string()
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Node {
    pub required: Option<Decimal>,
    pub name: String,
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

#[derive(Clone, Debug, PartialEq, Serialize)]
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
            round_string(self.required),
            String::from_iter(
                self.item
                    .split('-')
                    .map(|s| s.chars().next().unwrap())
                    .collect::<Vec<_>>()
            )
        )?;
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

#[derive(Serialize)]
pub struct RawGraph {
    graph: GraphType,
    root: NodeIndex,
}
