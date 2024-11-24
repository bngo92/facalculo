#![feature(let_chains)]
use petgraph::{algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef, Direction};
use rust_decimal::Decimal;
use serde_derive::{Deserialize, Serialize};
use std::{collections::HashMap, fmt::Display};

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
        self.graph
            .add_edge(self.root, node, Edge { required, belt });
    }

    pub fn group_nodes(&mut self) {
        // Select one node for each item
        let mut selected_nodes: HashMap<_, NodeIndex> = HashMap::new();
        let mut nodes = algo::toposort(&self.graph, None).expect("graph should be directed");
        for node in &nodes {
            for target in self.graph.neighbors_directed(*node, Direction::Outgoing) {
                if !selected_nodes.contains_key(&self.graph[target].name) {
                    selected_nodes.insert(self.graph[target].name.to_owned(), target);
                }
            }
        }
        // Traverse nodes in reverse order so we only have to handle incoming edges
        nodes.reverse();
        for node in nodes {
            let Some(first_node) = selected_nodes.get(&self.graph[node].name).copied() else {
                continue;
            };
            if node == first_node {
                continue;
            }
            let edges: Vec<_> = self
                .graph
                .edges_directed(node, Direction::Incoming)
                .map(|e| (e.source(), e.id(), *e.weight()))
                .collect();
            for (source, id, edge) in edges {
                // Merge edge with existing edge between selected nodes
                if let Some(first_source) = selected_nodes.get(&self.graph[source].name).copied()
                    && let Some(edge) = self.graph.find_edge(first_source, first_node)
                {
                    if source == first_source {
                        continue;
                    }
                    let required = self.graph[id].required;
                    self.graph[edge].required += required;
                // or move edge to connect them
                } else {
                    self.graph.remove_edge(id);
                    self.graph.add_edge(source, first_node, edge);
                }
            }
            // Merge nodes
            let x = self.graph.remove_node(node).unwrap().required.unwrap();
            *self.graph[first_node].required.as_mut().unwrap() += x;
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

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub struct Edge {
    pub required: Decimal,
    pub belt: Option<i64>,
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", round_string(self.required))?;
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
