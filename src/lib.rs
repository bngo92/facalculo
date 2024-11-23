use petgraph::graph::NodeIndex;
use rust_decimal::Decimal;
use serde_derive::Serialize;
use std::{collections::HashMap, fmt::Display};

pub mod compute;

type GraphType = petgraph::Graph<Node, Edge>;

pub struct Graph<'a> {
    graph: GraphType,
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

    pub fn group_nodes(&self) -> Graph<'a> {
        let mut graph = GraphType::new();
        // The root node doesn't have an incoming edge so it needs to be copied separately
        let mut nodes = HashMap::from([(
            &self.graph[self.root].name,
            graph.add_node(self.graph[self.root].clone()),
        )]);
        let mut edges = HashMap::new();
        // Aggregate nodes by incoming edge
        for i in self.graph.edge_indices() {
            let (ix, iy) = self.graph.edge_endpoints(i).unwrap();
            if !nodes.contains_key(&self.graph[ix].name) {
                nodes.insert(
                    &self.graph[ix].name,
                    graph.add_node(Node {
                        required: Some(Decimal::ZERO),
                        name: self.graph[ix].name.clone(),
                    }),
                );
            }
            let x = nodes[&self.graph[ix].name];
            if !nodes.contains_key(&self.graph[iy].name) {
                nodes.insert(
                    &self.graph[iy].name,
                    graph.add_node(Node {
                        required: Some(Decimal::ZERO),
                        name: self.graph[iy].name.clone(),
                    }),
                );
            }
            let y = nodes[&self.graph[iy].name];
            if let Some(required) = self.graph[iy].required {
                *graph[y].required.as_mut().unwrap() += required;
            }
            if !edges.contains_key(&(&self.graph[ix].name, &self.graph[iy].name)) {
                edges.insert(
                    (&self.graph[ix].name, &self.graph[iy].name),
                    graph.add_edge(
                        x,
                        y,
                        Edge {
                            required: Decimal::ZERO,
                            belt: self.graph[i].belt,
                        },
                    ),
                );
            }
            let edge = edges[&(&self.graph[ix].name, &self.graph[iy].name)];
            graph[edge].required += self.graph[i].required;
        }
        Graph {
            graph,
            root: self.root,
            recipes: self.recipes,
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

#[derive(Clone, Serialize)]
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

#[derive(Clone, Serialize)]
pub struct Edge {
    required: Decimal,
    belt: Option<i64>,
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
