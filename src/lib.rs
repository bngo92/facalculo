use petgraph::graph::NodeIndex;
use rust_decimal::Decimal;
use std::{collections::HashMap, fmt::Display};

pub mod compute;

type GraphType = petgraph::Graph<Node, String>;

pub struct Graph<'a> {
    graph: GraphType,
    root: NodeIndex,
    recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
}

impl<'a> Graph<'a> {
    pub fn new(
        required: Decimal,
        key: &str,
        recipes: &'a HashMap<&'a str, RecipeRate<'a>>,
    ) -> Graph<'a> {
        let mut graph = GraphType::new();
        let root = build_node(required, key, recipes, &mut graph);
        Graph {
            graph,
            root,
            recipes,
        }
    }

    pub fn group_nodes(&self) -> Graph<'a> {
        let mut graph = GraphType::new();
        // The root node doesn't have an incoming edge so it needs to be copied separately
        let mut nodes = HashMap::from([(
            &self.graph[self.root].name,
            graph.add_node(self.graph[self.root].clone()),
        )]);
        // Aggregate nodes by incoming edge
        for i in self.graph.edge_indices() {
            let (ix, iy) = self.graph.edge_endpoints(i).unwrap();
            if !nodes.contains_key(&self.graph[ix].name) {
                nodes.insert(
                    &self.graph[ix].name,
                    graph.add_node(Node {
                        required: Decimal::ZERO,
                        name: self.graph[ix].name.clone(),
                    }),
                );
            }
            let x = nodes[&self.graph[ix].name];
            if !nodes.contains_key(&self.graph[iy].name) {
                nodes.insert(
                    &self.graph[iy].name,
                    graph.add_node(Node {
                        required: Decimal::ZERO,
                        name: self.graph[iy].name.clone(),
                    }),
                );
            }
            let y = nodes[&self.graph[iy].name];
            graph[y].required += self.graph[iy].required;
            if !graph.contains_edge(x, y) {
                graph.add_edge(x, y, String::new());
            }
        }
        Graph {
            graph,
            root: self.root,
            recipes: self.recipes,
        }
    }
}

fn build_node(
    required: Decimal,
    key: &str,
    recipes: &HashMap<&str, RecipeRate>,
    graph: &mut GraphType,
) -> NodeIndex {
    let node = graph.add_node(Node {
        required,
        name: key.to_owned(),
    });
    if let Some(recipe) = recipes.get(key) {
        let ratio = required / recipe.results[0].rate;
        for i in &recipe.ingredients {
            let n = build_node(ratio * i.rate, &i.name, recipes, graph);
            graph.add_edge(node, n, String::new());
        }
    }
    node
}

pub fn round_string(d: &Decimal) -> String {
    d.round_dp(3).to_string()
}

#[derive(Clone)]
pub struct Node {
    pub required: Decimal,
    pub name: String,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", round_string(&self.required), self.name)
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
            round_string(&self.results[0].rate),
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
        write!(f, "{} {}", round_string(&self.rate), self.name)
    }
}
