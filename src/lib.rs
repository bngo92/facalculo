use petgraph::graph::NodeIndex;
use rust_decimal::Decimal;
use std::{collections::HashMap, fmt::Display};

pub mod compute;

type Graph = petgraph::Graph<Node, String>;

pub fn build_graph(
    required: Decimal,
    key: &str,
    recipe_rates: &HashMap<&str, RecipeRate>,
) -> (Graph, NodeIndex) {
    let mut graph = Graph::new();
    let root = build_node(required, key, recipe_rates, &mut graph);
    (graph, root)
}

fn build_node(
    required: Decimal,
    key: &str,
    recipe_rates: &HashMap<&str, RecipeRate>,
    graph: &mut Graph,
) -> NodeIndex {
    let node = graph.add_node(Node {
        required,
        name: key.to_owned(),
    });
    if let Some(recipe) = recipe_rates.get(key) {
        let ratio = required / recipe.results[0].rate;
        for i in &recipe.ingredients {
            let n = build_node(ratio * i.rate, &i.name, recipe_rates, graph);
            graph.add_edge(node, n, String::new());
        }
    }
    node
}

pub fn round_string(d: &Decimal) -> String {
    d.round_dp(3).to_string()
}

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
