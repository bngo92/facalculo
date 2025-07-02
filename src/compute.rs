use crate::graph::Graph;
use petgraph::{visit::Dfs, Direction};
use rust_decimal::Decimal;
use std::collections::HashMap;

pub fn total<'a>(graphs: &'a [Graph]) -> HashMap<&'a str, Decimal> {
    let mut total = HashMap::new();
    for graph in graphs {
        let Graph { graph, recipes, .. } = graph;
        let mut dfs = Dfs::new(graph, graph.externals(Direction::Incoming).next().unwrap());
        while let Some(nx) = dfs.next(&graph) {
            if let Some(recipe) = recipes.get(graph[nx].name.as_str()) {
                let ratio = graph[nx].required.unwrap() / recipe.rate().results[0].rate;
                for i in &recipe.rate().ingredients {
                    *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
                }
            }
        }
    }
    total
}
