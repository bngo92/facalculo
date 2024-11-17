use crate::Graph;
use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec, parse,
    printer::PrinterContext,
};
use petgraph::{dot::Dot, visit::Dfs};
use rust_decimal::Decimal;
use std::{collections::HashMap, process::Command};

pub fn render(graph: &Graph) -> Result<(), Box<dyn std::error::Error>> {
    let dot = Dot::new(&graph.graph);
    let g = parse(&dot.to_string())?;
    exec(
        g,
        &mut PrinterContext::default(),
        vec![
            Format::Svg.into(),
            CommandArg::Output("out.svg".to_string()),
        ],
    )?;
    Command::new("open").arg("out.svg").spawn()?;
    Ok(())
}

pub fn total<'a>(graph: &'a Graph) -> HashMap<&'a str, Decimal> {
    let Graph {
        graph,
        root,
        recipes,
    } = graph;
    let mut dfs = Dfs::new(graph, *root);
    let mut total = HashMap::new();
    while let Some(nx) = dfs.next(&graph) {
        if let Some(recipe) = recipes.get(graph[nx].name.as_str()) {
            let ratio = graph[nx].required / recipe.results[0].rate;
            for i in &recipe.ingredients {
                *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
            }
        }
    }
    total
}
