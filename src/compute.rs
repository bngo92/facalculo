use crate::Graph;
use graphviz_rust::{
    cmd::{CommandArg, Format},
    exec, parse,
    printer::PrinterContext,
};
use petgraph::{
    visit::{Dfs, EdgeRef, IntoEdgeReferences, IntoNodeReferences, NodeIndexable, NodeRef},
    Direction,
};
use rust_decimal::Decimal;
use std::{collections::HashMap, fmt::Write, process::Command};

static INDENT: &str = "    ";

pub fn render(graph: &Graph, module: bool) -> Result<(), Box<dyn std::error::Error>> {
    let g = &graph.graph;
    let mut f = String::new();
    writeln!(f, "digraph {{")?;
    writeln!(f, "{INDENT}0 [label = \"outputs\"]")?;
    writeln!(f, "{INDENT}1 [label = \"inputs\"]")?;
    for node in g.node_references() {
        writeln!(
            f,
            "{}{} [label = \"{}\"]",
            INDENT,
            g.to_index(node.id()) + 2,
            node.weight()
        )?;
    }
    // Render inputs and outputs outside of the subgraph
    for node in g.externals(Direction::Incoming) {
        writeln!(
            f,
            "{}0 -> {} [label = \"{}\" dir=back]",
            INDENT,
            node.index() + 2,
            g[node].trim()
        )?;
    }
    for node in g.externals(Direction::Outgoing) {
        writeln!(
            f,
            "{}{} -> 1 [label = \"{}\" dir=back]",
            INDENT,
            node.index() + 2,
            g[node].trim()
        )?;
    }
    if module {
        writeln!(f, "{}subgraph cluster {{", INDENT)?;
    }
    for edge in g.edge_references() {
        writeln!(
            f,
            "{}{}{} -> {} [label = \"{}\" dir=back]",
            INDENT,
            if module { INDENT } else { "" },
            g.to_index(edge.source()) + 2,
            g.to_index(edge.target()) + 2,
            edge.weight()
        )?;
    }
    if module {
        writeln!(f, "{}}}", INDENT)?;
    }
    writeln!(f, "}}")?;
    let g = parse(&f.to_string())?;
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
    let Graph { graph, recipes, .. } = graph;
    let mut dfs = Dfs::new(graph, graph.externals(Direction::Incoming).next().unwrap());
    let mut total = HashMap::new();
    while let Some(nx) = dfs.next(&graph) {
        if let Some(recipe) = recipes.get(graph[nx].name.as_str()) {
            let ratio = graph[nx].required.unwrap() / recipe.results[0].rate;
            for i in &recipe.ingredients {
                *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
            }
        }
    }
    total
}
