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
    for node in g.node_references() {
        writeln!(
            f,
            "{}{} [label = \"{}\"]",
            INDENT,
            g.to_index(node.id()),
            if node.id() == graph.input {
                "inputs".to_owned()
            } else if node.id() == graph.root {
                "outputs".to_owned()
            } else {
                node.weight().to_string()
            }
        )?;
    }
    // Render inputs and outputs outside of the subgraph
    let mut inputs = Vec::new();
    for edge in g
        .edges(graph.root)
        .chain(g.edges_directed(graph.input, Direction::Incoming))
    {
        writeln!(
            f,
            "{}{} -> {} [label = \"{}\" dir=back]",
            INDENT,
            g.to_index(edge.source()),
            g.to_index(edge.target()),
            edge.weight()
        )?;
    }
    for edge in g.edge_references() {
        if edge.source() == graph.root {
            continue;
        }
        if graph.inputs.contains(&g[edge.target()].name) {
            writeln!(
                f,
                "{}{} -> {} [label = \"{}\" dir=back]",
                INDENT,
                g.to_index(edge.source()),
                g.to_index(edge.target()),
                edge.weight()
            )?;
            inputs.push(edge.target());
        }
    }
    if module {
        writeln!(f, "{}subgraph cluster {{", INDENT)?;
    }
    for edge in g.edge_references() {
        if inputs.contains(&edge.target())
            || edge.source() == graph.root
            || edge.target() == graph.input
        {
            continue;
        }
        writeln!(
            f,
            "{}{}{} -> {} [label = \"{}\" dir=back]",
            INDENT,
            if module { INDENT } else { "" },
            g.to_index(edge.source()),
            g.to_index(edge.target()),
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
    let Graph {
        graph,
        root,
        recipes,
        ..
    } = graph;
    let mut dfs = Dfs::new(graph, *root);
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
