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

pub fn render(graph: &Graph) -> Result<(), Box<dyn std::error::Error>> {
    let g = &graph.graph;
    let mut f = String::new();
    writeln!(f, "digraph {{")?;
    writeln!(f, "{INDENT}0 [label = \"outputs\"]")?;
    writeln!(f, "{INDENT}1 [label = \"inputs\"]")?;
    writeln!(f, "{}subgraph cluster {{", INDENT)?;
    writeln!(f, "{INDENT}{INDENT}node [shape=record]")?;
    for node in g.node_references() {
        writeln!(
            f,
            "{}{}{} [label = \"{}\"]",
            INDENT,
            INDENT,
            g.to_index(node.id()) + 2,
            node.weight()
        )?;
    }
    writeln!(f, "{}}}", INDENT)?;
    // Render inputs and outputs outside of the subgraph
    for node in g.externals(Direction::Incoming) {
        let mut node_obj = g[node].clone();
        *node_obj.required.as_mut().unwrap() *=
            graph.recipes[node_obj.name.as_str()].results[0].rate;
        writeln!(
            f,
            "{}0 -> {} [label = \"{}\" dir=back]",
            INDENT,
            node.index() + 2,
            node_obj.trim()
        )?;
    }
    for node in g.externals(Direction::Outgoing) {
        let edges = graph.get_ingredients(
            &graph.recipes[g[node].name.as_str()],
            g[node].required.unwrap(),
            None,
        );
        if edges.is_empty() {
            let mut node_obj = g[node].clone();
            *node_obj.required.as_mut().unwrap() *=
                graph.recipes[node_obj.name.as_str()].results[0].rate;
            writeln!(
                f,
                "{}{} -> 1 [label = \"{}\" dir=back]",
                INDENT,
                node.index() + 2,
                node_obj.trim()
            )?;
        } else {
            for edge in edges {
                if !graph.imports.contains_key(&edge.item) {
                    writeln!(
                        f,
                        "{}{} -> 1 [label = \"{}\" dir=back]",
                        INDENT,
                        node.index() + 2,
                        edge
                    )?;
                }
            }
        }
    }
    for (import, nodes) in &graph.imports {
        for (node, required) in nodes {
            writeln!(
                f,
                "{}{} -> 1 [label = \"{} {}\" dir=back]",
                INDENT,
                g.to_index(*node) + 2,
                crate::round_string(*required),
                crate::trim(import)
            )?;
        }
    }
    for edge in g.edge_references() {
        writeln!(
            f,
            "{}{} -> {} [label = \"{}\" dir=back]",
            INDENT,
            g.to_index(edge.source()) + 2,
            g.to_index(edge.target()) + 2,
            edge.weight()
        )?;
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
