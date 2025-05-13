use crate::module::{Edge, Graph};
use petgraph::{
    visit::{Dfs, EdgeRef, IntoEdgeReferences, IntoNodeReferences, NodeIndexable, NodeRef},
    Direction,
};
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

static INDENT: &str = "    ";

pub fn render(
    graphs: &[Graph],
    imports: &HashMap<String, Vec<(usize, Decimal)>>,
    used_imports: &HashSet<String>,
) -> Result<String, Box<dyn std::error::Error>> {
    let mut f = String::new();
    writeln!(f, "digraph {{")?;
    writeln!(f, "{INDENT}newrank=true")?;
    writeln!(f, "{INDENT}0 [label = \"outputs\"]")?;
    writeln!(f, "{INDENT}1 [label = \"inputs\"]")?;
    // Offset for output and input nodes
    let mut index = 2;
    for graph in graphs {
        index += render_module(&mut f, graph, INDENT, index, imports, used_imports)?;
    }
    writeln!(f, "}}")?;
    Ok(f)
}

fn render_module(
    f: &mut impl Write,
    graph: &Graph,
    indent: &str,
    index: usize,
    imports: &HashMap<String, Vec<(usize, Decimal)>>,
    used_imports: &HashSet<String>,
) -> Result<usize, Box<dyn std::error::Error>> {
    let g = &graph.graph;
    writeln!(
        f,
        "{indent}subgraph cluster_{} {{",
        graph.name.replace('-', "_")
    )?;
    writeln!(f, "{indent}{INDENT}node [shape=record]")?;
    for node in g.node_references() {
        writeln!(
            f,
            "{indent}{}{} [label = \"{}\"]",
            INDENT,
            g.to_index(node.id()) + index,
            node.weight()
        )?;
    }
    writeln!(f, "{indent}}}")?;
    for node in g.externals(Direction::Incoming) {
        let mut node_obj = g[node].clone();
        if let Some(dependencies) = imports.get(&node_obj.name) {
            for &(import, required) in dependencies {
                writeln!(
                    f,
                    "{indent}{} -> {} [label = \"{}\" dir=back]",
                    import,
                    node.index() + index,
                    Edge {
                        item: node_obj.name.clone(),
                        required,
                        belt: None
                    }
                )?;
            }
        } else {
            *node_obj.required.as_mut().unwrap() *=
                graph.recipes.get(node_obj.name.as_str()).unwrap().results[0].rate;
            writeln!(
                f,
                "{indent}0 -> {} [label = \"{}\" dir=back]",
                node.index() + index,
                node_obj.trim()
            )?;
        }
    }
    for node in g.externals(Direction::Outgoing) {
        let edges = graph.get_ingredients(
            graph.recipes.get(g[node].name.as_str()).unwrap(),
            g[node].required.unwrap(),
            None,
        );
        if edges.is_empty() {
            let mut node_obj = g[node].clone();
            *node_obj.required.as_mut().unwrap() *=
                graph.recipes.get(node_obj.name.as_str()).unwrap().results[0].rate;
            writeln!(
                f,
                "{indent}{} -> 1 [label = \"{}\" dir=back]",
                node.index() + index,
                node_obj.trim()
            )?;
        } else {
            for edge in edges {
                if !used_imports.contains(&edge.item) {
                    writeln!(
                        f,
                        "{indent}{} -> 1 [label = \"{}\" dir=back]",
                        node.index() + index,
                        edge
                    )?;
                }
            }
        }
    }
    for edge in g.edge_references() {
        writeln!(
            f,
            "{indent}{} -> {} [label = \"{}\" dir=back]",
            g.to_index(edge.source()) + index,
            g.to_index(edge.target()) + index,
            edge.weight()
        )?;
    }
    Ok(g.node_count())
}

pub fn total<'a>(graphs: &'a [Graph]) -> HashMap<&'a str, Decimal> {
    let mut total = HashMap::new();
    for graph in graphs {
        let Graph { graph, recipes, .. } = graph;
        let mut dfs = Dfs::new(graph, graph.externals(Direction::Incoming).next().unwrap());
        while let Some(nx) = dfs.next(&graph) {
            if let Some(recipe) = recipes.get(graph[nx].name.as_str()) {
                let ratio = graph[nx].required.unwrap() / recipe.results[0].rate;
                for i in &recipe.ingredients {
                    *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
                }
            }
        }
    }
    total
}
