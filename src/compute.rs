use crate::graph::{Edge, Graph};
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
    for node in g.node_references() {
        if node.weight().structure {
            writeln!(
                f,
                "{indent}{}{} [label = \"{}\" shape=record]",
                INDENT,
                g.to_index(node.id()) + index,
                node.weight()
            )?;
        } else {
            writeln!(
                f,
                "{indent}{}{} [label = \"{}\"]",
                INDENT,
                g.to_index(node.id()) + index,
                node.weight()
            )?;
        }
    }
    writeln!(f, "{indent}}}")?;
    for (o, node) in &graph.outputs {
        if let Some(dependencies) = imports.get(o.as_str()) {
            for &(import, required) in dependencies {
                writeln!(
                    f,
                    "{indent}{} -> {} [label = \"{}\" dir=back]",
                    import,
                    node.index() + index,
                    Edge {
                        item: (*o).to_owned(),
                        required,
                        belt: None
                    }
                )?;
            }
        } else {
            let mut node_obj = g[*node].clone();
            *node_obj.required.as_mut().unwrap() *= graph
                .recipes
                .get(node_obj.name.as_str())
                .unwrap()
                .rate()
                .results[0]
                .rate;
            writeln!(
                f,
                "{indent}0 -> {} [label = \"{}\" dir=back]",
                node.index() + index,
                node_obj.trim()
            )?;
        }
    }
    for (import, node) in &graph.imports {
        if !used_imports.contains(import) {
            writeln!(
                f,
                "{indent}{} -> 1 [label = \"{}\" dir=back]",
                node.index() + index,
                g[*node].trim()
            )?;
        }
    }
    for node in graph.resource_imports.values() {
        let mut node_obj = g[*node].clone();
        // Convert number of resources structures back to the resource rate
        *node_obj.required.as_mut().unwrap() *= graph
            .recipes
            .get(node_obj.name.as_str())
            .unwrap()
            .rate()
            .results[0]
            .rate;
        writeln!(
            f,
            "{indent}{} -> 1 [label = \"{}\" dir=back]",
            node.index() + index,
            node_obj.trim()
        )?;
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
                let ratio = graph[nx].required.unwrap() / recipe.rate().results[0].rate;
                for i in &recipe.rate().ingredients {
                    *total.entry(i.name.as_str()).or_insert(Decimal::ZERO) += ratio * i.rate;
                }
            }
        }
    }
    total
}
