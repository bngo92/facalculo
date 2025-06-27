use crate::graph::{Edge, Graph, Import};
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

pub struct RenderArgs {
    imports: HashMap<String, Vec<(String, usize, Decimal)>>,
    used_imports: HashSet<String>,
    details: bool,
}

impl RenderArgs {
    pub fn new(
        imports: HashMap<String, Vec<(String, usize, Decimal)>>,
        used_imports: HashSet<String>,
        details: bool,
    ) -> Self {
        Self {
            imports,
            used_imports,
            details,
        }
    }
}

pub fn render(
    graphs: &[Graph],
    args: &RenderArgs,
    production: &mut [(String, Decimal)],
    structure_energy: &mut [(String, i32, Decimal)],
) -> Result<String, Box<dyn std::error::Error>> {
    let mut f = String::new();
    writeln!(f, "digraph {{")?;
    writeln!(f, "{INDENT}0 [label = \"outputs\"]")?;
    writeln!(f, "{INDENT}1 [label = \"inputs\"]")?;
    // Offset for output and input nodes
    let mut offset = 2;
    let mut offsets = HashMap::new();
    for graph in graphs {
        offsets.insert(graph.name.as_str(), offset);
        offset += render_module(
            &mut f,
            graph,
            INDENT,
            &offsets,
            args,
            !structure_energy.is_empty(),
        )?;
    }
    if !production.is_empty() {
        production.sort_by_key(|t| -t.1);
        let production: Vec<_> = production
            .iter_mut()
            .map(|t| format!("{} {}", crate::round_string(t.1), t.0))
            .collect();
        writeln!(
            f,
            "{INDENT}{} [label = \"{}\" shape=record]",
            offset,
            production.join("\\n")
        )?;
    }
    if !structure_energy.is_empty() {
        let total_energy: Decimal = structure_energy.iter().map(|t| t.2).sum();
        structure_energy.sort_by_key(|t| -t.2);
        let structure_energy: Vec<_> = structure_energy
            .iter_mut()
            .filter_map(|t| {
                if t.0.is_empty() || t.2.is_zero() {
                    None
                } else {
                    Some(format!("{} {} {} W", t.1, t.0, crate::round_string(t.2)))
                }
            })
            .collect();
        writeln!(
            f,
            "{INDENT}{} [label = \"{} W\\n{}\" shape=record]",
            offset + 1,
            crate::round_string(total_energy),
            structure_energy.join("\\n")
        )?;
    }
    writeln!(f, "}}")?;
    Ok(f)
}

fn render_module(
    f: &mut impl Write,
    graph: &Graph,
    indent: &str,
    offsets: &HashMap<&str, usize>,
    args: &RenderArgs,
    show_energy: bool,
) -> Result<usize, Box<dyn std::error::Error>> {
    let RenderArgs {
        imports,
        used_imports,
        details,
    } = args;
    let index = offsets[graph.name.as_str()];
    let mut additional_nodes = 0;
    let g = &graph.graph;
    writeln!(
        f,
        "{indent}subgraph cluster_{} {{",
        graph.name.replace('-', "_")
    )?;
    for node in g.node_references() {
        if node.weight().structure.is_some() {
            writeln!(
                f,
                "{indent}{}{} [label = \"{}\" shape=record]",
                INDENT,
                g.to_index(node.id()) + index,
                node.weight().to_string(*details)
            )?;
        } else {
            writeln!(
                f,
                "{indent}{}{} [label = \"{}\"]",
                INDENT,
                g.to_index(node.id()) + index,
                node.weight().to_string(*details)
            )?;
        }
    }
    if show_energy
        && g.node_weights()
            .map(|n| n.structure.is_some() as i32)
            .sum::<i32>()
            > 1
    {
        let total_energy: Decimal = graph.energy.iter().map(|t| t.1).sum();
        writeln!(
            f,
            "{indent}{indent}{} [label = \"{} W\"]",
            g.node_count() + index,
            crate::round_string(total_energy)
        )?;
        additional_nodes += 1;
    }
    writeln!(f, "{indent}}}")?;
    for (o, (node, required)) in &graph.outputs {
        if let Some(dependencies) = imports.get(o.as_str()) {
            for (import_module, import, required) in dependencies {
                writeln!(
                    f,
                    "{indent}{} -> {} [label = \"{}\" dir=back]",
                    offsets[import_module.as_str()] + import,
                    node.index() + index,
                    Edge {
                        item: (*o).to_owned(),
                        required: *required,
                    }
                )?;
            }
        } else {
            writeln!(
                f,
                "{indent}0 -> {} [label = \"{}\" dir=back]",
                node.index() + index,
                Edge {
                    item: (*o).to_owned(),
                    required: *required,
                }
            )?;
        }
    }
    for (import, node) in &graph.imports {
        let (node, required) = match node {
            Import::Resource(node, required) => (node, *required),
            Import::Node(node) if !used_imports.contains(import) => {
                (node, g[*node].required.unwrap())
            }
            Import::Import(node, required) if !used_imports.contains(import) => (node, *required),
            _ => continue,
        };
        writeln!(
            f,
            "{indent}{} -> 1 [label = \"{}\" dir=back]",
            node.index() + index,
            Edge {
                item: import.to_owned(),
                required,
            }
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
    Ok(g.node_count() + additional_nodes)
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
