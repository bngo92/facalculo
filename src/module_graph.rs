use crate::{
    data::RecipeRepository,
    graph::{Edge, Graph, Import},
    module::NamedModule,
};
use core::error::Error;
use petgraph::{
    Directed, algo,
    prelude::GraphMap,
    visit::{EdgeRef, IntoEdgeReferences, IntoNodeReferences, NodeIndexable, NodeRef},
};
use rust_decimal::Decimal;
use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

static INDENT: &str = "    ";

pub struct ModuleGraph<'a> {
    pub graphs: Vec<Graph<'a>>,
    pub imports: HashMap<String, Vec<(String, usize, Decimal)>>,
    pub used_imports: HashSet<String>,
    pub production: Vec<(String, Decimal)>,
    pub energy: Vec<(String, i32, Decimal)>,
}

impl ModuleGraph<'_> {
    pub fn build<'a>(
        recipe_rates: &'a RecipeRepository,
        modules: &'a HashMap<String, NamedModule>,
        required: &mut HashMap<String, Decimal>,
        production: Option<Decimal>,
        energy: bool,
        asm: i64,
    ) -> Result<ModuleGraph<'a>, Box<dyn Error>> {
        // Sort modules so outputs are processed before inputs
        let mut outputs: HashMap<_, Vec<_>> = HashMap::new();
        let mut active_set = HashSet::new();
        for (node, module) in modules {
            for output in recipe_rates.get_outputs(&module.module) {
                outputs.entry(output).or_default().push(node.as_str());
                if required.contains_key(output) {
                    active_set.insert(node.clone());
                }
            }
        }
        let mut graph = GraphMap::<&str, (), Directed>::new();
        for (node, module) in modules {
            graph.add_node(node.as_str());
            for input in recipe_rates.get_inputs(&module.module) {
                if let Some(export_nodes) = outputs.get(input) {
                    if let [export_node] = &**export_nodes {
                        graph.add_edge(node.as_str(), export_node, ());
                    } else {
                        // TODO: support multiple exports
                        return Err(format!("multiple modules are exporting {input}").into());
                    }
                }
            }
        }
        let module_order: Vec<_> = algo::toposort(&graph, None)
            .unwrap()
            .into_iter()
            .map(ToOwned::to_owned)
            .collect();
        let mut graphs = Vec::new();
        let mut imports: HashMap<String, Vec<(String, usize, Decimal)>> = HashMap::new();
        let mut total_production = HashMap::new();
        let mut structure_count = HashMap::new();
        let mut total_energy = HashMap::new();
        for module in module_order {
            if !active_set.contains(&module) {
                continue;
            }
            active_set.extend(graph.neighbors(&module).map(ToOwned::to_owned));
            let graph = Graph::from_module(modules[&module].clone(), required, recipe_rates, asm);
            for (import, node) in &graph.imports {
                // We do not create import nodes for science packs
                let (import_required, node) = match *node {
                    Import::Resource(..) => continue,
                    Import::Node(node) => (graph.graph[node].required.unwrap(), node),
                    Import::Import(node, required) => (required, node),
                };
                *required.entry(import.clone()).or_default() += import_required;
                imports.entry(import.clone()).or_default().push((
                    graph.name.clone(),
                    node.index(),
                    import_required,
                ));
            }
            if let Some(production) = production {
                for (item, rate) in &graph.production {
                    *total_production.entry(item.clone()).or_default() += production * rate;
                }
            }
            if energy {
                for (structure, count) in &graph.structures {
                    *structure_count.entry(structure.clone()).or_default() += count;
                }
                for (structure, energy) in &graph.energy {
                    *total_energy.entry(structure.clone()).or_default() += energy;
                }
            }
            graphs.push(graph);
        }
        Ok(ModuleGraph {
            graphs,
            imports,
            used_imports: outputs.into_keys().map(ToOwned::to_owned).collect(),
            production: total_production.into_iter().collect(),
            energy: structure_count
                .into_iter()
                .map(|t| {
                    let energy = total_energy[&t.0];
                    (t.0, t.1, energy)
                })
                .collect(),
        })
    }

    pub fn render(&mut self, details: bool) -> Result<String, Box<dyn Error>> {
        let mut f = String::new();
        writeln!(f, "digraph {{")?;
        writeln!(f, "{INDENT}0 [label = \"outputs\"]")?;
        writeln!(f, "{INDENT}1 [label = \"inputs\"]")?;
        // Offset for output and input nodes
        let mut offset = 2;
        let mut offsets = HashMap::new();
        for graph in &self.graphs {
            offsets.insert(graph.name.as_str(), offset);
            offset += self.render_module(
                &mut f,
                graph,
                INDENT,
                &offsets,
                details,
                !self.energy.is_empty(),
            )?;
        }

        for graph in &self.graphs {
            let index = offsets[graph.name.as_str()];
            // Connect output edges
            for (o, &(node, required)) in &graph.outputs {
                if let Some(dependencies) = self.imports.get(o.as_str()) {
                    // If another module depends on this module, connect the nodes
                    for (import_module, import, required) in dependencies {
                        writeln!(
                            f,
                            "{INDENT}{} -> {} [label = \"{}\" dir=back]",
                            offsets[import_module.as_str()] + import,
                            node.index() + index,
                            Edge {
                                item: (*o).clone(),
                                required: *required,
                            }
                        )?;
                    }
                } else {
                    // Otherwise export to output node
                    writeln!(
                        f,
                        "{INDENT}0 -> {} [label = \"{}\" dir=back]",
                        node.index() + index,
                        Edge {
                            item: (*o).clone(),
                            required,
                        }
                    )?;
                }
            }

            // Import from input node if there are no modules that are exporting the item
            for (import, node) in &graph.imports {
                let (node, required) = match *node {
                    Import::Resource(node, required) => (node, required),
                    Import::Node(node) if !self.used_imports.contains(import) => {
                        (node, graph.graph[node].required.unwrap())
                    }
                    Import::Import(node, required) if !self.used_imports.contains(import) => {
                        (node, required)
                    }
                    _ => continue,
                };
                writeln!(
                    f,
                    "{INDENT}{} -> 1 [label = \"{}\" dir=back]",
                    node.index() + index,
                    Edge {
                        item: import.to_owned(),
                        required,
                    }
                )?;
            }
        }

        if !self.production.is_empty() {
            self.production.sort_by_key(|t| -t.1);
            let production: Vec<_> = self
                .production
                .iter()
                .map(|t| format!("{} {}", crate::round_string(t.1), t.0))
                .collect();
            writeln!(
                f,
                "{INDENT}{} [label = \"{}\" shape=record]",
                offset,
                production.join("\\n")
            )?;
        }
        if !self.energy.is_empty() {
            let total_energy: Decimal = self.energy.iter().map(|t| t.2).sum();
            self.energy.sort_by_key(|t| -t.2);
            let structure_energy: Vec<_> = self
                .energy
                .iter()
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
        &self,
        f: &mut impl Write,
        graph: &Graph,
        indent: &str,
        offsets: &HashMap<&str, usize>,
        details: bool,
        show_energy: bool,
    ) -> Result<usize, Box<dyn Error>> {
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
                    node.weight().to_string(details)
                )?;
            } else {
                writeln!(
                    f,
                    "{indent}{}{} [label = \"{}\"]",
                    INDENT,
                    g.to_index(node.id()) + index,
                    node.weight().to_string(details)
                )?;
            }
        }
        if show_energy
            && g.node_weights()
                .map(|n| i32::from(n.structure.is_some()))
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
}
