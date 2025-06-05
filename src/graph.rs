use crate::{
    data::{Category, Rate, RecipeRate, RecipeRepository, RepositoryOption},
    module::{Module, NamedModule, Structure},
};
use nalgebra::{Matrix3, Matrix3x1};
use petgraph::{algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef, Direction};
use rust_decimal::{
    prelude::{FromPrimitive, ToPrimitive},
    Decimal,
};
use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

pub type GraphType = StableGraph<Node, Edge>;

pub struct Graph<'a> {
    pub name: String,
    pub graph: GraphType,
    pub recipes: &'a RecipeRepository,
    pub imports: HashMap<String, Import>,
    pub outputs: HashMap<String, NodeIndex>,
}

#[derive(Clone)]
pub enum Import {
    /// The importing module is grouping imports into import nodes
    Node(NodeIndex),

    /// Import is a raw resource that shouldn't be imported from other modules
    Resource(NodeIndex, Decimal),

    /// The node requires X items
    Import(NodeIndex, Decimal),
}

impl<'a> Graph<'a> {
    pub fn new(recipes: &'a RecipeRepository) -> Graph<'a> {
        Graph {
            name: String::new(),
            graph: GraphType::new(),
            recipes,
            imports: HashMap::new(),
            outputs: HashMap::new(),
        }
    }

    pub fn from_module(
        module: NamedModule,
        required: &HashMap<String, Decimal>,
        defaults: &HashMap<String, Result<Decimal, Decimal>>,
        recipes: &'a RecipeRepository,
        belt: Option<i64>,
    ) -> Graph<'a> {
        let mut graph = Graph::new(recipes);
        graph.name = module.name.clone();
        match module.module {
            Module::User { .. } => {
                let outputs: Vec<_> = recipes
                    .get_outputs(&module.module)
                    .iter()
                    .filter_map(|o| recipes.get(o).map(|r| (*o, r)))
                    .collect();
                for (item, recipe) in outputs {
                    graph.build_module_node(
                        &module.module,
                        item,
                        *required.get(item).unwrap_or_else(|| match &defaults[item] {
                            Ok(rate) => rate,
                            Err(rate) => {
                                eprintln!(
                                    "Using {} for {item} (1 assembler)",
                                    crate::round_string(*rate)
                                );
                                rate
                            }
                        }),
                        recipe,
                        belt,
                    );
                }
            }
            Module::AdvancedOilProcessing {} => {
                let required: HashMap<_, _> = ["heavy-oil", "light-oil", "petroleum-gas"]
                    .into_iter()
                    .map(|p| {
                        (
                            p,
                            required.get(p).and_then(|d| d.to_f64()).unwrap_or_default(),
                        )
                    })
                    .collect();
                let a = Matrix3::from_iterator(vec![5., 9., 11., -20., 15., 0., 0., -15., 10.]);
                let advanced = Matrix3x1::new(
                    required["heavy-oil"],
                    required["light-oil"],
                    required["petroleum-gas"],
                );
                let solution = a.lu().solve(&advanced).unwrap();
                let [advanced_oil_processing, heavy_oil_cracking, light_oil_cracking] =
                    solution.as_slice()
                else {
                    unimplemented!()
                };
                let mut last: Option<(NodeIndex, &str)> = None;
                for (output, recipe, ratio) in [
                    (
                        "heavy-oil",
                        recipes.get("advanced-oil-processing").unwrap().rate(),
                        advanced_oil_processing,
                    ),
                    (
                        "light-oil",
                        recipes.get("heavy-oil-cracking").unwrap().rate(),
                        heavy_oil_cracking,
                    ),
                    (
                        "petroleum-gas",
                        recipes.get("light-oil-cracking").unwrap().rate(),
                        light_oil_cracking,
                    ),
                ] {
                    let ratio = Decimal::from_f64(*ratio).unwrap();
                    let node = graph.graph.add_node(Node {
                        required: Some(ratio),
                        name: recipe.key.to_owned(),
                        structure: true,
                    });
                    if required[output] > 0. {
                        graph.outputs.insert(output.to_owned(), node);
                    }
                    for edge in graph.get_ingredients(recipe, ratio, belt) {
                        match last {
                            Some((last, item)) if edge.item == *item => {
                                graph.graph.add_edge(node, last, edge);
                            }
                            _ => {
                                let n = graph.imports.entry(edge.item.to_owned()).or_insert_with(
                                    || {
                                        Import::Node(graph.graph.add_node(Node {
                                            required: Some(Decimal::ZERO),
                                            name: edge.item.to_owned(),
                                            structure: false,
                                        }))
                                    },
                                );
                                let Import::Node(n) = n else { unreachable!() };
                                if let Some(required) = graph.graph[*n].required.as_mut() {
                                    *required += edge.required
                                }
                                graph.graph.add_edge(node, *n, edge);
                            }
                        }
                    }
                    last = Some((node, &recipe.results[0].name));
                }
            }
            Module::Science {} => {
                let required = required
                    .get("science")
                    .copied()
                    .unwrap_or_else(|| match &defaults["science"] {
                        Ok(rate) => *rate,
                        Err(_) => todo!(),
                    });
                let node = graph.graph.add_node(Node {
                    required: Some(required / recipes.science_recipe.results[0].rate),
                    name: "science".to_owned(),
                    structure: true,
                });
                for science in [
                    "automation-science-pack",
                    "logistic-science-pack",
                    "military-science-pack",
                    "chemical-science-pack",
                    "production-science-pack",
                    "utility-science-pack",
                    "space-science-pack",
                ] {
                    graph
                        .imports
                        .insert(science.to_owned(), Import::Import(node, required));
                }
                graph.outputs.insert("science".to_owned(), node);
            }
        }
        graph
    }

    fn build_module_node(
        &mut self,
        module: &Module,
        ingredient: &str,
        required: Decimal,
        recipe: Rate,
        belt: Option<i64>,
    ) -> NodeIndex {
        let ratio = required
            / recipe
                .rate()
                .results
                .iter()
                .find(|i| i.name == ingredient)
                .unwrap()
                .rate;
        let node = self.graph.add_node(Node {
            required: Some(ratio),
            name: recipe.rate().key.to_owned(),
            structure: true,
        });
        if let Rate::Resource(_) = recipe {
            self.imports
                .insert(ingredient.to_owned(), Import::Resource(node, required));
        }
        for result in &recipe.rate().results {
            if self
                .recipes
                .get_outputs(module)
                .contains(result.name.as_str())
            {
                self.outputs.insert(result.name.to_owned(), node);
            }
        }
        for edge in self.get_ingredients(recipe.rate(), ratio, belt) {
            if self.recipes.get_inputs(module).contains(edge.item.as_str()) {
                let n = self.imports.entry(edge.item.to_owned()).or_insert_with(|| {
                    Import::Node(self.graph.add_node(Node {
                        required: Some(Decimal::ZERO),
                        name: edge.item.to_owned(),
                        structure: false,
                    }))
                });
                let Import::Node(n) = n else { unreachable!() };
                if let Some(required) = self.graph[*n].required.as_mut() {
                    *required += edge.required;
                }
                self.graph.add_edge(node, *n, edge);
            } else {
                let recipe = match self.recipes.get_options(&edge.item) {
                    RepositoryOption::None => continue,
                    RepositoryOption::Some(recipe) => recipe,
                    RepositoryOption::Multiple(recipes) => {
                        let recipes: HashSet<_> = recipes.iter().collect();
                        let Module::User { structures } = module else {
                            continue;
                        };
                        let recipe = structures
                            .iter()
                            .map(|s| match s {
                                Structure::Recipe(r) => &r.name,
                                Structure::Resource(r) => &r.name,
                            })
                            .find(|r| recipes.contains(r))
                            .unwrap();
                        self.recipes.get(recipe).unwrap()
                    }
                };
                let n = self.build_module_node(module, &edge.item, edge.required, recipe, belt);
                self.graph.add_edge(node, n, edge);
            }
        }
        node
    }

    pub fn get_ingredients(
        &self,
        recipe: &RecipeRate,
        ratio: Decimal,
        belt: Option<i64>,
    ) -> Vec<Edge> {
        recipe
            .ingredients
            .iter()
            .map(move |i| {
                let belt = if let None | Some(Category::OilProcessing) =
                    self.recipes.get(i.name.as_str()).and_then(|r| {
                        if let Rate::Recipe(r) = r {
                            r.category
                        } else {
                            None
                        }
                    }) {
                    None
                } else {
                    belt
                };
                Edge {
                    required: ratio * i.rate,
                    belt,
                    item: i.name.to_owned(),
                }
            })
            .collect()
    }

    // TODO: we should not expand group nodes
    pub fn group_nodes(&mut self, items: Vec<String>) {
        // Group items and their dependencies
        // Select one node for each item
        let mut selected_nodes: HashMap<_, NodeIndex> = HashMap::new();
        let nodes = algo::toposort(&self.graph, None).expect("graph should be directed");
        for node in nodes.iter().copied() {
            if !self.graph.contains_node(node) {
                continue;
            }
            if !items.is_empty() && !items.contains(&self.graph[node].name) {
                continue;
            }
            let Some(selected_node) = selected_nodes.get(&self.graph[node].name).copied() else {
                selected_nodes.insert(self.graph[node].name.to_owned(), node);
                continue;
            };

            // Move incoming edges to selected node
            //
            // Suppose we are grouping b
            //
            // a1 a2
            //  |  |
            // b1 b2
            //  |  |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            // b1 b2
            //  |  |
            // c1 c2
            let edges: Vec<_> = self
                .graph
                .edges_directed(node, Direction::Incoming)
                .map(|e| (e.source(), e.id(), e.weight().clone()))
                .collect();
            for (source, id, edge) in edges {
                self.graph.add_edge(source, selected_node, edge);
                self.graph.remove_edge(id);
            }

            // Merge subgraphs with BFS
            // a1 a2
            //   \ |
            // b1 b2
            //  |  |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            // b1 b2
            //     |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            //    b2
            //     |
            // c1 c2
            //
            // becomes
            //
            // a1 a2
            //   \ |
            //    b2
            //     |
            //    c2
            let mut node_bfs = VecDeque::from_iter([node]);
            let mut selected_bfs = VecDeque::from_iter([selected_node]);
            while let Some(node) = node_bfs.pop_front() {
                let mut edges: Vec<_> = self
                    .graph
                    .edges_directed(node, Direction::Outgoing)
                    .map(|e| (e.target(), e.id(), e.weight().clone()))
                    .collect();
                edges.sort_by_key(|(target, _, _)| &self.graph[*target].name);
                let selected_node = selected_bfs.pop_front().unwrap();
                let mut selected_edges: Vec<_> = self
                    .graph
                    .edges_directed(selected_node, Direction::Outgoing)
                    .map(|e| (e.target(), e.id()))
                    .collect();
                selected_edges.sort_by_key(|(target, _)| &self.graph[*target].name);
                for (edge, selected_edge) in edges.into_iter().zip(selected_edges.into_iter()) {
                    self.graph[selected_edge.1].required += edge.2.required;
                    self.graph.remove_edge(edge.1);
                    node_bfs.push_back(edge.0);
                    selected_bfs.push_back(selected_edge.0);
                }
                // The input node does not have required
                if let Some(required) = self.graph[node].required {
                    *self.graph[selected_node].required.as_mut().unwrap() += required;
                    self.graph.remove_node(node);
                }
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Node {
    pub required: Option<Decimal>,
    pub name: String,
    pub structure: bool,
}

impl Node {
    pub fn trim(&self) -> String {
        if let Some(required) = self.required {
            format!("{} {}", crate::round_string(required), trim(&self.name))
        } else {
            self.name.clone()
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(required) = self.required {
            write!(f, "{} {}", crate::round_string(required), self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Edge {
    pub required: Decimal,
    pub belt: Option<i64>,
    pub item: String,
}

impl Display for Edge {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {}",
            crate::round_string(self.required),
            trim(&self.item)
        )?;
        if let Some(belt) = self.belt {
            write!(
                f,
                " ({})",
                crate::round_string(self.required / Decimal::from(belt))
            )?;
        }
        Ok(())
    }
}

fn trim(s: &str) -> String {
    String::from_iter(
        s.split('-')
            .map(|s| s.chars().next().unwrap())
            .collect::<Vec<_>>(),
    )
}
