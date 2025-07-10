use crate::{
    data::{Effect, IngredientRate, Rate, RecipeRate, RecipeRepository},
    module::{Module, NamedModule, Structure},
};
use core::fmt;
use nalgebra::{Matrix3, Matrix3x1};
use petgraph::{Direction, algo, graph::NodeIndex, stable_graph::StableGraph, visit::EdgeRef};
use rust_decimal::{
    Decimal,
    prelude::{FromPrimitive, ToPrimitive},
};
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

pub type GraphType = StableGraph<Node, Edge>;
type OutputInfo<'a> = &'a HashMap<&'a str, (Rate<Cow<'a, RecipeRate>>, String, Decimal, Effect)>;

pub struct Graph<'a> {
    pub name: String,
    pub graph: GraphType,
    pub recipes: &'a RecipeRepository,
    pub imports: HashMap<String, Import>,
    pub outputs: HashMap<String, (NodeIndex, Decimal)>,
    pub production: HashMap<String, Decimal>,
    pub structures: HashMap<String, i32>,
    pub energy: HashMap<String, Decimal>,
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
    pub fn new(recipes: &'a RecipeRepository) -> Self {
        Graph {
            name: String::new(),
            graph: GraphType::new(),
            recipes,
            imports: HashMap::new(),
            outputs: HashMap::new(),
            production: HashMap::new(),
            structures: HashMap::new(),
            energy: HashMap::new(),
        }
    }

    pub fn from_module(
        module: NamedModule,
        required: &HashMap<String, Decimal>,
        recipes: &'a RecipeRepository,
        asm: i64,
    ) -> Self {
        let mut graph = Graph::new(recipes);
        graph.name.clone_from(&module.name);
        match module.module {
            Module::User { structures } => {
                let mut inputs = HashSet::new();
                let mut outputs = HashMap::new();
                for structure in &structures {
                    match structure {
                        Structure::Recipe { name, modules } => {
                            let recipe = recipes.get(name).unwrap();
                            inputs
                                .extend(recipe.rate().ingredients.iter().map(|i| i.name.as_str()));
                            let effect = Effect {
                                productivity: modules
                                    .iter()
                                    .map(|(m, c)| {
                                        recipes.modules[m].effect.productivity * Decimal::from(*c)
                                    })
                                    .sum(),
                                speed: modules
                                    .iter()
                                    .map(|(m, c)| {
                                        recipes.modules[m].effect.speed * Decimal::from(*c)
                                    })
                                    .sum(),
                                consumption: modules
                                    .iter()
                                    .map(|(m, c)| {
                                        recipes.modules[m].effect.consumption * Decimal::from(*c)
                                    })
                                    .sum(),
                                pollution: Decimal::ZERO,
                            };
                            for result in &recipe.rate().results {
                                let structure = recipe.rate().structure(asm).to_owned();
                                let assembling_machine =
                                    &recipes.assembling_machines[structure.as_str()];
                                outputs.insert(
                                    result.name.as_str(),
                                    (
                                        recipe.to_cow(),
                                        structure,
                                        assembling_machine.crafting_speed.unwrap_or(Decimal::ONE),
                                        effect,
                                    ),
                                );
                            }
                        }
                        Structure::Resource { name, structure } => {
                            let mut recipe = recipes.resources[name].clone();
                            let structure = structure
                                .clone()
                                .unwrap_or_else(|| recipe.structure(asm).to_owned());
                            let mining_drill = &recipes.mining_drills[structure.as_str()];
                            if let Some(drain) = mining_drill.resource_drain_rate_percent {
                                for ingredient in &mut recipe.ingredients {
                                    ingredient.rate *= drain / Decimal::ONE_HUNDRED;
                                }
                            }
                            outputs.insert(
                                name.as_str(),
                                (
                                    Rate::Resource(Cow::Owned(recipe)),
                                    structure,
                                    mining_drill.mining_speed,
                                    Effect::default(),
                                ),
                            );
                        }
                    }
                }
                let output_set: HashSet<_> = outputs.keys().copied().collect();
                let exports: Vec<_> = output_set.difference(&inputs).copied().collect();
                for item in &exports {
                    if let Some(required) = required.get(*item) {
                        graph.build_module_node(
                            &outputs,
                            item,
                            *required,
                            &output_set,
                            &exports,
                            true,
                        );
                    }
                }
            }
            Module::AdvancedOilProcessing {} => {
                let process_recipes = [
                    ("heavy-oil", "advanced-oil-processing"),
                    ("light-oil", "heavy-oil-cracking"),
                    ("petroleum-gas", "light-oil-cracking"),
                ];
                let required: HashMap<_, _> = process_recipes
                    .into_iter()
                    .map(|(p, _)| {
                        (
                            p,
                            required
                                .get(p)
                                .and_then(ToPrimitive::to_f64)
                                .unwrap_or_default(),
                        )
                    })
                    .collect();
                let a = Matrix3::from_iterator(vec![5., 9., 11., -20., 15., 0., 0., -15., 10.]);
                let advanced =
                    Matrix3x1::from_iterator(process_recipes.iter().map(|(r, _)| required[r]));
                let solution = a.lu().solve(&advanced).unwrap();
                let mut last: Option<(NodeIndex, &str)> = None;
                for (i, (output, recipe)) in process_recipes.into_iter().enumerate() {
                    let recipe = recipes.get(recipe).unwrap();
                    let recipe = recipe.rate();
                    let ratio = Decimal::from_f64(solution[i]).unwrap();
                    let energy =
                        ratio * recipes.assembling_machines[recipe.structure(asm)].energy();
                    let node = graph.graph.add_node(Node {
                        required: Some(ratio),
                        name: recipe.key.clone(),
                        structure: Some(recipe.structure(asm).to_owned()),
                        energy,
                    });
                    graph.production = required
                        .iter()
                        .map(|(i, r)| ((*i).to_owned(), Decimal::from_f64(*r).unwrap()))
                        .collect();
                    *graph
                        .structures
                        .entry(recipe.structure(asm).to_owned())
                        .or_default() += ratio.ceil().to_i32().unwrap();
                    *graph
                        .energy
                        .entry(recipe.structure(asm).to_owned())
                        .or_default() += energy;
                    if required[output] > 0. {
                        graph.outputs.insert(
                            output.to_owned(),
                            (node, Decimal::from_f64(required[output]).unwrap()),
                        );
                    }
                    for edge in get_ingredients(recipe, ratio, Decimal::ZERO) {
                        match last {
                            Some((last, item)) if edge.item == *item => {
                                graph.graph.add_edge(node, last, edge);
                            }
                            _ => {
                                let n =
                                    graph.imports.entry(edge.item.clone()).or_insert_with(|| {
                                        Import::Node(graph.graph.add_node(Node {
                                            required: Some(Decimal::ZERO),
                                            name: edge.item.clone(),
                                            structure: None,
                                            energy: Decimal::ZERO,
                                        }))
                                    });
                                let Import::Node(n) = *n else { unreachable!() };
                                if let Some(required) = graph.graph[n].required.as_mut() {
                                    *required += edge.required;
                                }
                                graph.graph.add_edge(node, n, edge);
                            }
                        }
                    }
                    last = Some((node, &recipe.results[0].name));
                }
            }
            Module::Science {
                modules,
                research_speed,
                research_time,
            } => {
                let required = required["science"];
                let effect = Effect {
                    productivity: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.productivity * Decimal::from(*c))
                        .sum(),
                    speed: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.speed * Decimal::from(*c))
                        .sum(),
                    consumption: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.consumption * Decimal::from(*c))
                        .sum(),
                    pollution: Decimal::ZERO,
                };
                let science_rate = (Decimal::ONE
                    + research_speed.unwrap_or(Decimal::from_f64(2.5).unwrap()))
                    / research_time.unwrap_or(Decimal::from(60));
                let science_recipe = RecipeRate {
                    category: None,
                    key: "science".to_owned(),
                    ingredients: [
                        "automation-science-pack",
                        "logistic-science-pack",
                        "military-science-pack",
                        "chemical-science-pack",
                        "production-science-pack",
                        "utility-science-pack",
                        "space-science-pack",
                    ]
                    .into_iter()
                    .map(|i| IngredientRate {
                        rate: science_rate,
                        name: i.to_owned(),
                    })
                    .collect(),
                    results: vec![IngredientRate {
                        rate: science_rate,
                        name: "science".to_owned(),
                    }],
                };
                graph.build_module_node(
                    &HashMap::from([(
                        "science",
                        (
                            Rate::Recipe(Cow::Borrowed(&science_recipe)),
                            science_recipe.structure(asm).to_owned(),
                            Decimal::ONE,
                            effect,
                        ),
                    )]),
                    "science",
                    required,
                    &HashSet::new(),
                    &["science"],
                    false,
                );
            }
            Module::RocketSilo { modules } => {
                let required = required["rocket"];
                let effect = Effect {
                    productivity: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.productivity * Decimal::from(*c))
                        .sum(),
                    speed: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.speed * Decimal::from(*c))
                        .sum(),
                    consumption: modules
                        .iter()
                        .map(|(m, c)| recipes.modules[m].effect.consumption * Decimal::from(*c))
                        .sum(),
                    pollution: Decimal::ZERO,
                };
                let rocket_recipe = RecipeRate {
                    category: None,
                    key: "rocket".to_owned(),
                    ingredients: vec![IngredientRate {
                        rate: Decimal::from(50),
                        name: "rocket-part".to_owned(),
                    }],
                    results: vec![IngredientRate {
                        rate: Decimal::ONE,
                        name: "rocket".to_owned(),
                    }],
                };
                let mut outputs = HashMap::from([(
                    "rocket",
                    (
                        Rate::Recipe(Cow::Borrowed(&rocket_recipe)),
                        rocket_recipe.structure(asm).to_owned(),
                        Decimal::ONE,
                        Effect::default(),
                    ),
                )]);
                let recipe = recipes.get("rocket-part").unwrap();
                for result in &recipe.rate().results {
                    outputs.insert(
                        result.name.as_str(),
                        (
                            recipe.to_cow(),
                            recipe.rate().structure(asm).to_owned(),
                            Decimal::ONE,
                            effect,
                        ),
                    );
                }
                graph.build_module_node(
                    &outputs,
                    "rocket",
                    required,
                    &HashSet::from(["rocket-part"]),
                    &["rocket"],
                    false,
                );
            }
        }
        graph
    }

    fn build_module_node(
        &mut self,
        outputs: OutputInfo,
        ingredient: &str,
        required: Decimal,
        output_set: &HashSet<&str>,
        exports: &[&str],
        import_nodes: bool,
    ) -> NodeIndex {
        let (recipe, structure, speed, effect) = &outputs[ingredient];
        let ratio = required
            / recipe
                .rate()
                .results
                .iter()
                .find(|i| i.name == ingredient)
                .unwrap()
                .rate;
        let (productivity, energy_per_structure) = if structure.is_empty() {
            (effect.productivity, Decimal::ZERO)
        } else {
            let structure = &self.recipes.assembling_machines[structure];
            (
                effect.productivity
                    + structure
                        .effect_receiver
                        .as_ref()
                        .and_then(|e| e.base_effect.get("productivity"))
                        .unwrap_or(&Decimal::ZERO),
                structure.energy() * (Decimal::ONE + effect.consumption),
            )
        };
        let structures =
            ratio / speed / (Decimal::ONE + productivity) / (Decimal::ONE + effect.speed);
        let energy = structures * energy_per_structure;
        let node = self.graph.add_node(Node {
            required: Some(structures),
            name: recipe.rate().key.clone(),
            structure: Some(structure.clone()),
            energy,
        });
        *self.production.entry(ingredient.to_owned()).or_default() += required;
        *self.structures.entry(structure.clone()).or_default() +=
            structures.ceil().to_i32().unwrap();
        *self.energy.entry(structure.clone()).or_default() += energy;
        for result in &recipe.rate().results {
            if exports.contains(&result.name.as_str()) {
                self.outputs
                    .insert(result.name.clone(), (node, ratio * result.rate));
            }
        }
        if let Rate::Resource(recipe) = recipe {
            self.imports.insert(
                ingredient.to_owned(),
                Import::Resource(node, ratio * recipe.ingredients[0].rate),
            );
            return node;
        }
        for edge in get_ingredients(recipe.rate(), ratio, productivity) {
            if output_set.contains(edge.item.as_str()) {
                let n = self.build_module_node(
                    outputs,
                    &edge.item,
                    edge.required,
                    output_set,
                    exports,
                    import_nodes,
                );
                self.graph.add_edge(node, n, edge);
            } else if import_nodes {
                let n = self.imports.entry(edge.item.clone()).or_insert_with(|| {
                    Import::Node(self.graph.add_node(Node {
                        required: Some(Decimal::ZERO),
                        name: edge.item.clone(),
                        structure: None,
                        energy: Decimal::ZERO,
                    }))
                });
                let Import::Node(n) = *n else { unreachable!() };
                if let Some(required) = self.graph[n].required.as_mut() {
                    *required += edge.required;
                }
                self.graph.add_edge(node, n, edge);
            } else {
                self.imports
                    .insert(edge.item.clone(), Import::Import(node, edge.required));
            }
        }
        node
    }

    // TODO: we should not expand group nodes
    pub fn group_nodes(&mut self, items: &[String]) {
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
                selected_nodes.insert(self.graph[node].name.clone(), node);
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
                edges.sort_by_key(|&(target, _, _)| &self.graph[target].name);
                let selected_node = selected_bfs.pop_front().unwrap();
                let mut selected_edges: Vec<_> = self
                    .graph
                    .edges_directed(selected_node, Direction::Outgoing)
                    .map(|e| (e.target(), e.id()))
                    .collect();
                selected_edges.sort_by_key(|&(target, _)| &self.graph[target].name);
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Node {
    pub required: Option<Decimal>,
    pub name: String,
    pub structure: Option<String>,
    pub energy: Decimal,
}

impl Node {
    pub fn trim(&self) -> String {
        if let Some(required) = self.required {
            format!("{} {}", crate::round_string(required), trim(&self.name))
        } else {
            self.name.clone()
        }
    }

    pub fn to_string(&self, details: bool) -> String {
        let s = if let Some(required) = self.required {
            format!("{} {}", crate::round_string(required), self.name)
        } else {
            self.name.clone()
        };
        if let (true, Some(structure)) = (details, &self.structure) {
            let energy = if self.energy == Decimal::ZERO {
                String::new()
            } else {
                format!("\\n{} W", crate::round_string(self.energy))
            };
            format!("{s}\\n{structure}{energy}")
        } else {
            s
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string(false))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Edge {
    pub required: Decimal,
    pub item: String,
}

impl Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}",
            crate::round_string(self.required),
            trim(&self.item)
        )
    }
}

fn get_ingredients(recipe: &RecipeRate, ratio: Decimal, productivity: Decimal) -> Vec<Edge> {
    recipe
        .ingredients
        .iter()
        .map(move |i| Edge {
            required: ratio * i.rate / (Decimal::ONE + productivity),
            item: i.name.clone(),
        })
        .collect()
}

fn trim(s: &str) -> String {
    s.split('-').map(|s| s.chars().next().unwrap()).collect()
}
