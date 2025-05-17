use std::{collections::HashSet, fs};

use facalculo::data::{self, Data};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let b = include_bytes!("../data-raw-dump.json");
    let data: Data = serde_json::from_slice(b)?;
    let recipe_rates = data::calculate_rates(&data, 2);
    fs::write(
        "examples/copper-ore.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "copper-ore",
            true,
            &[],
            &HashSet::new(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/copper-plate.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "copper-plate",
            true,
            &["copper-ore"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>(),
            &HashSet::new(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/iron-ore.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "iron-ore",
            true,
            &[],
            &HashSet::new(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/iron-plate.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "iron-plate",
            true,
            &["iron-ore"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>(),
            &HashSet::new(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/automation-science-pack.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "automation-science-pack",
            true,
            &["copper-plate", "iron-plate"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>(),
            &["iron-gear-wheel"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/electronic-circuit.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "electronic-circuit",
            true,
            &["copper-plate", "iron-plate"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>(),
            &["copper-cable"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect(),
            &recipe_rates,
        )?)?,
    )?;
    fs::write(
        "examples/logistic-science-pack.json",
        serde_json::to_string_pretty(&facalculo::generate(
            "logistic-science-pack",
            true,
            &["iron-plate", "electronic-circuit"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect::<Vec<_>>(),
            &["iron-gear-wheel"]
                .into_iter()
                .map(ToOwned::to_owned)
                .collect(),
            &recipe_rates,
        )?)?,
    )?;
    Ok(())
}
