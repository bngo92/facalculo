use std::fs;

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
            &["copper-ore"].into_iter().map(ToOwned::to_owned).collect(),
            &recipe_rates,
        )?)?,
    )?;
    Ok(())
}
