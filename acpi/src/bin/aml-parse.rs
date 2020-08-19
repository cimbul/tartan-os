#![warn(clippy::pedantic)]

use std::env;
use std::error::Error;
use std::fmt;
use std::fs;
use std::path::Path;
use tartan_acpi::aml::parse::parse_table;


fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<_> = env::args().collect();
    if args.len() <= 1 {
        eprintln!("Usage: aml-parse INPUT");
        return Err(Box::new(UsageError));
    }

    let aml_path = Path::new(&args[1]);
    let aml_data = fs::read(aml_path)?;

    match parse_table(&aml_data) {
        Ok(t) => println!("Successfully parsed {}:\n{:#x?}", aml_path.display(), t),
        Err(e) => print!("Error parsing {}:\n\n{}", aml_path.display(), e),
    }

    Ok(())
}


#[derive(Debug)]
struct UsageError;

impl fmt::Display for UsageError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "Usage error")
    }
}

impl Error for UsageError {}
