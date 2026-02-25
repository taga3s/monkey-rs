use std::{env, fs::File, io::Read, path::Path};

use runner::runner::run;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: just run <FILE_PATH>");
        std::process::exit(1);
    }

    let file_path = &args[1];
    if !file_path.ends_with(".mon") {
        eprintln!("Error: The file must have a \".mon\" extension.");
        std::process::exit(1);
    }

    let path = Path::new(file_path);
    let mut file = File::open(path)?;

    let mut buf = String::new();
    file.read_to_string(&mut buf)?;

    if let Some(result) = run(&buf) {
        println!("{}", result);
    }

    Ok(())
}
