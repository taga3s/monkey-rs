use std::io::{self, Write};

use evaluator::evaluator::Evaluator;
use lexer::lexer::Lexer;
use parser::parser::Parser;

fn start() -> io::Result<()> {
    let mut input = String::new();

    loop {
        input.clear();

        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        let trimmed = input.trim();

        if trimmed.starts_with('/') {
            match trimmed {
                "/quit" => {
                    println!("Goodbye!");
                    break;
                }
                _ => {
                    println!("Unknown command: {}", trimmed);
                    continue;
                }
            }
        }

        if trimmed.is_empty() {
            continue;
        }

        let lexer = Lexer::new(trimmed);
        let mut parser = Parser::new(lexer);
        let program = match parser.parse_program() {
            Ok(program) => program,
            Err(err) => {
                eprintln!("{}", err);
                continue;
            }
        };
        let evaluator = Evaluator::new();

        match evaluator.eval(&program) {
            Ok(result) => {
                println!("{}", result.inspect());
            }
            Err(err) => {
                println!("{}", err);
            }
        }
    }

    Ok(())
}

fn main() {
    println!("Welcome to monkey-rs REPL! Feel free to type in commands.\nType /quit to exit.");
    if let Err(e) = start() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}
