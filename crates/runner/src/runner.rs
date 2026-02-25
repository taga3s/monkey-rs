use wasm_bindgen::prelude::*;

use evaluator::evaluator;
use lexer::lexer::Lexer;
use object::environment::Environment;
use parser::parser::Parser;

#[wasm_bindgen]
pub fn run(input: &str) -> Option<String> {
    let env = Environment::new();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    if !parser.errors().is_empty() {
        print_parse_errors(parser.errors());
        return None;
    }

    match evaluator::eval(&program, env) {
        Ok(result) => Some(result.inspect()),
        Err(err) => Some(format!("Error: {}", err)),
    }
}

fn print_parse_errors(errors: &[String]) {
    for msg in errors {
        eprintln!("Error: {}", msg);
    }
}
