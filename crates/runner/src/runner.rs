use wasm_bindgen::prelude::*;

use evaluator::evaluator;
use lexer::lexer::Lexer;
use object::environment::Environment;
use parser::parser::Parser;

#[wasm_bindgen]
pub fn run(input: &str) -> String {
    let env = Environment::new();

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(result) => result,
        Err(err) => return format!("Error: {}", err),
    };

    match evaluator::eval(&program, env) {
        Ok(result) => result.inspect(),
        Err(err) => format!("Error: {}", err),
    }
}
