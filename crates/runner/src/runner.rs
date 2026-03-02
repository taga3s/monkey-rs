use evaluator::evaluator::Evaluator;
use wasm_bindgen::prelude::*;

use lexer::lexer::Lexer;
use parser::parser::Parser;

#[wasm_bindgen]
pub fn run(input: &str) -> String {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = match parser.parse_program() {
        Ok(result) => result,
        Err(err) => return format!("Error: {}", err),
    };

    let evaluator = Evaluator::new();
    match evaluator.eval(&program) {
        Ok(result) => result.inspect(),
        Err(err) => format!("Error: {}", err),
    }
}
