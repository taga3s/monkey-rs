use evaluator::evaluator::Evaluator;
use utils::context::Context;
use wasm_bindgen::prelude::*;

use lexer::lexer::Lexer;
use parser::parser::Parser;

#[wasm_bindgen]
pub fn run(input: &str) -> String {
    let ctx = Context::new(input);
    let lexer = Lexer::new(&ctx);
    let mut parser = Parser::new(&ctx, lexer);
    let program = match parser.parse_program() {
        Ok(result) => result,
        Err(err) => return format!("Error: {}", err),
    };

    let evaluator = Evaluator::new();
    match evaluator.run(&ctx, &program) {
        Ok(result) => result.inspect(),
        Err(err) => format!("Error: {}", err),
    }
}
