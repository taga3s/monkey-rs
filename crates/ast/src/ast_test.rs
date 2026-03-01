use token::token::{Literal, Token, TokenType};
use utils::context::Context;

use crate::ast::{Expression, Identifier, LetStatement, Node, Program, Statement};

#[test]
fn test_to_string() {
    let ctx = Context::new("let myVar = anotherVar;");
    let program = Program {
        ctx: &ctx,
        statements: vec![Node::Statement(Statement::Let(LetStatement {
            ctx: &ctx,
            token: Token {
                ty: TokenType::LET,
                literal: Literal::new(0, 3),
            },
            name: Some(Identifier {
                ctx: &ctx,
                token: Token {
                    ty: TokenType::IDENT,
                    literal: Literal::new(4, 9),
                },
                value: Literal::new(4, 9),
            }),
            value: Some(Box::new(Node::Expression(Expression::Identifier(
                Identifier {
                    ctx: &ctx,
                    token: Token {
                        ty: TokenType::IDENT,
                        literal: Literal::new(12, 22),
                    },
                    value: Literal::new(12, 22),
                },
            )))),
        }))],
    };

    if program.to_string() != "let myVar = anotherVar;" {
        panic!("program.to_string() wrong. got={}", program);
    }
}
