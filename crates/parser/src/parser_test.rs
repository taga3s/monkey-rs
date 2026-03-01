use std::vec;

use ast::ast::{Expression, Node, Program, Statement, TNode};
use lexer::lexer::Lexer;
use utils::{context::Context, test::TestLiteral};

use crate::parser::Parser;

//-- Test helpers --//
fn test_parse_program<'a>(ctx: &'a Context<'a>) -> Program<'a> {
    let lexer = Lexer::new(ctx);
    let mut parser = Parser::new(&ctx, lexer);

    let program = match parser.parse_program() {
        Ok(Node::Program(p)) => p,
        Err(err) => {
            panic!(
                "parser.parse_program() did not return Program. reason: {}",
                err
            );
        }
        _ => panic!("parser.parse_program() did not return Program"),
    };
    check_parser_errors(&parser);
    program
}

fn check_parsed_program_len(program: &Program, expected_len: usize) {
    if program.statements.len() != expected_len {
        panic!(
            "program.statements does not contain {} statements. got={}",
            expected_len,
            program.statements.len()
        );
    }
}

fn test_literal_expression(ctx: &Context, exp: &Expression, expected: &TestLiteral) -> bool {
    match expected {
        TestLiteral::Int(value) => test_integer_literal(ctx, exp, value),
        TestLiteral::Str(value) => test_identifier(ctx, exp, value),
        TestLiteral::Bool(value) => test_boolean_literal(ctx, exp, value),
        _ => {
            panic!("type of exp not handled. got={:?}", exp);
        }
    }
}

fn test_integer_literal(ctx: &Context, il: &Expression, value: &i64) -> bool {
    let integer = match il {
        Expression::IntegerLiteral(il) => il,
        _ => {
            panic!("il is not IntegerLiteral.");
        }
    };

    if integer.value != *value {
        panic!("integer.value is not {}. got={}", value, integer.value);
    }

    let literal_ref = integer.token_literal().with_ref(ctx).reference;
    if literal_ref != value.to_string() {
        panic!(
            "integer.token_literal() is not {}. got={}",
            value, literal_ref
        );
    }

    true
}

fn test_identifier(ctx: &Context, ident: &Expression, value: &str) -> bool {
    let ident = match ident {
        Expression::Identifier(ident) => ident,
        _ => {
            panic!("ident is not Identifier.");
        }
    };

    let ident_ref = ident.value.with_ref(ctx).reference;
    if ident_ref != value {
        panic!("ident.value is not {}. got={}", value, ident_ref);
    }

    let literal_ref = ident.token_literal().with_ref(ctx).reference;
    if literal_ref != value {
        panic!(
            "ident.token_literal() is not {}. got={}",
            value, literal_ref
        );
    }

    true
}

fn test_boolean_literal(ctx: &Context, exp: &Expression, value: &bool) -> bool {
    let boolean = match exp {
        Expression::Boolean(boolean) => boolean,
        _ => {
            panic!("exp is not Boolean.");
        }
    };

    if boolean.value != *value {
        panic!("boolean.value is not {}. got={}", value, boolean.value);
    }

    let literal_ref = boolean.token_literal().with_ref(ctx).reference;
    if literal_ref != value.to_string() {
        panic!(
            "boolean.token_literal() is not {}. got={}",
            value, literal_ref
        );
    }

    true
}

fn test_infix_expression(
    ctx: &Context,
    exp: &Expression,
    left: TestLiteral,
    operator: &str,
    right: TestLiteral,
) -> bool {
    let op_exp = match exp {
        Expression::Infix(oe) => oe,
        _ => {
            panic!("exp is not InfixExpression.");
        }
    };

    let left_exp = match op_exp.left.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("op_exp.left is not Expression.");
        }
    };

    if !test_literal_expression(ctx, left_exp, &left) {
        return false;
    }

    let operator_ref = op_exp.operator.with_ref(ctx).reference;
    if operator_ref != operator {
        panic!("operator is not {}. got={}", operator, operator_ref);
    }

    let right_exp = match op_exp.right.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("op_exp.right is not Expression.");
        }
    };

    if !test_literal_expression(ctx, right_exp, &right) {
        return false;
    }

    true
}

fn check_parser_errors(parser: &Parser) {
    let errors = parser.errors();
    if errors.is_empty() {
        return;
    }

    eprintln!("parser has {} errors", errors.len());
    for err in errors {
        eprintln!("parser error: {}", err);
    }
}

// -- Tests -- //
#[test]
fn test_let_statements() {
    let tests = vec![
        ("let x = 5;", "x", TestLiteral::Int(5)),
        ("let y = true;", "y", TestLiteral::Bool(true)),
        ("let foobar = y;", "foobar", TestLiteral::Str("y")),
    ];

    for test in tests {
        let (input, ident, expected) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        check_parsed_program_len(&program, 1);

        let stmt = match &program.statements[0] {
            Node::Statement(s) => s,
            _ => {
                panic!("program.statements[0] is not Statement.");
            }
        };
        if !test_let_statement(&ctx, stmt, ident) {
            return;
        }

        let let_stmt_value = match stmt {
            Statement::Let(s) => match s.value.as_ref().unwrap().as_ref() {
                Node::Expression(e) => e,
                _ => {
                    panic!("let_stmt.value is not Expression.");
                }
            },
            _ => {
                panic!("stmt is not LetStatement.");
            }
        };
        if !test_literal_expression(&ctx, let_stmt_value, &expected) {
            return;
        }
    }

    fn test_let_statement(ctx: &Context, stmt: &Statement, ident: &str) -> bool {
        if !matches!(stmt, Statement::Let(_)) {
            panic!("stmt is not LetStatement.");
        }

        let let_stmt = match stmt {
            Statement::Let(s) => s,
            _ => {
                panic!("stmt is not LetStatement.");
            }
        };

        let ident_ref = let_stmt
            .name
            .as_ref()
            .unwrap()
            .value
            .with_ref(ctx)
            .reference;
        if ident_ref != ident {
            panic!("let_stmt.name.value not '{}'. got={}", ident, ident_ref);
        }

        let literal_ref = let_stmt
            .name
            .as_ref()
            .unwrap()
            .value
            .with_ref(ctx)
            .reference;
        if literal_ref != ident {
            panic!(
                "let_stmt.name.token_literal() not '{}'. got={}",
                ident, literal_ref
            );
        }

        true
    }
}

#[test]
fn test_return_statements() {
    let input = r#"
      return 5;
      return 10;
      return 838383;
    "#;
    let ctx = Context::new(input);
    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 3);

    for stmt in program.statements.iter() {
        let return_stmt = match stmt {
            Node::Statement(Statement::Return(s)) => s,
            _ => {
                panic!("stmt is not ReturnStatement.");
            }
        };

        let literal_ref = return_stmt.token_literal().with_ref(&ctx).reference;
        if literal_ref != "return" {
            panic!(
                "return_stmt.token_literal() not 'return'. got={}",
                literal_ref
            );
        }
    }
}

#[test]
fn test_identifier_expression() {
    let input = "foobar;";

    let ctx = Context::new(input);
    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ident = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::Identifier(ident)) => ident,
        _ => panic!("stmt.expression is not Identifier."),
    };

    let ident_ref = ident.value.with_ref(&ctx).reference;
    if ident_ref != "foobar" {
        panic!("ident.value is not 'foobar'. got={}", ident_ref);
    }

    let literal_ref = ident.token_literal().with_ref(&ctx).reference;
    if literal_ref != "foobar" {
        panic!("ident.token_literal() is not 'foobar'. got={}", literal_ref);
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let literal = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IntegerLiteral(literal)) => literal,
        _ => panic!("stmt.expression is not IntegerLiteral."),
    };

    if literal.value != 5 {
        panic!("literal.value is not 5. got={}", literal.value);
    }

    let literal_ref = literal.token_literal().with_ref(&ctx).reference;
    if literal_ref != "5" {
        panic!("literal.token_literal() is not {}. got={}", 5, literal_ref);
    }
}

#[test]
fn test_string_literal_expression() {
    let input = r#""hello world";"#;
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let literal = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::StringLiteral(literal)) => literal,
        _ => panic!("stmt.expression is not StringLiteral."),
    };
    let literal_ref = literal.value.with_ref(&ctx).reference;
    if literal_ref != "hello world" {
        panic!("literal.value is not 'hello world'. got={}", literal_ref);
    }
}

#[test]
fn test_boolean_expression() {
    let tests = vec![("true;", true), ("false;", false)];

    for test in tests {
        let (input, value) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        check_parsed_program_len(&program, 1);

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let boolean = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::Boolean(boolean)) => boolean,
            _ => panic!("stmt.expression is not Boolean."),
        };

        if boolean.value != value {
            panic!("boolean.value is not {}. got={}", value, boolean.value);
        }

        let literal_ref = boolean.token_literal().with_ref(&ctx).reference;
        if literal_ref != value.to_string() {
            panic!(
                "boolean.token_literal() is not {}. got={}",
                value, literal_ref
            );
        };
    }
}

#[test]
fn test_parsing_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let array = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::ArrayLiteral(array)) => array,
        _ => panic!("stmt.expression is not ArrayLiteral."),
    };

    let mut exprs: Vec<&Expression> = vec![];

    for element in array.elements.iter() {
        let exp = match element.as_ref() {
            Node::Expression(e) => e,
            _ => {
                panic!("array.elements[] is not Expression.");
            }
        };
        exprs.push(exp);
    }

    test_literal_expression(&ctx, exprs[0], &TestLiteral::Int(1));
    test_infix_expression(
        &ctx,
        exprs[1],
        TestLiteral::Int(2),
        "*",
        TestLiteral::Int(2),
    );
    test_infix_expression(
        &ctx,
        exprs[2],
        TestLiteral::Int(3),
        "+",
        TestLiteral::Int(3),
    );
}

#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let index_exp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IndexExpression(index)) => index,
        _ => panic!("stmt.expression is not IndexExpression."),
    };

    let ident = match index_exp.left.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("index_exp.left is not Expression.");
        }
    };

    if !test_identifier(&ctx, ident, "myArray") {
        return;
    }

    let index = match index_exp.index.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("index_exp.index is not Expression.");
        }
    };
    test_infix_expression(&ctx, index, TestLiteral::Int(1), "+", TestLiteral::Int(1));
}

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![
        ("!5;", "!", TestLiteral::Int(5)),
        ("-15;", "-", TestLiteral::Int(15)),
        ("!true;", "!", TestLiteral::Bool(true)),
        ("!false;", "!", TestLiteral::Bool(false)),
    ];

    for test in prefix_tests {
        let (input, operator, value) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        check_parsed_program_len(&program, 1);

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let expression = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::Prefix(expression)) => expression,
            _ => panic!("stmt.expression is not PrefixExpression."),
        };

        let operator_ref = expression.operator.with_ref(&ctx).reference;
        if operator_ref != operator {
            panic!(
                "expression.operator is not '{}'. got={}",
                operator, operator_ref
            );
        }

        let right_exp = match expression.right.as_ref().unwrap().as_ref() {
            Node::Expression(e) => e,
            _ => {
                panic!("expression.right is not Expression.");
            }
        };

        if !test_literal_expression(&ctx, right_exp, &value) {
            return;
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let infix_tests: Vec<(&'static str, TestLiteral, &'static str, TestLiteral)> = vec![
        ("5 + 5;", TestLiteral::Int(5), "+", TestLiteral::Int(5)),
        ("5 - 5;", TestLiteral::Int(5), "-", TestLiteral::Int(5)),
        ("5 * 5;", TestLiteral::Int(5), "*", TestLiteral::Int(5)),
        ("5 / 5;", TestLiteral::Int(5), "/", TestLiteral::Int(5)),
        ("5 > 5;", TestLiteral::Int(5), ">", TestLiteral::Int(5)),
        ("5 < 5;", TestLiteral::Int(5), "<", TestLiteral::Int(5)),
        ("5 == 5;", TestLiteral::Int(5), "==", TestLiteral::Int(5)),
        ("5 != 5;", TestLiteral::Int(5), "!=", TestLiteral::Int(5)),
        (
            "true == true",
            TestLiteral::Bool(true),
            "==",
            TestLiteral::Bool(true),
        ),
        (
            "true != false",
            TestLiteral::Bool(true),
            "!=",
            TestLiteral::Bool(false),
        ),
        (
            "false == false",
            TestLiteral::Bool(false),
            "==",
            TestLiteral::Bool(false),
        ),
    ];

    for test in infix_tests {
        let (input, left_value, operator, right_value) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        check_parsed_program_len(&program, 1);

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let exp = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(exp) => exp,
            _ => panic!("stmt.expression is not InfixExpression."),
        };

        if !test_infix_expression(&ctx, exp, left_value, operator, right_value) {
            return;
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
        ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        (
            "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        ),
        (
            "add(a + b + c * d / f + g)",
            "add((((a + b) + ((c * d) / f)) + g))",
        ),
        (
            "a * [1, 2, 3, 4][b * c] * d",
            "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        ),
        (
            "add(a * b[2], b[1], 2 * [1, 2][1])",
            "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        ),
    ];

    for test in tests {
        let (input, expected) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        let actual = program.to_string();
        if actual != expected {
            panic!("expected={}, got={}", expected, actual);
        }
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x }";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ifexp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IfExpression(expression)) => expression,
        _ => panic!("stmt.expression is not IfExpression."),
    };

    let exp = match ifexp.condition.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("ifexp.condition is not Expression.");
        }
    };

    if !test_infix_expression(&ctx, exp, TestLiteral::Str("x"), "<", TestLiteral::Str("y")) {
        return;
    }

    if ifexp.consequence.is_none() {
        panic!("ifexp.consequence is None.");
    }

    let if_exp_consequence = match ifexp.consequence.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.consequence is not BlockStatement.");
        }
    };

    if if_exp_consequence.statements.len() != 1 {
        panic!(
            "if_exp_consequence.statements does not contain 1 statement. got={}",
            if_exp_consequence.statements.len()
        );
    }

    let consequence = match &if_exp_consequence.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.consequence.statements[0] is not ExpressionStatement."),
    };

    let ident = match consequence.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("consequence.expression is not Identifier."),
    };
    if !test_identifier(&ctx, ident, "x") {
        return;
    }

    if ifexp.alternative.is_some() {
        panic!("ifexp.alternative is not None.");
    }
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y }";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let ifexp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::IfExpression(expression)) => expression,
        _ => panic!("stmt.expression is not IfExpression."),
    };

    let condition = match ifexp.condition.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("ifexp.condition is not Expression.");
        }
    };

    if !test_infix_expression(
        &ctx,
        condition,
        TestLiteral::Str("x"),
        "<",
        TestLiteral::Str("y"),
    ) {
        return;
    }

    if ifexp.consequence.is_none() {
        panic!("ifexp.consequence is None.");
    }

    let if_exp_consequence = match ifexp.consequence.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.consequence is not BlockStatement.");
        }
    };

    if if_exp_consequence.statements.len() != 1 {
        panic!(
            "if_exp_consequence.statements does not contain 1 statement. got={}",
            if_exp_consequence.statements.len()
        );
    }

    let consequence = match &if_exp_consequence.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.consequence.statements[0] is not ExpressionStatement."),
    };

    let ident = match consequence.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("consequence.expression is not Identifier."),
    };

    if !test_identifier(&ctx, ident, "x") {
        return;
    }

    if ifexp.alternative.is_none() {
        panic!("ifexp.alternative is None.");
    }

    let if_exp_alternative = match ifexp.alternative.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("ifexp.alternative is not BlockStatement.");
        }
    };

    if if_exp_alternative.statements.len() != 1 {
        panic!(
            "if_exp_alternative.statements does not contain 1 statement. got={}",
            if_exp_alternative.statements.len()
        );
    }

    let alternative = match &if_exp_alternative.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("ifexp.alternative.statements[0] is not ExpressionStatement."),
    };

    let ident = match alternative.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("alternative.expression is not Identifier."),
    };

    if !test_identifier(&ctx, ident, "y") {}
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let func = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::FunctionLiteral(function)) => function,
        _ => panic!("stmt.expression is not FunctionLiteral."),
    };

    if func.parameters.len() != 2 {
        panic!(
            "function.parameters does not contain 2 parameters. got={}",
            func.parameters.len()
        );
    }

    let param0 = match &func.parameters[0] {
        Node::Expression(e) => e,
        _ => {
            panic!("function.parameters[0] is not Expression.");
        }
    };

    if !test_literal_expression(&ctx, param0, &TestLiteral::Str("x")) {
        return;
    }

    let param1 = match &func.parameters[1] {
        Node::Expression(e) => e,
        _ => {
            panic!("function.parameters[1] is not Expression.");
        }
    };

    if !test_literal_expression(&ctx, param1, &TestLiteral::Str("y")) {
        return;
    }

    let func_body = match func.body.as_ref().unwrap().as_ref() {
        Node::Statement(Statement::BlockStatement(b)) => b,
        _ => {
            panic!("func.body is not BlockStatement.");
        }
    };

    if func_body.statements.len() != 1 {
        panic!(
            "func_body.statements does not contain 1 statement. got={}",
            func_body.statements.len()
        );
    }

    let body_stmt = match &func_body.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("func_body.statements[0] is not ExpressionStatement."),
    };

    let exp = match body_stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(e) => e,
        _ => panic!("body_stmt.expression is not InfixExpression."),
    };

    if !test_infix_expression(&ctx, exp, TestLiteral::Str("x"), "+", TestLiteral::Str("y")) {}
}

#[test]
fn test_function_parameter_parsing() {
    let tests = vec![
        ("fn() {};", vec![]),
        ("fn(x) {};", vec![TestLiteral::Str("x")]),
        (
            "fn(x, y, z) {};",
            vec![
                TestLiteral::Str("x"),
                TestLiteral::Str("y"),
                TestLiteral::Str("z"),
            ],
        ),
    ];

    for test in tests {
        let (input, expected_params) = test;
        let ctx = Context::new(input);

        let program = test_parse_program(&ctx);
        check_parsed_program_len(&program, 1);

        let stmt = match &program.statements[0] {
            Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
            _ => panic!("program.statements[0] is not ExpressionStatement."),
        };

        let func = match stmt.expression.as_ref().unwrap().as_ref() {
            Node::Expression(Expression::FunctionLiteral(function)) => function,
            _ => panic!("stmt.expression is not FunctionLiteral."),
        };

        if func.parameters.len() != expected_params.len() {
            panic!(
                "func.parameters does not contain {} parameters. got={}",
                expected_params.len(),
                func.parameters.len()
            );
        }

        for (i, ident) in expected_params.iter().enumerate() {
            let param = match &func.parameters[i] {
                Node::Expression(e) => e,
                _ => {
                    panic!("func.parameters[{}] is not Expression.", i);
                }
            };
            if !test_literal_expression(&ctx, param, ident) {
                return;
            }
        }
    }
}

#[test]
fn test_call_expression_parsing() {
    let input = "add(1, 2 * 3, 4 + 5);";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };

    let exp = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::CallExpression(exp)) => exp,
        _ => panic!("stmt.expression is not CallExpression."),
    };

    let func = match exp.function.as_ref() {
        Node::Expression(e) => e,
        _ => {
            panic!("exp.function is not Expression.");
        }
    };

    if !test_identifier(&ctx, func, "add") {
        return;
    }

    if exp.arguments.len() != 3 {
        panic!(
            "exp.arguments does not contain 3 arguments. got={}",
            exp.arguments.len()
        );
    }

    let exp0 = match &exp.arguments[0].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments[0] is not Expression.");
        }
    };

    if !test_literal_expression(&ctx, exp0, &TestLiteral::Int(1)) {
        return;
    }

    let exp1 = match &exp.arguments[1].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments[1] is not Expression.");
        }
    };

    if !test_infix_expression(&ctx, exp1, TestLiteral::Int(2), "*", TestLiteral::Int(3)) {
        return;
    }

    let exp2 = match &exp.arguments[2].as_ref() {
        Node::Expression(exps) => exps,
        _ => {
            panic!("exp.arguments[2] is not Expression.");
        }
    };

    if !test_infix_expression(&ctx, exp2, TestLiteral::Int(4), "+", TestLiteral::Int(5)) {}
}

#[test]
fn test_parsing_hash_literals_string_keys() {
    let input = r#"{"one": 1, "two": 2, "three": 3}"#;
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let hash = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::HashLiteral(hash)) => hash,
        _ => panic!("stmt.expression is not HashLiteral."),
    };

    if hash.pairs.len() != 3 {
        panic!(
            "hash.pairs does not contain 3 pairs. got={}",
            hash.pairs.len()
        );
    }

    let expected_map: std::collections::HashMap<&str, i64> =
        vec![("one", 1), ("two", 2), ("three", 3)]
            .into_iter()
            .collect();

    for (key, value) in hash.pairs.iter() {
        let key_expr = match key.as_ref() {
            Node::Expression(Expression::StringLiteral(s)) => s,
            _ => panic!("key is not StringLiteral."),
        };
        let key_str = key_expr.value.with_ref(&ctx).reference;

        let expected_value = match expected_map.get(key_str) {
            Some(v) => TestLiteral::Int(*v),
            None => panic!("unexpected key: {}", key_str),
        };

        let value_expr = match value.as_ref() {
            Node::Expression(e) => e,
            _ => panic!("value is not Expression."),
        };

        if !test_literal_expression(&ctx, value_expr, &expected_value) {
            return;
        }
    }
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let hash = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::HashLiteral(hash)) => hash,
        _ => panic!("stmt.expression is not HashLiteral."),
    };

    if !hash.pairs.is_empty() {
        panic!("hash.pairs is not empty. got={}", hash.pairs.len());
    }
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    let input = r#"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"#;
    let ctx = Context::new(input);

    let program = test_parse_program(&ctx);
    check_parsed_program_len(&program, 1);

    let stmt = match &program.statements[0] {
        Node::Statement(Statement::ExpressionStatement(stmt)) => stmt,
        _ => panic!("program.statements[0] is not ExpressionStatement."),
    };
    let hash = match stmt.expression.as_ref().unwrap().as_ref() {
        Node::Expression(Expression::HashLiteral(hash)) => hash,
        _ => panic!("stmt.expression is not HashLiteral."),
    };

    if hash.pairs.len() != 3 {
        panic!(
            "hash.pairs does not contain 3 pairs. got={}",
            hash.pairs.len()
        );
    }

    let expected_map: std::collections::HashMap<&str, (i64, &str, i64)> = vec![
        ("one", (0, "+", 1)),
        ("two", (10, "-", 8)),
        ("three", (15, "/", 5)),
    ]
    .into_iter()
    .collect();

    for (key, value) in hash.pairs.iter() {
        let key_expr = match key.as_ref() {
            Node::Expression(Expression::StringLiteral(s)) => s,
            _ => panic!("key is not StringLiteral."),
        };
        let key_str = key_expr.value.with_ref(&ctx).reference;

        let (left, operator, right) = match expected_map.get(key_str) {
            Some(v) => v,
            None => panic!("unexpected key: {}", key_str),
        };

        let value_expr = match value.as_ref() {
            Node::Expression(e) => e,
            _ => panic!("value is not Expression."),
        };

        if !test_infix_expression(
            &ctx,
            value_expr,
            TestLiteral::Int(*left),
            operator,
            TestLiteral::Int(*right),
        ) {
            return;
        }
    }
}
