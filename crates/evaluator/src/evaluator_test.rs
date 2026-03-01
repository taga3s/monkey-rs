use lexer::lexer::Lexer;
use object::{
    error::EvaluateError,
    object::{Boolean, Integer, Null, ObjectTypes, StringLiteral},
};
use parser::parser::Parser;
use utils::{context::Context, test::TestLiteral};

use crate::evaluator::Evaluator;

// -- Test Helpers -- //
fn test_eval(input: &str) -> Result<ObjectTypes, EvaluateError> {
    let ctx = Context::new(input);
    let lexer = Lexer::new(&ctx);
    let mut parser = Parser::new(&ctx, lexer);
    let program = parser.parse_program().unwrap();
    let evaluator = Evaluator::new();
    evaluator.run(&ctx, &program)
}

fn test_integer_object(obj: Result<ObjectTypes, EvaluateError>, expected: i64) {
    let value = match obj {
        Ok(ObjectTypes::Integer(integer)) => integer.value,
        Err(err) => panic!("object is not Integer. got={}", err),
        _ => return,
    };

    assert_eq!(value, expected);
}

fn test_boolean_object(obj: Result<ObjectTypes, EvaluateError>, expected: bool) {
    let value = match obj {
        Ok(ObjectTypes::Boolean(boolean)) => boolean.value,
        Err(err) => panic!("object is not Boolean. got={}", err),
        _ => panic!("object is not Boolean"),
    };

    assert_eq!(value, expected);
}

// -- Tests -- //
#[test]
fn test_eval_integer_expression() {
    let tests = vec![
        ("5", 5),
        ("10", 10),
        ("-5", -5),
        ("-10", -10),
        ("5 + 5 + 5 + 5 - 10", 10),
        ("2 * 2 * 2 * 2 * 2", 32),
        ("-50 + 100 + -50", 0),
        ("5 * 2 + 10", 20),
        ("5 + 2 * 10", 25),
        ("20 + 2 * -10", 0),
        ("50 / 2 * 2 + 10", 60),
        ("2 * (5 + 10)", 30),
        ("3 * 3 * 3 + 10", 37),
        ("3 * (3 * 3) + 10", 37),
        ("(5 + 10 * 2 + 15 /3) * 2 + -10", 50),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_string_literal() {
    let input = r#""Hello, World!""#;

    let evaluated = test_eval(input);
    match evaluated {
        Ok(ObjectTypes::StringLiteral(string)) => {
            assert_eq!(string.value, "Hello, World!");
        }
        Err(err) => panic!("evaluated is not StringLiteral. got={}", err),
        _ => {}
    }
}

#[test]
fn test_string_concatenation() {
    let input = r#""Hello, " + "World!""#;

    let evaluated = test_eval(input);
    match evaluated {
        Ok(ObjectTypes::StringLiteral(string)) => {
            assert_eq!(string.value, "Hello, World!");
        }
        Err(err) => panic!("evaluated is not StringLiteral. got={}", err),
        _ => {}
    }
}

#[test]
fn test_eval_boolean_expression() {
    let tests = vec![
        ("true", true),
        ("false", false),
        ("1 < 2", true),
        ("1 > 2", false),
        ("1 < 1", false),
        ("1 > 1", false),
        ("1 == 1", true),
        ("1 != 1", false),
        ("1 == 2", false),
        ("1 != 2", true),
        ("true == true", true),
        ("false == false", true),
        ("true == false", false),
        ("true != false", true),
        ("false != true", true),
        ("(1 < 2) == true", true),
        ("(1 < 2) == false", false),
        ("(1 > 2) == true", false),
        ("(1 > 2) == false", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_bang_operator() {
    let tests = vec![
        ("!true", false),
        ("!false", true),
        ("!5", false),
        ("!!true", true),
        ("!!false", false),
        ("!!5", true),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_boolean_object(evaluated, expected);
    }
}

#[test]
fn test_if_else_expressions() {
    let tests = vec![
        ("if (true) { 10 }", Some(10)),
        ("if (false) { 10 }", None),
        ("if (1) { 10 }", Some(10)),
        ("if (1 < 2) { 10 }", Some(10)),
        ("if (1 > 2) { 10 }", None),
        ("if (1 > 2) { 10 } else { 20 }", Some(20)),
        ("if (1 < 2) { 10 } else { 20 }", Some(10)),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);
        match test.1 {
            Some(expected) => {
                test_integer_object(evaluated, expected);
            }
            None => {
                test_null_object(evaluated);
            }
        }
    }
}

fn test_null_object(obj: Result<ObjectTypes, EvaluateError>) -> bool {
    match obj {
        Ok(ObjectTypes::Null(Null {})) => true,
        Err(err) => panic!("object is not Null. got={}", err),
        _ => panic!("object is not Null"),
    }
}

#[test]
fn test_return_statements() {
    let tests = vec![
        ("return 10;", 10),
        ("return 10; 9;", 10),
        ("return 2 * 5; 9;", 10),
        ("9; return 2 * 5; 9;", 10),
        ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_error_handling() {
    let tests = vec![
        ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
        ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
        ("-true", "unknown operator: -BOOLEAN"),
        ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
        ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
        (
            "if (10 > 1) { true + false; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        (
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
            "unknown operator: BOOLEAN + BOOLEAN",
        ),
        ("foobar", "identifier not found: foobar"),
        ("\"Hello\" - \"World\"", "unknown operator: STRING - STRING"),
        (
            r#"{"name": "Monkey"}[fn(x) { x }];"#,
            "unusable as hash key: FUNCTION",
        ),
        (
            "fn(x) { x }();",
            "wrong number of arguments: expected 1, got 0",
        ),
        (
            "fn(x, y) { x + y }(1);",
            "wrong number of arguments: expected 2, got 1",
        ),
        (
            "fn(x) { x }(1, 2);",
            "wrong number of arguments: expected 1, got 2",
        ),
        (
            "fn(a, b, c) { a + b + c }(1, 2);",
            "wrong number of arguments: expected 3, got 2",
        ),
    ];

    for test in tests {
        let evaluated = test_eval(test.0);

        match evaluated {
            Err(error) => {
                assert_eq!(error.message, test.1);
            }
            Ok(obj) => {
                panic!("expected error but got object: {}", obj.inspect());
            }
        }
    }
}

#[test]
fn test_let_statements() {
    let tests = vec![
        ("let a = 5; a;", 5),
        ("let a = 5 * 5; a;", 25),
        ("let a = 5; let b = a; b;", 5),
        ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_function_object() {
    let input = "fn(x) { x + 2; };";

    let evaluated = test_eval(input);
    match evaluated {
        Ok(ObjectTypes::Function(function)) => {
            assert_eq!(function.parameters.len(), 1);
            assert_eq!(function.parameters[0].to_string(), "x");
            assert_eq!(function.body.to_string(), "(x + 2)");
        }
        Err(err) => {
            panic!("object is not Function. got={}", err);
        }
        _ => {
            panic!("object is not Function");
        }
    }
}

#[test]
fn test_function_application() {
    let tests = vec![
        ("let identity = fn(x) { x; }; identity(5);", 5),
        ("let identity = fn(x) { return x; }; identity(5);", 5),
        ("let double = fn(x) { x * 2; }; double(5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
        ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
        ("fn(x) { x; }(5)", 5),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        test_integer_object(evaluated, expected);
    }
}

#[test]
fn test_closures() {
    let input = "
    let newAdder = fn(x) {
        fn(y) { x + y };
    };
    let addTwo = newAdder(2);
    addTwo(3);
    ";

    let evaluated = test_eval(input);
    test_integer_object(evaluated, 5);
}

#[test]
fn test_builtin_functions() {
    let tests = vec![
        (r#"len("")"#, TestLiteral::Int(0)),
        (r#"len("four")"#, TestLiteral::Int(4)),
        (r#"len("hello world")"#, TestLiteral::Int(11)),
        (
            "len(1)",
            TestLiteral::Str("argument to `len` not supported, got INTEGER"),
        ),
        (
            r#"len("one", "two")"#,
            TestLiteral::Str("wrong number of arguments. got=2, want=1"),
        ),
        ("len([1, 2, 3])", TestLiteral::Int(3)),
        ("len([])", TestLiteral::Int(0)),
        ("first([1, 2, 3])", TestLiteral::Int(1)),
        ("last([1, 2, 3])", TestLiteral::Int(3)),
        (
            "rest([1, 2, 3])",
            TestLiteral::Array(vec![TestLiteral::Int(2), TestLiteral::Int(3)]),
        ),
        (
            "push([1, 2], 3)",
            TestLiteral::Array(vec![
                TestLiteral::Int(1),
                TestLiteral::Int(2),
                TestLiteral::Int(3),
            ]),
        ),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);

        match expected {
            TestLiteral::Int(expected) => {
                test_integer_object(evaluated, expected);
            }
            TestLiteral::Str(expected) => match evaluated {
                Err(error) => {
                    assert_eq!(error.message, expected);
                }
                Ok(obj) => {
                    panic!("expected error but got object: {}", obj.inspect());
                }
            },
            TestLiteral::Array(expected) => match evaluated {
                Ok(ObjectTypes::Array(array)) => {
                    for (i, elem) in expected.iter().enumerate() {
                        match elem {
                            TestLiteral::Int(v) => {
                                test_integer_object(Ok(array.elements[i].clone()), *v);
                            }
                            TestLiteral::Bool(v) => {
                                test_boolean_object(Ok(array.elements[i].clone()), *v);
                            }
                            TestLiteral::Str(v) => match array.elements[i].clone() {
                                ObjectTypes::StringLiteral(s) => {
                                    assert_eq!(s.value, *v);
                                }
                                _ => panic!(
                                    "object is not StringLiteral. got={}",
                                    array.elements[i].inspect()
                                ),
                            },
                            TestLiteral::Array(_) => {
                                panic!("nested arrays not supported in test")
                            }
                        }
                    }
                }
                Err(err) => {
                    panic!("object is not Array. got={}", err);
                }
                _ => {
                    panic!("object is not Array");
                }
            },
            _ => panic!("test case error"),
        }
    }
}

#[test]
fn test_array_literals() {
    let input = "[1, 2 * 2, 3 + 3]";

    let evaluated = test_eval(input);
    match evaluated {
        Ok(ObjectTypes::Array(array)) => {
            assert_eq!(array.elements.len(), 3);
            test_integer_object(Ok(array.elements[0].clone()), 1);
            test_integer_object(Ok(array.elements[1].clone()), 4);
            test_integer_object(Ok(array.elements[2].clone()), 6);
        }
        Err(err) => {
            panic!("object is not Array. got={}", err);
        }
        _ => {
            panic!("object is not Array");
        }
    }
}

#[test]
fn test_array_index_expressions() {
    let tests = vec![
        ("[1, 2, 3][0]", Some(1)),
        ("[1, 2, 3][1]", Some(2)),
        ("[1, 2, 3][2]", Some(3)),
        ("let i = 0; [1][i];", Some(1)),
        ("[1, 2, 3][1 + 1];", Some(3)),
        ("let myArray = [1, 2, 3]; myArray[2];", Some(3)),
        (
            "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            Some(6),
        ),
        (
            "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            Some(2),
        ),
        ("[1, 2, 3][3]", None),
        ("[1, 2, 3][-1]", None),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Some(expected) => {
                test_integer_object(evaluated, expected);
            }
            None => {
                test_null_object(evaluated);
            }
        }
    }
}

#[test]
fn test_hash_literals() {
    let input = r#"
    let two = "two";
    {
        "one": 10 - 9,
        two: 1 + 1,
        "thr" + "ee": 6 / 2,
        4: 4,
        true: 5,
        false: 6
    }
    "#;

    let evaluated = test_eval(input);
    match evaluated {
        Ok(ObjectTypes::Hash(hash)) => {
            let expected = vec![
                (
                    ObjectTypes::StringLiteral(StringLiteral {
                        value: "one".to_string(),
                    }),
                    1,
                ),
                (
                    ObjectTypes::StringLiteral(StringLiteral {
                        value: "two".to_string(),
                    }),
                    2,
                ),
                (
                    ObjectTypes::StringLiteral(StringLiteral {
                        value: "three".to_string(),
                    }),
                    3,
                ),
                (ObjectTypes::Integer(Integer { value: 4 }), 4),
                (ObjectTypes::Boolean(Boolean { value: true }), 5),
                (ObjectTypes::Boolean(Boolean { value: false }), 6),
            ];

            assert_eq!(hash.pairs.len(), expected.len());

            for (expected_key, expected_value) in expected {
                let hash_key = match &expected_key {
                    ObjectTypes::StringLiteral(s) => s.hash_key(),
                    ObjectTypes::Integer(i) => i.hash_key(),
                    ObjectTypes::Boolean(b) => b.hash_key(),
                    _ => panic!("unusable as hash key: {}", expected_key.inspect()),
                };
                let pair = hash.pairs.get(&hash_key).unwrap();
                test_integer_object(Ok(pair.value.clone()), expected_value);
            }
        }
        Err(err) => {
            panic!("Eval didn't return Hash. got={}", err);
        }
        _ => {
            panic!("Eval didn't return Hash");
        }
    }
}

#[test]
fn test_hash_index_expressions() {
    let tests = vec![
        (r#"{"foo": 5}["foo"]"#, Some(5)),
        (r#"{"foo": 5}["bar"]"#, None),
        (r#"let key = "foo"; {"foo": 5}[key]"#, Some(5)),
        (r#"{}["foo"]"#, None),
        (r#"{5: 5}[5]"#, Some(5)),
        (r#"{true: 5}[true]"#, Some(5)),
        (r#"{false: 5}[false]"#, Some(5)),
    ];

    for (input, expected) in tests {
        let evaluated = test_eval(input);
        match expected {
            Some(expected) => {
                test_integer_object(evaluated, expected);
            }
            None => {
                test_null_object(evaluated);
            }
        }
    }
}
