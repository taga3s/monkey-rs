use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::ast::{
    BlockStatement, Expression, HashLiteral, Identifier, IfExpression, Node, Program, Statement,
};
use object::{
    environment::Environment,
    error::{EvaluationError, new_evaluation_error},
    object::{
        Array, Boolean, Function, Hash, HashPair, Integer, Null, ObjectType, ObjectTypes,
        ReturnValue, StringLiteral,
    },
};

use crate::builtins::BUILTINS;

const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

pub fn eval(node: &Node, env: Rc<RefCell<Environment>>) -> Result<ObjectTypes, EvaluationError> {
    let result = match node {
        Node::Program(program) => eval_program(program, env)?,
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => ObjectTypes::Integer(Integer { value: il.value }),
            Expression::StringLiteral(sl) => ObjectTypes::StringLiteral(StringLiteral {
                value: sl.value.clone(),
            }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value)?,
            Expression::ArrayLiteral(al) => {
                let elements = eval_expressions(&al.elements, env)?;
                ObjectTypes::Array(Array { elements })
            }
            Expression::IndexExpression(ie) => {
                let left = eval(
                    ie.left.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing left operand in index expression",
                    ))?,
                    env.clone(),
                )?;
                let index = eval(
                    ie.index.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing index in index expression",
                    ))?,
                    env.clone(),
                )?;
                return eval_index_expression(&left, &index);
            }
            Expression::Identifier(ident) => eval_identifier(ident, env.clone())?,
            Expression::Infix(infix) => {
                let left = eval(
                    infix.left.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing left operand in infix expression",
                    ))?,
                    env.clone(),
                )?;
                let right = eval(
                    infix.right.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing right operand in infix expression",
                    ))?,
                    env.clone(),
                )?;
                return eval_infix_expression(&infix.operator, &left, &right);
            }
            Expression::Prefix(prefix) => {
                let right = eval(
                    prefix.right.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing operand in prefix expression",
                    ))?,
                    env,
                )?;
                return eval_prefix_expression(&prefix.operator, &right);
            }
            Expression::IfExpression(ifexp) => eval_if_expression(ifexp, env)?,
            Expression::FunctionLiteral(fl) => {
                let mut parameters = vec![];
                for p in &fl.parameters {
                    match p {
                        Node::Expression(Expression::Identifier(ident)) => {
                            parameters.push(ident.clone())
                        }
                        _ => {
                            return Err(new_evaluation_error(
                                "function parameter is not an identifier",
                            ));
                        }
                    }
                }
                let body = match fl
                    .body
                    .as_ref()
                    .ok_or(new_evaluation_error(
                        "[internal:evaluator] missing function body",
                    ))?
                    .as_ref()
                {
                    Node::Statement(Statement::BlockStatement(bs)) => bs,
                    _ => {
                        return Err(new_evaluation_error(
                            "function body is not a block statement",
                        ));
                    }
                };
                return Ok(ObjectTypes::Function(Function {
                    parameters,
                    body: body.clone(),
                    env,
                }));
            }
            Expression::CallExpression(ce) => {
                let func = eval(ce.function.as_ref(), env.clone())?;
                let args = eval_expressions(&ce.arguments, env.clone())?;
                return apply_function(&func, &args);
            }
            Expression::HashLiteral(hl) => eval_hash_literal(hl, env)?,
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(
                es.expression.as_ref().ok_or(new_evaluation_error(
                    "[internal:evaluator] missing expression in expression statement",
                ))?,
                env,
            )?,
            Statement::Let(ls) => {
                let val = eval(
                    ls.value.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing value in let statement",
                    ))?,
                    env.clone(),
                )?;
                env.borrow_mut().set(
                    &ls.name
                        .as_ref()
                        .ok_or(new_evaluation_error(
                            "[internal:evaluator] missing identifier in let statement",
                        ))?
                        .value,
                    val,
                );
                NULL
            }
            Statement::Return(rs) => {
                let val = eval(
                    rs.return_value.as_ref().ok_or(new_evaluation_error(
                        "[internal:evaluator] missing return value in return statement",
                    ))?,
                    env,
                )?;
                return Ok(ObjectTypes::ReturnValue(ReturnValue {
                    value: Box::new(val),
                }));
            }
            Statement::BlockStatement(bs) => eval_block_statement(bs, env)?,
        },
    };
    Ok(result)
}

fn eval_program(
    program: &Program,
    env: Rc<RefCell<Environment>>,
) -> Result<ObjectTypes, EvaluationError> {
    let mut result = NULL;

    for stmt in &program.statements {
        result = eval(stmt, env.clone())?;
        if let ObjectTypes::ReturnValue(return_value) = result {
            return Ok(*return_value.value);
        }
    }
    Ok(result)
}

fn eval_block_statement(
    bs: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<ObjectTypes, EvaluationError> {
    let mut result = NULL;

    for stmt in &bs.statements {
        result = eval(stmt, env.clone())?;
        if result.as_type(ObjectType::ReturnValueObj) {
            return Ok(result);
        }
    }
    Ok(result)
}

fn native_bool_to_boolean_object(input: bool) -> Result<ObjectTypes, EvaluationError> {
    if input { Ok(TRUE) } else { Ok(FALSE) }
}

fn eval_prefix_expression(
    operator: &str,
    right: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Err(new_evaluation_error(&format!(
            "unknown operator: {}{}",
            operator,
            right.ty()
        ))),
    }
}

fn eval_identifier(
    node: &Identifier,
    env: Rc<RefCell<Environment>>,
) -> Result<ObjectTypes, EvaluationError> {
    if let Some(val) = env.borrow().get(&node.value) {
        return Ok(val);
    }

    if let Some((_, builtin)) = BUILTINS.iter().find(|(name, _)| *name == node.value) {
        return Ok(ObjectTypes::Builtin(builtin.clone()));
    }

    Err(new_evaluation_error(&format!(
        "identifier not found: {}",
        node.value
    )))
}

fn eval_bang_operator_expression(right: &ObjectTypes) -> Result<ObjectTypes, EvaluationError> {
    match right {
        ObjectTypes::Boolean(boolean) => {
            if boolean.value {
                Ok(FALSE)
            } else {
                Ok(TRUE)
            }
        }
        ObjectTypes::Null(_) => Ok(TRUE),
        _ => Ok(FALSE),
    }
}

fn eval_minus_prefix_operator_expression(
    right: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    if right.ty() != ObjectType::IntegerObj {
        return Err(new_evaluation_error(&format!(
            "unknown operator: -{}",
            right.ty()
        )));
    }

    match right {
        ObjectTypes::Integer(integer) => Ok(ObjectTypes::Integer(Integer {
            value: -integer.value,
        })),
        _ => Err(new_evaluation_error(&format!(
            "unknown operator: -{}",
            right.ty()
        ))),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    if left.as_type(ObjectType::IntegerObj) && right.as_type(ObjectType::IntegerObj) {
        return eval_integer_infix_expression(operator, left, right);
    };
    if left.as_type(ObjectType::StringObj) && right.as_type(ObjectType::StringObj) {
        return eval_string_infix_expression(operator, left, right);
    };
    if left.ty() != right.ty() {
        return Err(new_evaluation_error(&format!(
            "type mismatch: {} {} {}",
            left.ty(),
            operator,
            right.ty()
        )));
    }
    //FIXME: comparison logic may be invalid
    if operator == "==" {
        return native_bool_to_boolean_object(left.inspect() == right.inspect());
    }
    if operator == "!=" {
        return native_bool_to_boolean_object(left.inspect() != right.inspect());
    }

    Err(new_evaluation_error(&format!(
        "unknown operator: {} {} {}",
        left.ty(),
        operator,
        right.ty()
    )))
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    let left_val = match &left {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return Err(new_evaluation_error("left is not an integer")),
    };
    let right_val = match &right {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return Err(new_evaluation_error("right is not an integer")),
    };

    match operator {
        "+" => Ok(ObjectTypes::Integer(Integer {
            value: left_val + right_val,
        })),
        "-" => Ok(ObjectTypes::Integer(Integer {
            value: left_val - right_val,
        })),
        "*" => Ok(ObjectTypes::Integer(Integer {
            value: left_val * right_val,
        })),
        "/" => {
            if right_val == 0 {
                return Err(new_evaluation_error(&format!(
                    "attempt to divide by zero: {} {} {}",
                    left_val, operator, right_val
                )));
            }
            Ok(ObjectTypes::Integer(Integer {
                value: left_val / right_val,
            }))
        }
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => Err(new_evaluation_error(&format!(
            "unknown operator: {} {} {}",
            &left.ty(),
            operator,
            &right.ty()
        ))),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    let left_val = match &left {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return Err(new_evaluation_error("left is not a string")),
    };
    let right_val = match &right {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return Err(new_evaluation_error("right is not a string")),
    };

    match operator {
        "+" => Ok(ObjectTypes::StringLiteral(StringLiteral {
            value: format!("{}{}", left_val, right_val),
        })),
        _ => Err(new_evaluation_error(&format!(
            "unknown operator: {} {} {}",
            &left.ty(),
            operator,
            &right.ty()
        ))),
    }
}

fn is_truthy(obj: &ObjectTypes) -> bool {
    match obj {
        ObjectTypes::Boolean(boolean) => boolean.value,
        ObjectTypes::Null(_) => false,
        _ => true,
    }
}

fn eval_if_expression(
    ie: &IfExpression,
    env: Rc<RefCell<Environment>>,
) -> Result<ObjectTypes, EvaluationError> {
    let condition = eval(
        ie.condition.as_ref().ok_or(new_evaluation_error(
            "[internal:evaluator] missing condition in if expression",
        ))?,
        env.clone(),
    )?;
    if is_truthy(&condition) {
        return eval(
            ie.consequence.as_ref().ok_or(new_evaluation_error(
                "[internal:evaluator] missing consequence in if expression",
            ))?,
            env.clone(),
        );
    }
    if let Some(alt) = &ie.alternative {
        return eval(alt, env.clone());
    }
    Ok(NULL)
}

fn eval_expressions(
    exps: &[Box<Node>],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<ObjectTypes>, EvaluationError> {
    let mut result = vec![];

    for e in exps {
        let evaluated = eval(e.as_ref(), env.clone())?;
        result.push(evaluated);
    }
    Ok(result)
}

fn apply_function(
    func: &ObjectTypes,
    args: &[ObjectTypes],
) -> Result<ObjectTypes, EvaluationError> {
    if let ObjectTypes::Function(function) = func {
        let extended_env = extend_function_env(function, args)?;
        let evaluated = eval(
            &Node::Statement(Statement::BlockStatement(function.body.clone())),
            extended_env,
        )?;
        return Ok(unwrap_return_value(evaluated));
    }

    if let ObjectTypes::Builtin(builtin) = func {
        return (builtin.fn_)(args);
    }

    Err(new_evaluation_error(&format!(
        "not a function: {}",
        func.ty()
    )))
}

fn extend_function_env(
    func: &Function,
    args: &[ObjectTypes],
) -> Result<Rc<RefCell<Environment>>, EvaluationError> {
    // Check if the number of arguments matches the number of parameters
    if func.parameters.len() != args.len() {
        return Err(new_evaluation_error(&format!(
            "wrong number of arguments: expected {}, got {}",
            func.parameters.len(),
            args.len()
        )));
    }

    let env = Environment::new_enclosed(func.env.clone());

    // Parameter count matches argument count (checked above)
    for (param_idx, param) in func.parameters.iter().enumerate() {
        env.borrow_mut().set(&param.value, args[param_idx].clone());
    }

    Ok(env)
}

fn unwrap_return_value(obj: ObjectTypes) -> ObjectTypes {
    if obj.as_type(ObjectType::ReturnValueObj)
        && let ObjectTypes::ReturnValue(rv) = obj
    {
        return *rv.value;
    }
    obj
}

fn eval_index_expression(
    left: &ObjectTypes,
    index: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    if left.as_type(ObjectType::ArrayObj) && index.as_type(ObjectType::IntegerObj) {
        return eval_array_expression(left, index);
    }
    if left.as_type(ObjectType::HashObj) {
        return eval_hash_index_expression(left, index);
    }

    Err(new_evaluation_error(&format!(
        "index operator not supported: {}",
        left.ty()
    )))
}

fn eval_array_expression(
    left: &ObjectTypes,
    index: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    let array = match left {
        ObjectTypes::Array(array) => array,
        _ => return Err(new_evaluation_error("left is not an array")),
    };
    let idx = match index {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return Err(new_evaluation_error("index is not an integer")),
    };

    let array_len = array.elements.len() as i64;

    // Return NULL for out-of-bounds access
    if idx < 0 || idx >= array_len {
        return Ok(NULL);
    }

    // idx is guaranteed to be within bounds
    Ok(array.elements[idx as usize].clone())
}

fn eval_hash_literal(
    hl: &HashLiteral,
    env: Rc<RefCell<Environment>>,
) -> Result<ObjectTypes, EvaluationError> {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in &hl.pairs {
        let key = eval(key_node, env.clone())?;

        let hash_key = match &key {
            ObjectTypes::StringLiteral(s) => s.hash_key(),
            ObjectTypes::Integer(i) => i.hash_key(),
            ObjectTypes::Boolean(b) => b.hash_key(),
            _ => {
                return Err(new_evaluation_error(&format!(
                    "unusable as hash key: {}",
                    key.ty()
                )));
            }
        };

        let value = eval(value_node, env.clone())?;

        pairs.insert(hash_key, HashPair { key, value });
    }

    Ok(ObjectTypes::Hash(Hash { pairs }))
}

fn eval_hash_index_expression(
    left: &ObjectTypes,
    index: &ObjectTypes,
) -> Result<ObjectTypes, EvaluationError> {
    let hash_object: &Hash = match left {
        ObjectTypes::Hash(h) => h,
        _ => return Err(new_evaluation_error("left is not a hash")),
    };

    let key = match index {
        ObjectTypes::StringLiteral(s) => s.hash_key(),
        ObjectTypes::Integer(i) => i.hash_key(),
        ObjectTypes::Boolean(b) => b.hash_key(),
        _ => {
            return Err(new_evaluation_error(&format!(
                "unusable as hash key: {}",
                index.ty()
            )));
        }
    };

    Ok(match hash_object.pairs.get(&key) {
        Some(pair) => pair.value.clone(),
        None => NULL,
    })
}
