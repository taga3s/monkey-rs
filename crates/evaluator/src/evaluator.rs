use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::ast::{
    BlockStatement, Expression, HashLiteral, Identifier, IfExpression, Node, Program, Statement,
};
use object::{
    environment::Environment,
    object::{
        Array, Boolean, Function, Hash, HashPair, Integer, Null, ObjectType, ObjectTypes,
        ReturnValue, StringLiteral,
    },
};

use crate::builtins::BUILTINS;

const NULL: ObjectTypes = ObjectTypes::Null(Null {});
const TRUE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: true });
const FALSE: ObjectTypes = ObjectTypes::Boolean(Boolean { value: false });

pub fn eval(node: &Node, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    let result = match node {
        Node::Program(program) => eval_program(program, env),
        Node::Expression(expr) => match expr {
            Expression::IntegerLiteral(il) => ObjectTypes::Integer(Integer { value: il.value }),
            Expression::StringLiteral(sl) => ObjectTypes::StringLiteral(StringLiteral {
                value: sl.value.clone(),
            }),
            Expression::Boolean(boolean) => native_bool_to_boolean_object(boolean.value),
            Expression::ArrayLiteral(al) => {
                let elements = eval_expressions(&al.elements, env);
                if elements.len() == 1 && is_error(&elements[0]) {
                    return elements[0].clone();
                }
                ObjectTypes::Array(Array { elements })
            }
            Expression::IndexExpression(ie) => {
                let left = eval(ie.left.as_ref().unwrap(), env.clone());
                if is_error(&left) {
                    return left;
                }
                let index = eval(ie.index.as_ref().unwrap(), env.clone());
                if is_error(&index) {
                    return index;
                }
                return eval_index_expression(&left, &index);
            }
            Expression::Identifier(ident) => eval_identifier(ident, env.clone()),
            Expression::Infix(infix) => {
                let left = eval(infix.left.as_ref().unwrap(), env.clone());
                if is_error(&left) {
                    return left;
                }
                let right = eval(infix.right.as_ref().unwrap(), env.clone());
                if is_error(&right) {
                    return right;
                }
                return eval_infix_expression(&infix.operator, &left, &right);
            }
            Expression::Prefix(prefix) => {
                let right = eval(prefix.right.as_ref().unwrap(), env);
                if is_error(&right) {
                    return right;
                }
                return eval_prefix_expression(&prefix.operator, &right);
            }
            Expression::IfExpression(ifexp) => eval_if_expression(ifexp, env),
            Expression::FunctionLiteral(fl) => {
                let mut parameters = vec![];
                for p in &fl.parameters {
                    match p {
                        Node::Expression(Expression::Identifier(ident)) => {
                            parameters.push(ident.clone())
                        }
                        _ => {
                            return new_error("function parameter is not an identifier");
                        }
                    }
                }
                let body = match fl.body.as_ref().unwrap().as_ref() {
                    Node::Statement(Statement::BlockStatement(bs)) => bs,
                    _ => {
                        return new_error("function body is not a block statement");
                    }
                };
                return ObjectTypes::Function(Function {
                    parameters,
                    body: body.clone(),
                    env,
                });
            }
            Expression::CallExpression(ce) => {
                let func = eval(ce.function.as_ref(), env.clone());
                if is_error(&func) {
                    return func;
                }
                let args = eval_expressions(&ce.arguments, env.clone());
                if args.len() == 1 && is_error(&args[0]) {
                    return args[0].clone();
                }
                return apply_function(&func, &args);
            }
            Expression::HashLiteral(hl) => eval_hash_literal(hl, env),
        },
        Node::Statement(stmt) => match stmt {
            Statement::ExpressionStatement(es) => eval(es.expression.as_ref().unwrap(), env),
            Statement::Let(ls) => {
                let val = eval(ls.value.as_ref().unwrap(), env.clone());
                if is_error(&val) {
                    return val;
                }
                env.borrow_mut().set(&ls.name.as_ref().unwrap().value, val);
                NULL
            }
            Statement::Return(rs) => {
                let val = eval(rs.return_value.as_ref().unwrap(), env);
                if is_error(&val) {
                    return val;
                }
                return ObjectTypes::ReturnValue(ReturnValue {
                    value: Box::new(val),
                });
            }
            Statement::BlockStatement(bs) => eval_block_statement(bs, env),
        },
    };
    result
}

fn eval_program(program: &Program, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    let mut result = NULL; //FIXME: handle other object types

    for stmt in &program.statements {
        result = eval(stmt, env.clone());
        match result {
            ObjectTypes::ReturnValue(return_value) => {
                return *return_value.value;
            }
            ObjectTypes::Error(_) => return result,
            _ => {}
        }
    }
    result
}

fn eval_block_statement(bs: &BlockStatement, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    let mut result = NULL;

    for stmt in &bs.statements {
        result = eval(stmt, env.clone());
        if result.as_type(ObjectType::ReturnValueObj) || result.as_type(ObjectType::ErrorObj) {
            return result;
        }
    }
    result
}

fn new_error(message: &str) -> ObjectTypes {
    ObjectTypes::Error(object::object::Error {
        message: message.to_string(),
    })
}

fn is_error(obj: &ObjectTypes) -> bool {
    obj.as_type(ObjectType::ErrorObj)
}

fn native_bool_to_boolean_object(input: bool) -> ObjectTypes {
    if input { TRUE } else { FALSE }
}

fn eval_prefix_expression(operator: &str, right: &ObjectTypes) -> ObjectTypes {
    match operator {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => new_error(&format!("unknown operator: {}{}", operator, right.ty())),
    }
}

fn eval_identifier(node: &Identifier, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    if let Some(val) = env.borrow().get(&node.value) {
        return val;
    }

    if let Some((_, builtin)) = BUILTINS.iter().find(|(name, _)| *name == node.value) {
        return ObjectTypes::Builtin(builtin.clone());
    }

    new_error(&format!("identifier not found: {}", node.value))
}

fn eval_bang_operator_expression(right: &ObjectTypes) -> ObjectTypes {
    match right {
        ObjectTypes::Boolean(boolean) => {
            if boolean.value {
                FALSE
            } else {
                TRUE
            }
        }
        ObjectTypes::Null(_) => TRUE,
        _ => FALSE,
    }
}

fn eval_minus_prefix_operator_expression(right: &ObjectTypes) -> ObjectTypes {
    if right.ty() != ObjectType::IntegerObj {
        return new_error(&format!("unknown operator: -{}", right.ty()));
    }

    match right {
        ObjectTypes::Integer(integer) => ObjectTypes::Integer(Integer {
            value: -integer.value,
        }),
        _ => new_error(&format!("unknown operator: -{}", right.ty())),
    }
}

fn eval_infix_expression(operator: &str, left: &ObjectTypes, right: &ObjectTypes) -> ObjectTypes {
    if left.as_type(ObjectType::IntegerObj) && right.as_type(ObjectType::IntegerObj) {
        return eval_integer_infix_expression(operator, left, right);
    };
    if left.as_type(ObjectType::StringObj) && right.as_type(ObjectType::StringObj) {
        return eval_string_infix_expression(operator, left, right);
    };
    if left.ty() != right.ty() {
        return new_error(&format!(
            "type mismatch: {} {} {}",
            left.ty(),
            operator,
            right.ty()
        ));
    }
    if operator == "==" {
        return native_bool_to_boolean_object(left == right);
    }
    if operator == "!=" {
        return native_bool_to_boolean_object(left.inspect() != right.inspect());
    }

    new_error(&format!(
        "unknown operator: {} {} {}",
        left.ty(),
        operator,
        right.ty()
    ))
}

fn eval_integer_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> ObjectTypes {
    let left_val = match &left {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("left is not an integer"),
    };
    let right_val = match &right {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("right is not an integer"),
    };

    match operator {
        "+" => ObjectTypes::Integer(Integer {
            value: left_val + right_val,
        }),
        "-" => ObjectTypes::Integer(Integer {
            value: left_val - right_val,
        }),
        "*" => ObjectTypes::Integer(Integer {
            value: left_val * right_val,
        }),
        "/" => {
            if right_val == 0 {
                return new_error(&format!(
                    "attempt to divide by zero: {} {} {}",
                    left_val, operator, right_val
                ));
            }
            ObjectTypes::Integer(Integer {
                value: left_val / right_val,
            })
        }
        "<" => native_bool_to_boolean_object(left_val < right_val),
        ">" => native_bool_to_boolean_object(left_val > right_val),
        "==" => native_bool_to_boolean_object(left_val == right_val),
        "!=" => native_bool_to_boolean_object(left_val != right_val),
        _ => new_error(&format!(
            "unknown operator: {} {} {}",
            &left.ty(),
            operator,
            &right.ty()
        )),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: &ObjectTypes,
    right: &ObjectTypes,
) -> ObjectTypes {
    let left_val = match &left {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return new_error("left is not a string"),
    };
    let right_val = match &right {
        ObjectTypes::StringLiteral(string) => string.value.clone(),
        _ => return new_error("right is not a string"),
    };

    match operator {
        "+" => ObjectTypes::StringLiteral(StringLiteral {
            value: format!("{}{}", left_val, right_val),
        }),
        _ => new_error(&format!(
            "unknown operator: {} {} {}",
            &left.ty(),
            operator,
            &right.ty()
        )),
    }
}

fn is_truthy(obj: &ObjectTypes) -> bool {
    match obj {
        ObjectTypes::Boolean(boolean) => boolean.value,
        ObjectTypes::Null(_) => false,
        _ => true,
    }
}

fn eval_if_expression(ie: &IfExpression, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    let condition = eval(ie.condition.as_ref().unwrap(), env.clone());
    if is_truthy(&condition) {
        return eval(ie.consequence.as_ref().unwrap(), env.clone());
    }
    if let Some(alt) = &ie.alternative {
        return eval(alt, env.clone());
    }
    NULL
}

fn eval_expressions(exps: &[Box<Node>], env: Rc<RefCell<Environment>>) -> Vec<ObjectTypes> {
    let mut result = vec![];

    for e in exps {
        let evaluated = eval(e.as_ref(), env.clone());
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }
    result
}

fn apply_function(func: &ObjectTypes, args: &[ObjectTypes]) -> ObjectTypes {
    if let ObjectTypes::Function(function) = func {
        let extended_env = extend_function_env(function, args);
        let evaluated = eval(
            &Node::Statement(Statement::BlockStatement(function.body.clone())),
            extended_env,
        );
        return unwrap_return_value(evaluated);
    }

    if let ObjectTypes::Builtin(builtin) = func {
        return (builtin.fn_)(args);
    }

    new_error(&format!("not a function: {}", func.ty()))
}

fn extend_function_env(func: &Function, args: &[ObjectTypes]) -> Rc<RefCell<Environment>> {
    let env = Environment::new_enclosed(func.env.clone());

    for (param_idx, param) in func.parameters.iter().enumerate() {
        env.borrow_mut().set(&param.value, args[param_idx].clone());
    }

    env
}

fn unwrap_return_value(obj: ObjectTypes) -> ObjectTypes {
    if obj.as_type(ObjectType::ReturnValueObj) {
        if let ObjectTypes::ReturnValue(rv) = obj {
            return *rv.value;
        }
    }
    obj
}

fn eval_index_expression(left: &ObjectTypes, index: &ObjectTypes) -> ObjectTypes {
    if left.as_type(ObjectType::ArrayObj) && index.as_type(ObjectType::IntegerObj) {
        return eval_array_expression(left, index);
    }
    if left.as_type(ObjectType::HashObj) {
        return eval_hash_index_expression(left, index);
    }

    new_error(&format!("index operator not supported: {}", left.ty()))
}

fn eval_array_expression(left: &ObjectTypes, index: &ObjectTypes) -> ObjectTypes {
    let array = match left {
        ObjectTypes::Array(array) => array,
        _ => return new_error("left is not an array"),
    };
    let idx = match index {
        ObjectTypes::Integer(integer) => integer.value,
        _ => return new_error("index is not an integer"),
    };
    let max = array.elements.len() as i64 - 1;

    if idx < 0 || idx > max {
        return NULL;
    }

    array.elements[idx as usize].clone()
}

fn eval_hash_literal(hl: &HashLiteral, env: Rc<RefCell<Environment>>) -> ObjectTypes {
    let mut pairs = HashMap::new();

    for (key_node, value_node) in &hl.pairs {
        let key = eval(key_node, env.clone());
        if is_error(&key) {
            return key;
        }

        let hash_key = match &key {
            ObjectTypes::StringLiteral(s) => s.hash_key(),
            ObjectTypes::Integer(i) => i.hash_key(),
            ObjectTypes::Boolean(b) => b.hash_key(),
            _ => {
                return new_error(&format!("unusable as hash key: {}", key.ty()));
            }
        };

        let value = eval(value_node, env.clone());
        if is_error(&value) {
            return value;
        }

        pairs.insert(hash_key, HashPair { key, value });
    }

    ObjectTypes::Hash(Hash { pairs })
}

fn eval_hash_index_expression(left: &ObjectTypes, index: &ObjectTypes) -> ObjectTypes {
    let hash_object: &Hash = match left {
        ObjectTypes::Hash(h) => h,
        _ => return new_error("left is not a hash"),
    };

    let key = match index {
        ObjectTypes::StringLiteral(s) => s.hash_key(),
        ObjectTypes::Integer(i) => i.hash_key(),
        ObjectTypes::Boolean(b) => b.hash_key(),
        _ => {
            return new_error(&format!("unusable as hash key: {}", index.ty()));
        }
    };

    match hash_object.pairs.get(&key) {
        Some(pair) => pair.value.clone(),
        None => NULL,
    }
}
