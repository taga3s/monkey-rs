use object::{
    error::EvaluationError,
    object::{Builtin, ObjectTypes},
};

pub static BUILTINS: [(&str, Builtin); 6] = [
    ("len", Builtin { fn_: len_builtin }),
    ("first", Builtin { fn_: first_builtin }),
    ("last", Builtin { fn_: last_builtin }),
    ("rest", Builtin { fn_: rest_builtin }),
    ("push", Builtin { fn_: push_builtin }),
    ("log", Builtin { fn_: puts_log }),
];

fn len_builtin(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    if args.len() != 1 {
        return Err(EvaluationError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::StringLiteral(string) => Ok(ObjectTypes::Integer(object::object::Integer {
            value: string.value.len() as i64,
        })),
        ObjectTypes::Array(array) => Ok(ObjectTypes::Integer(object::object::Integer {
            value: array.elements.len() as i64,
        })),
        _ => Err(EvaluationError {
            message: format!("argument to `len` not supported, got {}", args[0].ty()),
        }),
    }
}

fn first_builtin(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    if args.len() != 1 {
        return Err(EvaluationError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                Ok(array.elements[0].clone())
            } else {
                Ok(ObjectTypes::Null(object::object::Null {}))
            }
        }
        _ => Err(EvaluationError {
            message: format!("argument to `first` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn last_builtin(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    if args.len() != 1 {
        return Err(EvaluationError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                Ok(array.elements[array.elements.len() - 1].clone())
            } else {
                Ok(ObjectTypes::Null(object::object::Null {}))
            }
        }
        _ => Err(EvaluationError {
            message: format!("argument to `last` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn rest_builtin(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    if args.len() != 1 {
        return Err(EvaluationError {
            message: format!("wrong number of arguments. got={}, want=1", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            if !array.elements.is_empty() {
                let new_elements = array.elements[1..].to_vec();
                Ok(ObjectTypes::Array(object::object::Array {
                    elements: new_elements,
                }))
            } else {
                Ok(ObjectTypes::Null(object::object::Null {}))
            }
        }
        _ => Err(EvaluationError {
            message: format!("argument to `rest` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn push_builtin(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    if args.len() != 2 {
        return Err(EvaluationError {
            message: format!("wrong number of arguments. got={}, want=2", args.len()),
        });
    }

    match &args[0] {
        ObjectTypes::Array(array) => {
            let mut new_elements = array.elements.clone();
            new_elements.push(args[1].clone());
            Ok(ObjectTypes::Array(object::object::Array {
                elements: new_elements,
            }))
        }
        _ => Err(EvaluationError {
            message: format!("argument to `push` must be ARRAY, got {}", args[0].ty()),
        }),
    }
}

fn puts_log(args: &[ObjectTypes]) -> Result<ObjectTypes, EvaluationError> {
    for arg in args {
        println!("{}", arg.inspect());
    }
    Ok(ObjectTypes::Null(object::object::Null {}))
}
