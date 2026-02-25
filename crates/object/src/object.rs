//! Object of Evaluation for the Monkey interpreter.

use core::{fmt, str};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::ast::{BlockStatement, Identifier};

use crate::{environment::Environment, error::EvaluationError};

#[derive(PartialEq, Eq, Clone, Hash, Debug)]
pub enum ObjectType {
    IntegerObj,
    StringObj,
    BooleanObj,
    ArrayObj,
    NullObj,
    ReturnValueObj,
    FunctionObj,
    BuiltinObj,
    HashObj,
    ErrorObj,
}

impl fmt::Display for ObjectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ty: &str = match self {
            ObjectType::IntegerObj => "INTEGER",
            ObjectType::StringObj => "STRING",
            ObjectType::BooleanObj => "BOOLEAN",
            ObjectType::ArrayObj => "ARRAY",
            ObjectType::NullObj => "NULL",
            ObjectType::ReturnValueObj => "RETURN_VALUE",
            ObjectType::FunctionObj => "FUNCTION",
            ObjectType::BuiltinObj => "BUILTIN",
            ObjectType::HashObj => "HASH",
            ObjectType::ErrorObj => "ERROR",
        };
        write!(f, "{}", ty)
    }
}

#[derive(Clone)]
pub enum ObjectTypes {
    Integer(Integer),
    StringLiteral(StringLiteral),
    Boolean(Boolean),
    Array(Array),
    Null(Null),
    ReturnValue(ReturnValue),
    Function(Function),
    Builtin(Builtin),
    Hash(Hash),
}

impl ObjectTypes {
    pub fn ty(&self) -> ObjectType {
        match self {
            ObjectTypes::Integer(integer) => integer.ty(),
            ObjectTypes::StringLiteral(string) => string.ty(),
            ObjectTypes::Boolean(boolean) => boolean.ty(),
            ObjectTypes::Array(array) => array.ty(),
            ObjectTypes::Null(null) => null.ty(),
            ObjectTypes::ReturnValue(return_value) => return_value.ty(),
            ObjectTypes::Function(function) => function.ty(),
            ObjectTypes::Builtin(builtin) => builtin.ty(),
            ObjectTypes::Hash(hash) => hash.ty(),
        }
    }

    pub fn as_type(&self, ty: ObjectType) -> bool {
        match self {
            ObjectTypes::Integer(integer) => integer.as_type(ty),
            ObjectTypes::StringLiteral(string) => string.as_type(ty),
            ObjectTypes::Boolean(boolean) => boolean.as_type(ty),
            ObjectTypes::Array(array) => array.as_type(ty),
            ObjectTypes::Null(null) => null.as_type(ty),
            ObjectTypes::ReturnValue(return_value) => return_value.as_type(ty),
            ObjectTypes::Function(function) => function.as_type(ty),
            ObjectTypes::Builtin(builtin) => builtin.as_type(ty),
            ObjectTypes::Hash(hash) => hash.as_type(ty),
        }
    }

    pub fn inspect(&self) -> String {
        match self {
            ObjectTypes::Integer(integer) => integer.inspect(),
            ObjectTypes::StringLiteral(string) => string.inspect(),
            ObjectTypes::Boolean(boolean) => boolean.inspect(),
            ObjectTypes::Array(array) => array.inspect(),
            ObjectTypes::Null(null) => null.inspect(),
            ObjectTypes::ReturnValue(return_value) => return_value.inspect(),
            ObjectTypes::Function(function) => function.inspect(),
            ObjectTypes::Builtin(builtin) => builtin.inspect(),
            ObjectTypes::Hash(hash) => hash.inspect(),
        }
    }
}

trait Object {
    fn ty(&self) -> ObjectType;
    fn as_type(&self, ty: ObjectType) -> bool;
    fn inspect(&self) -> String;
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct HashKey {
    ty: ObjectType,
    value: u64,
}

#[derive(PartialEq, Clone)]
pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn ty(&self) -> ObjectType {
        ObjectType::IntegerObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::IntegerObj
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Integer {
    pub fn hash_key(&self) -> HashKey {
        HashKey {
            ty: self.ty(),
            value: self.value as u64,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct StringLiteral {
    pub value: String,
}

impl Object for StringLiteral {
    fn ty(&self) -> ObjectType {
        ObjectType::StringObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::StringObj
    }

    fn inspect(&self) -> String {
        self.value.to_string()
    }
}

// FNV-1a 64-bit hash function
// ref: https://ssojet.com/hashing/fnv-1a-in-python/
fn fnv1a_64(data: &str) -> u64 {
    let fnv_prime: u64 = 0x100000001b3;
    let mut hash_val: u64 = 0xcbf29ce484222325;
    for byte in data.as_bytes() {
        hash_val ^= *byte as u64;
        hash_val = hash_val.wrapping_mul(fnv_prime);
    }
    hash_val
}

impl StringLiteral {
    pub fn hash_key(&self) -> HashKey {
        let hash = fnv1a_64(&self.value);
        HashKey {
            ty: self.ty(),
            value: hash,
        }
    }
}

#[derive(PartialEq, Clone)]
pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn ty(&self) -> ObjectType {
        ObjectType::BooleanObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::BooleanObj
    }

    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
}

impl Boolean {
    pub fn hash_key(&self) -> HashKey {
        let value = if self.value { 1 } else { 0 };
        HashKey {
            ty: self.ty(),
            value,
        }
    }
}

#[derive(Clone)]
pub struct Array {
    pub elements: Vec<ObjectTypes>,
}

impl Object for Array {
    fn ty(&self) -> ObjectType {
        ObjectType::ArrayObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::ArrayObj
    }

    fn inspect(&self) -> String {
        let elements: Vec<String> = self.elements.iter().map(|e| e.inspect()).collect();
        format!("[{}]", elements.join(", "))
    }
}

#[derive(PartialEq, Clone)]
pub struct Null;

impl Object for Null {
    fn ty(&self) -> ObjectType {
        ObjectType::NullObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::NullObj
    }

    fn inspect(&self) -> String {
        "null".to_string()
    }
}

#[derive(Clone)]
pub struct ReturnValue {
    pub value: Box<ObjectTypes>,
}

impl Object for ReturnValue {
    fn ty(&self) -> ObjectType {
        ObjectType::ReturnValueObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::ReturnValueObj
    }

    fn inspect(&self) -> String {
        self.value.inspect()
    }
}

#[derive(Clone)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
    pub env: Rc<RefCell<Environment>>,
}

impl Object for Function {
    fn ty(&self) -> ObjectType {
        ObjectType::FunctionObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::FunctionObj
    }

    fn inspect(&self) -> String {
        let params: Vec<String> = self.parameters.iter().map(|p| p.to_string()).collect();
        format!("fn({}) {{\n{}\n}}", params.join(", "), self.body)
    }
}

pub type BuiltinFunction = fn(&[ObjectTypes]) -> Result<ObjectTypes, EvaluationError>;

#[derive(Clone)]
pub struct Builtin {
    pub fn_: BuiltinFunction,
}

impl Object for Builtin {
    fn ty(&self) -> ObjectType {
        ObjectType::BuiltinObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::BuiltinObj
    }

    fn inspect(&self) -> String {
        "builtin function".to_string()
    }
}

#[derive(Clone)]
pub struct HashPair {
    pub key: ObjectTypes,
    pub value: ObjectTypes,
}

#[derive(Clone)]
pub struct Hash {
    pub pairs: HashMap<HashKey, HashPair>,
}

impl Object for Hash {
    fn ty(&self) -> ObjectType {
        ObjectType::HashObj
    }

    fn as_type(&self, ty: ObjectType) -> bool {
        ty == ObjectType::HashObj
    }

    fn inspect(&self) -> String {
        let pairs: Vec<String> = self
            .pairs
            .values()
            .map(|pair| format!("{}: {}", pair.key.inspect(), pair.value.inspect()))
            .collect();
        format!("{{{}}}", pairs.join(", "))
    }
}
