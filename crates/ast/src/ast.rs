use std::{collections::HashMap, fmt};

use token::token::{Literal, Token};
use utils::context::Context;

use std::hash::{Hash, Hasher};

#[derive(PartialEq, Clone, Debug)]
pub enum Node<'ctx> {
    Program(Program<'ctx>),
    Statement(Statement<'ctx>),
    Expression(Expression<'ctx>),
}

impl<'ctx> fmt::Display for Node<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Node::Program(p) => write!(f, "{}", p),
            Node::Statement(s) => write!(f, "{}", s),
            Node::Expression(e) => write!(f, "{}", e),
        }
    }
}

impl<'ctx> Eq for Node<'ctx> {}

impl<'ctx> Hash for Node<'ctx> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_string().hash(state); //FIXME: This is not a good hash function
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Statement<'ctx> {
    Let(LetStatement<'ctx>),
    Return(ReturnStatement<'ctx>),
    ExpressionStatement(ExpressionStatement<'ctx>),
    BlockStatement(BlockStatement<'ctx>),
}

impl<'ctx> fmt::Display for Statement<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(s) => write!(f, "{}", s),
            Statement::Return(s) => write!(f, "{}", s),
            Statement::ExpressionStatement(s) => write!(f, "{}", s),
            Statement::BlockStatement(s) => write!(f, "{}", s),
        }
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'ctx> {
    IntegerLiteral(IntegerLiteral<'ctx>),
    StringLiteral(StringLiteral<'ctx>),
    Boolean(Boolean<'ctx>),
    ArrayLiteral(ArrayLiteral<'ctx>),
    IndexExpression(IndexExpression<'ctx>),
    Identifier(Identifier<'ctx>),
    Prefix(PrefixExpression<'ctx>),
    Infix(InfixExpression<'ctx>),
    IfExpression(IfExpression<'ctx>),
    FunctionLiteral(FunctionLiteral<'ctx>),
    CallExpression(CallExpression<'ctx>),
    HashLiteral(HashLiteral<'ctx>),
}

impl<'ctx> fmt::Display for Expression<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::IntegerLiteral(e) => write!(f, "{}", e),
            Expression::StringLiteral(e) => write!(f, "{}", e),
            Expression::Boolean(e) => write!(f, "{}", e),
            Expression::ArrayLiteral(e) => write!(f, "{}", e),
            Expression::IndexExpression(e) => write!(f, "{}", e),
            Expression::Identifier(e) => write!(f, "{}", e),
            Expression::Prefix(e) => write!(f, "{}", e),
            Expression::Infix(e) => write!(f, "{}", e),
            Expression::IfExpression(e) => write!(f, "{}", e),
            Expression::FunctionLiteral(e) => write!(f, "{}", e),
            Expression::CallExpression(e) => write!(f, "{}", e),
            Expression::HashLiteral(e) => write!(f, "{}", e),
        }
    }
}

pub trait TNode {
    fn token_literal(&self) -> Literal;
}

pub trait TStatement
where
    Self: TNode,
{
    fn statement_node(&self);
}

pub trait TExpression
where
    Self: TNode,
{
    fn expression_node(&self);
}

#[derive(PartialEq, Clone, Debug)]
pub struct Program<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub statements: Vec<Node<'ctx>>,
}

impl<'ctx> TNode for Program<'ctx> {
    fn token_literal(&self) -> Literal {
        if !self.statements.is_empty() {
            Literal::new(0, 0) // Program doesn't have a token
        } else {
            Literal::new(0, 0)
        }
    }
}

impl<'ctx> fmt::Display for Program<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("");
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Identifier<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub value: Literal,
}

impl<'ctx> TExpression for Identifier<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for Identifier<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for Identifier<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.with_ref(self.ctx))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct LetStatement<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub name: Option<Identifier<'ctx>>,
    pub value: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TStatement for LetStatement<'ctx> {
    fn statement_node(&self) {}
}

impl<'ctx> TNode for LetStatement<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for LetStatement<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "{} {} = {};",
            self.token_literal().with_ref(self.ctx),
            self.name.as_ref().map_or("".to_string(), |n| n.to_string()),
            self.value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ReturnStatement<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub return_value: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TStatement for ReturnStatement<'ctx> {
    fn statement_node(&self) {}
}

impl<'ctx> TNode for ReturnStatement<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for ReturnStatement<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "{} {};",
            self.token_literal().with_ref(self.ctx),
            self.return_value
                .as_ref()
                .map_or("".to_string(), |v| v.to_string())
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ExpressionStatement<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub expression: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TStatement for ExpressionStatement<'ctx> {
    fn statement_node(&self) {}
}

impl<'ctx> TNode for ExpressionStatement<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for ExpressionStatement<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.expression
                .as_ref()
                .map_or("".to_string(), |e| e.to_string())
        )
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IntegerLiteral<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub value: i64,
}

impl<'ctx> TExpression for IntegerLiteral<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for IntegerLiteral<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for IntegerLiteral<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct StringLiteral<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub value: Literal,
}

impl<'ctx> TExpression for StringLiteral<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for StringLiteral<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for StringLiteral<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value.with_ref(self.ctx))
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct PrefixExpression<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub operator: Literal,
    pub right: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for PrefixExpression<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for PrefixExpression<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for PrefixExpression<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({}{})",
            self.operator.with_ref(self.ctx),
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct InfixExpression<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub left: Option<Box<Node<'ctx>>>,
    pub operator: Literal,
    pub right: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for InfixExpression<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for InfixExpression<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for InfixExpression<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({} {} {})",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.operator.with_ref(self.ctx),
            self.right
                .as_ref()
                .map_or("".to_string(), |r| r.to_string())
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Boolean<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub value: bool,
}

impl<'ctx> TExpression for Boolean<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for Boolean<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for Boolean<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct ArrayLiteral<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub elements: Vec<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for ArrayLiteral<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for ArrayLiteral<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for ArrayLiteral<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IndexExpression<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub left: Option<Box<Node<'ctx>>>,
    pub index: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for IndexExpression<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for IndexExpression<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for IndexExpression<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "({}[{}])",
            self.left.as_ref().map_or("".to_string(), |l| l.to_string()),
            self.index
                .as_ref()
                .map_or("".to_string(), |i| i.to_string())
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct IfExpression<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub condition: Option<Box<Node<'ctx>>>,
    pub consequence: Option<Box<Node<'ctx>>>,
    pub alternative: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for IfExpression<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for IfExpression<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for IfExpression<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = format!(
            "if{}{}{}",
            self.condition
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.consequence
                .as_ref()
                .map_or("".to_string(), |c| c.to_string()),
            self.alternative
                .as_ref()
                .map_or("".to_string(), |a| format!("else{}", a))
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct BlockStatement<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub statements: Vec<Node<'ctx>>,
}

impl<'ctx> TStatement for BlockStatement<'ctx> {
    fn statement_node(&self) {}
}

impl<'ctx> TNode for BlockStatement<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for BlockStatement<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = self
            .statements
            .iter()
            .map(|s| s.to_string())
            .collect::<Vec<String>>()
            .join("");
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct FunctionLiteral<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub parameters: Vec<Node<'ctx>>,
    pub body: Option<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for FunctionLiteral<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for FunctionLiteral<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for FunctionLiteral<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let body = self.body.as_ref().map_or("".to_string(), |b| b.to_string());
        let out = format!(
            "{}({}){{{}}}",
            self.token_literal().with_ref(self.ctx),
            params,
            body
        );
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct CallExpression<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub function: Box<Node<'ctx>>,
    pub arguments: Vec<Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for CallExpression<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for CallExpression<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for CallExpression<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let func = &self.function.to_string();
        let args = self
            .arguments
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>()
            .join(", ");
        let out = format!("{}({})", func, args);
        write!(f, "{}", out)
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct HashLiteral<'ctx> {
    pub ctx: &'ctx Context<'ctx>,
    pub token: Token,
    pub pairs: HashMap<Box<Node<'ctx>>, Box<Node<'ctx>>>,
}

impl<'ctx> TExpression for HashLiteral<'ctx> {
    fn expression_node(&self) {}
}

impl<'ctx> TNode for HashLiteral<'ctx> {
    fn token_literal(&self) -> Literal {
        self.token.literal
    }
}

impl<'ctx> fmt::Display for HashLiteral<'ctx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pairs = self
            .pairs
            .iter()
            .map(|(k, v)| format!("{}: {}", k, v))
            .collect::<Vec<String>>()
            .join(", ");
        let out = format!("{{{}}}", pairs);
        write!(f, "{}", out)
    }
}
