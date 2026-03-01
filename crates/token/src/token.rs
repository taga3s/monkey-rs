use std::fmt;

use utils::context::Context;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub ty: TokenType,
    pub literal: Literal,
}

impl Token {
    pub fn new(start: usize, end: usize) -> Self {
        Token {
            ty: TokenType::ILLEGAL,
            literal: Literal::new(start, end),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    ILLEGAL,
    EOF,
    IDENT,
    INT,
    STRING,
    // operators
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    EQ,
    NOTEQ,
    // delimiters
    COMMA,
    SEMICOLON,
    COLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    // keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let out = match self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::STRING => "STRING",
            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::BANG => "!",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",
            TokenType::LT => "<",
            TokenType::GT => ">",
            TokenType::EQ => "==",
            TokenType::NOTEQ => "!=",
            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",
            TokenType::COLON => ":",
            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::LBRACKET => "[",
            TokenType::RBRACKET => "]",
            TokenType::FUNCTION => "fn",
            TokenType::LET => "let",
            TokenType::TRUE => "true",
            TokenType::FALSE => "false",
            TokenType::IF => "if",
            TokenType::ELSE => "else",
            TokenType::RETURN => "return",
        };
        write!(f, "{}", out)
    }
}

const KEYWORDS: [(&str, TokenType); 7] = [
    ("fn", TokenType::FUNCTION),
    ("let", TokenType::LET),
    ("true", TokenType::TRUE),
    ("false", TokenType::FALSE),
    ("if", TokenType::IF),
    ("else", TokenType::ELSE),
    ("return", TokenType::RETURN),
];

pub fn lookup_ident(ident: &str) -> TokenType {
    if let Some((_, token_type)) = KEYWORDS.iter().find(|(key, _)| *key == ident) {
        return *token_type;
    }
    TokenType::IDENT
}

#[derive(Eq, PartialEq, Debug, Clone, Copy, Hash)]
pub struct Literal {
    pub start: usize,
    pub end: usize,
}

impl Literal {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn with_ref<'ctx>(self, ctx: &'ctx Context) -> LiteralWithRef<'ctx> {
        LiteralWithRef {
            literal: self,
            reference: &ctx.get_source()[self.start..self.end],
        }
    }
}

pub struct LiteralWithRef<'ctx> {
    pub literal: Literal,
    pub reference: &'ctx str,
}

impl<'a> fmt::Display for LiteralWithRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.reference)
    }
}
