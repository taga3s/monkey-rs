use std::{collections::HashMap, vec};

use ast::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
    IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement,
    StringLiteral,
};
use lexer::lexer::Lexer;
use token::token::{Token, TokenType};

use crate::error::{ParseError, new_parse_error};

type PrefixParseFn = fn(&mut Parser) -> Result<Box<Node>, ParseError>;
type InfixParseFn = fn(&mut Parser, left: Box<Node>) -> Result<Box<Node>, ParseError>;

#[derive(Clone, Copy, Debug, PartialEq, PartialOrd)]
pub enum Precedence {
    LOWEST,
    EQUALS,      // ==
    LESSGREATER, // > or <
    SUM,         // +
    PRODUCT,     // *
    PREFIX,      // -X or !X
    CALL,        // myFunction(X)
    INDEX,       // array[index]
}

const PRECEDENCES: [(TokenType, Precedence); 10] = [
    (TokenType::EQ, Precedence::EQUALS),
    (TokenType::NOTEQ, Precedence::EQUALS),
    (TokenType::LT, Precedence::LESSGREATER),
    (TokenType::GT, Precedence::LESSGREATER),
    (TokenType::PLUS, Precedence::SUM),
    (TokenType::MINUS, Precedence::SUM),
    (TokenType::SLASH, Precedence::PRODUCT),
    (TokenType::ASTERISK, Precedence::PRODUCT),
    (TokenType::LPAREN, Precedence::CALL),
    (TokenType::LBRACKET, Precedence::INDEX),
];

pub struct Parser {
    lexer: Lexer,
    errors: Vec<String>,
    cur_token: Token,
    peek_token: Token,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            errors: vec![],
            cur_token: Token::new(),
            peek_token: Token::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix(TokenType::IDENT, Self::parse_identifier);
        parser.register_prefix(TokenType::INT, Self::parse_integer_literal);
        parser.register_prefix(TokenType::STRING, Self::parse_string_literal);
        parser.register_prefix(TokenType::TRUE, Self::parse_boolean);
        parser.register_prefix(TokenType::FALSE, Self::parse_boolean);
        parser.register_prefix(TokenType::BANG, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::MINUS, Self::parse_prefix_expression);
        parser.register_prefix(TokenType::LPAREN, Self::parse_grouped_expression);
        parser.register_prefix(TokenType::LBRACKET, Self::parse_array_literal);
        parser.register_prefix(TokenType::IF, Self::parse_if_expression);
        parser.register_prefix(TokenType::FUNCTION, Self::parse_function_literal);
        parser.register_prefix(TokenType::LBRACE, Self::parse_hash_literal);

        parser.register_infix(TokenType::PLUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::MINUS, Self::parse_infix_expression);
        parser.register_infix(TokenType::SLASH, Self::parse_infix_expression);
        parser.register_infix(TokenType::ASTERISK, Self::parse_infix_expression);
        parser.register_infix(TokenType::EQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::NOTEQ, Self::parse_infix_expression);
        parser.register_infix(TokenType::LT, Self::parse_infix_expression);
        parser.register_infix(TokenType::GT, Self::parse_infix_expression);
        parser.register_infix(TokenType::LPAREN, Self::parse_call_expression);
        parser.register_infix(TokenType::LBRACKET, Self::parse_index_expression);

        parser.next_token();
        parser.next_token();

        parser
    }

    pub fn errors(&self) -> &[String] {
        &self.errors
    }

    fn peek_error(&mut self, tok: &TokenType) {
        let msg = format!(
            "expected next token to be \"{}\", got \"{}\" instead",
            tok, self.peek_token.ty
        );
        self.errors.push(msg);
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Result<Node, ParseError> {
        let mut stmts = vec![];
        while self.cur_token.ty != TokenType::EOF {
            let stmt = self.parse_statement()?;
            stmts.push(stmt);
            self.next_token();
        }
        Ok(Node::Program(Program { statements: stmts }))
    }

    fn parse_statement(&mut self) -> Result<Node, ParseError> {
        let stmt = match self.cur_token.ty {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
        stmt.map(Node::Statement)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, ParseError> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(&TokenType::IDENT) {
            return Err(new_parse_error(
                "[internal:parser] expected identifier after 'let' keyword",
            ));
        }

        stmt.name = Some(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(&TokenType::ASSIGN) {
            return Err(new_parse_error(
                "[internal:parser] expected '=' after identifier in let statement",
            ));
        }

        self.next_token();

        let value = self.parse_expression(Precedence::LOWEST)?;
        stmt.value = Some(value);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Let(stmt))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, ParseError> {
        let mut stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        let return_value = self.parse_expression(Precedence::LOWEST)?;
        stmt.return_value = Some(return_value);

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::Return(stmt))
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, ParseError> {
        let expr = self.parse_expression(Precedence::LOWEST)?;
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: Some(expr),
        };

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Statement::ExpressionStatement(stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Box<Node>, ParseError> {
        let prefix_parse_fn = match self.prefix_parse_fns.get(&self.cur_token.ty) {
            Some(p) => p,
            None => {
                return Err(new_parse_error(&format!(
                    "[internal:parser] no prefix parse function for token type '{}'",
                    self.cur_token.ty
                )));
            }
        };

        let mut left_expr = prefix_parse_fn(self);

        // 右結合力が高い場合、 left_exp が次の演算子に関連づけられている infix_parse_fn に渡されることはない。
        while !self.peek_token_is(&TokenType::SEMICOLON) && precedence < self.peek_precedence() {
            let infix_parse_fn = match self.infix_parse_fns.get(&self.peek_token.ty) {
                Some(f) => *f,
                None => return left_expr,
            };

            self.next_token();

            left_expr = infix_parse_fn(self, left_expr?);
        }

        left_expr
    }

    fn register_prefix(&mut self, tok: TokenType, fn_: PrefixParseFn) {
        self.prefix_parse_fns.insert(tok, fn_);
    }

    fn parse_identifier(&mut self) -> Result<Box<Node>, ParseError> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        Ok(Box::new(Node::Expression(Expression::Identifier(ident))))
    }

    fn parse_integer_literal(&mut self) -> Result<Box<Node>, ParseError> {
        let value = match self.cur_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                return Err(new_parse_error(&format!(
                    "[internal:parser] could not parse '{}' as integer",
                    self.cur_token.literal
                )));
            }
        };

        let ident = IntegerLiteral {
            token: self.cur_token.clone(),
            value,
        };

        Ok(Box::new(Node::Expression(Expression::IntegerLiteral(
            ident,
        ))))
    }

    fn parse_string_literal(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Expression(Expression::StringLiteral(
            StringLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            },
        ))))
    }

    fn parse_boolean(&mut self) -> Result<Box<Node>, ParseError> {
        Ok(Box::new(Node::Expression(Expression::Boolean(Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE),
        }))))
    }

    fn parse_prefix_expression(&mut self) -> Result<Box<Node>, ParseError> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        self.next_token();

        let right = self.parse_expression(Precedence::PREFIX)?;
        expression.right = Some(right);

        Ok(Box::new(Node::Expression(Expression::Prefix(expression))))
    }

    fn parse_grouped_expression(&mut self) -> Result<Box<Node>, ParseError> {
        self.next_token();

        let expr = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(new_parse_error(
                "[internal:parser] expected ')' to close grouped expression",
            ));
        }

        expr
    }

    fn parse_array_literal(&mut self) -> Result<Box<Node>, ParseError> {
        let elements = self.parse_expression_list(TokenType::RBRACKET)?;
        let array = ArrayLiteral {
            token: self.cur_token.clone(),
            elements,
        };
        Ok(Box::new(Node::Expression(Expression::ArrayLiteral(array))))
    }

    #[allow(clippy::vec_box)]
    fn parse_expression_list(&mut self, end: TokenType) -> Result<Vec<Box<Node>>, ParseError> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Ok(list);
        }

        self.next_token();
        let expr = self.parse_expression(Precedence::LOWEST)?;
        list.push(expr);

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let expr = self.parse_expression(Precedence::LOWEST)?;
            list.push(expr);
        }

        if !self.expect_peek(&end) {
            return Err(new_parse_error(&format!(
                "[internal:parser] expected '{}' to close expression list",
                end
            )));
        }

        Ok(list)
    }

    fn parse_index_expression(&mut self, left: Box<Node>) -> Result<Box<Node>, ParseError> {
        let mut expr = IndexExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            index: None,
        };

        self.next_token();
        let index = self.parse_expression(Precedence::LOWEST)?;
        expr.index = Some(index);

        if !self.expect_peek(&TokenType::RBRACKET) {
            return Err(new_parse_error(
                "[internal:parser] expected ']' to close index expression",
            ));
        }

        Ok(Box::new(Node::Expression(Expression::IndexExpression(
            expr,
        ))))
    }

    fn register_infix(&mut self, tok: TokenType, fn_: InfixParseFn) {
        self.infix_parse_fns.insert(tok, fn_);
    }

    fn parse_infix_expression(&mut self, left: Box<Node>) -> Result<Box<Node>, ParseError> {
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        expression.right = Some(right);

        Ok(Box::new(Node::Expression(Expression::Infix(expression))))
    }

    fn parse_function_literal(&mut self) -> Result<Box<Node>, ParseError> {
        let mut literal = FunctionLiteral {
            token: self.cur_token.clone(),
            parameters: vec![],
            body: None,
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return Err(new_parse_error(
                "[internal:parser] expected '(' after 'fn' keyword",
            ));
        }

        literal.parameters = self.parse_function_parameters()?;

        if !self.expect_peek(&TokenType::LBRACE) {
            return Err(new_parse_error(
                "[internal:parser] expected '{' to start function body",
            ));
        }

        let body = self.parse_block_statement()?;
        literal.body = Some(body);

        Ok(Box::new(Node::Expression(Expression::FunctionLiteral(
            literal,
        ))))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Node>, ParseError> {
        let mut idents: Vec<Node> = vec![];

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Ok(idents);
        }

        self.next_token();

        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        idents.push(Node::Expression(Expression::Identifier(ident)));

        while self.peek_token_is(&TokenType::COMMA) {
            self.next_token();
            self.next_token();
            let ident = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };
            idents.push(Node::Expression(Expression::Identifier(ident)));
        }

        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(new_parse_error(
                "[internal:parser] expected ')' to close function parameters",
            ));
        }

        Ok(idents)
    }

    fn parse_if_expression(&mut self) -> Result<Box<Node>, ParseError> {
        let mut expression = IfExpression {
            token: self.cur_token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return Err(new_parse_error(
                "[internal:parser] expected '(' after 'if' keyword",
            ));
        }

        self.next_token();
        let condition = self.parse_expression(Precedence::LOWEST)?;
        expression.condition = Some(condition);

        if !self.expect_peek(&TokenType::RPAREN) {
            return Err(new_parse_error(
                "[internal:parser] expected ')' to close if condition",
            ));
        }

        if !self.expect_peek(&TokenType::LBRACE) {
            return Err(new_parse_error(
                "[internal:parser] expected '{' to start if consequence block",
            ));
        }

        let consequence = self.parse_block_statement()?;
        expression.consequence = Some(consequence);

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(&TokenType::LBRACE) {
                return Err(new_parse_error(
                    "[internal:parser] expected '{' to start else block",
                ));
            }

            let alter = self.parse_block_statement()?;
            expression.alternative = Some(alter);
        }

        Ok(Box::new(Node::Expression(Expression::IfExpression(
            expression,
        ))))
    }

    fn parse_block_statement(&mut self) -> Result<Box<Node>, ParseError> {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        loop {
            if self.cur_token_is(TokenType::RBRACE) {
                break;
            }

            if self.cur_token_is(TokenType::EOF) {
                return Err(new_parse_error(
                    "[internal:parser] expected '}' to end block",
                ));
            }

            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Ok(Box::new(Node::Statement(Statement::BlockStatement(block))))
    }

    fn parse_call_expression(&mut self, function: Box<Node>) -> Result<Box<Node>, ParseError> {
        let arguments = self.parse_expression_list(TokenType::RPAREN)?;
        let expr = CallExpression {
            token: self.cur_token.clone(),
            function,
            arguments,
        };
        Ok(Box::new(Node::Expression(Expression::CallExpression(expr))))
    }

    fn parse_hash_literal(&mut self) -> Result<Box<Node>, ParseError> {
        let mut hash = HashLiteral {
            token: self.cur_token.clone(),
            pairs: HashMap::new(),
        };

        while !self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST)?;
            if !self.expect_peek(&TokenType::COLON) {
                return Err(new_parse_error(
                    "[internal:parser] expected ':' after hash key",
                ));
            }

            self.next_token();

            let value = self.parse_expression(Precedence::LOWEST)?;

            hash.pairs.insert(key, value);

            if !self.peek_token_is(&TokenType::RBRACE) && !self.expect_peek(&TokenType::COMMA) {
                return Err(new_parse_error(
                    "[internal:parser] expected ',' to separate hash pairs",
                ));
            }
        }

        if !self.expect_peek(&TokenType::RBRACE) {
            return Err(new_parse_error(
                "[internal:parser] expected '}' to close hash literal",
            ));
        }

        Ok(Box::new(Node::Expression(Expression::HashLiteral(hash))))
    }

    fn cur_token_is(&self, tok: TokenType) -> bool {
        self.cur_token.ty == tok
    }

    fn peek_token_is(&self, tok: &TokenType) -> bool {
        self.peek_token.ty == *tok
    }

    fn expect_peek(&mut self, tok: &TokenType) -> bool {
        if self.peek_token_is(tok) {
            self.next_token();
            true
        } else {
            self.peek_error(tok);
            false
        }
    }

    fn peek_precedence(&self) -> Precedence {
        match PRECEDENCES.iter().find(|(t, _)| *t == self.peek_token.ty) {
            Some((_, p)) => *p,
            None => Precedence::LOWEST,
        }
    }

    fn cur_precedence(&self) -> Precedence {
        match PRECEDENCES.iter().find(|(t, _)| t == &self.cur_token.ty) {
            Some((_, p)) => *p,
            None => Precedence::LOWEST,
        }
    }
}
