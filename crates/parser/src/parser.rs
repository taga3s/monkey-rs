use std::{collections::HashMap, vec};

use ast::ast::{
    ArrayLiteral, BlockStatement, Boolean, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, HashLiteral, Identifier, IfExpression, IndexExpression, InfixExpression,
    IntegerLiteral, LetStatement, Node, PrefixExpression, Program, ReturnStatement, Statement,
    StringLiteral,
};
use lexer::lexer::Lexer;
use token::token::{Token, TokenType};

type PrefixParseFn = fn(&mut Parser) -> Option<Box<Node>>;
type InfixParseFn = fn(&mut Parser, left: Box<Node>) -> Option<Box<Node>>;

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

    pub fn parse_program(&mut self) -> Node {
        let mut stmts = vec![];
        while self.cur_token.ty != TokenType::EOF {
            let stmt = self.parse_statement();
            if let Some(s) = stmt {
                stmts.push(s);
            }
            self.next_token();
        }
        Node::Program(Program { statements: stmts })
    }

    fn parse_statement(&mut self) -> Option<Node> {
        let stmt = match self.cur_token.ty {
            TokenType::LET => self.parse_let_statement(),
            TokenType::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        };
        stmt.map(Node::Statement)
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: None,
            value: None,
        };

        if !self.expect_peek(&TokenType::IDENT) {
            return None;
        }

        stmt.name = Some(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        });

        if !self.expect_peek(&TokenType::ASSIGN) {
            return None;
        }

        self.next_token();

        stmt.value = self.parse_expression(Precedence::LOWEST);

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Let(stmt))
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let mut stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: None,
        };

        self.next_token();

        stmt.return_value = self.parse_expression(Precedence::LOWEST);

        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::Return(stmt))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(Precedence::LOWEST),
        };

        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Some(Statement::ExpressionStatement(stmt))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Box<Node>> {
        let prefix_parse_fn = match self.prefix_parse_fns.get(&self.cur_token.ty) {
            Some(p) => p,
            None => {
                self.no_prefix_parse_fn_error(self.cur_token.ty);
                return None;
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

            left_expr = match left_expr {
                Some(le) => infix_parse_fn(self, le),
                None => return None,
            };
        }

        left_expr
    }

    fn register_prefix(&mut self, tok: TokenType, fn_: PrefixParseFn) {
        self.prefix_parse_fns.insert(tok, fn_);
    }

    fn no_prefix_parse_fn_error(&mut self, tok: TokenType) {
        let msg = format!("no prefix parse function for \"{}\" found", tok);
        self.errors.push(msg);
    }

    fn parse_identifier(&mut self) -> Option<Box<Node>> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };
        Some(Box::new(Node::Expression(Expression::Identifier(ident))))
    }

    fn parse_integer_literal(&mut self) -> Option<Box<Node>> {
        let value = match self.cur_token.literal.parse::<i64>() {
            Ok(v) => v,
            Err(_) => {
                let msg = format!("could not parse {} as integer", self.cur_token.literal);
                self.errors.push(msg);
                return None;
            }
        };

        let ident = IntegerLiteral {
            token: self.cur_token.clone(),
            value,
        };

        Some(Box::new(Node::Expression(Expression::IntegerLiteral(
            ident,
        ))))
    }

    fn parse_string_literal(&mut self) -> Option<Box<Node>> {
        Some(Box::new(Node::Expression(Expression::StringLiteral(
            StringLiteral {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            },
        ))))
    }

    fn parse_boolean(&mut self) -> Option<Box<Node>> {
        Some(Box::new(Node::Expression(Expression::Boolean(Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::TRUE),
        }))))
    }

    fn parse_prefix_expression(&mut self) -> Option<Box<Node>> {
        let mut expression = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        self.next_token();

        expression.right = self.parse_expression(Precedence::PREFIX);

        Some(Box::new(Node::Expression(Expression::Prefix(expression))))
    }

    fn parse_grouped_expression(&mut self) -> Option<Box<Node>> {
        self.next_token();

        let expr = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&TokenType::RPAREN) {
            return None;
        }

        expr
    }

    fn parse_array_literal(&mut self) -> Option<Box<Node>> {
        let elements = self.parse_expression_list(TokenType::RBRACKET)?;
        let array = ArrayLiteral {
            token: self.cur_token.clone(),
            elements,
        };
        Some(Box::new(Node::Expression(Expression::ArrayLiteral(array))))
    }

    #[allow(clippy::vec_box)]
    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<Box<Node>>> {
        let mut list = vec![];

        if self.peek_token_is(&end) {
            self.next_token();
            return Some(list);
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
            return None;
        }

        Some(list)
    }

    fn parse_index_expression(&mut self, left: Box<Node>) -> Option<Box<Node>> {
        let mut expr = IndexExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            index: None,
        };

        self.next_token();
        expr.index = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&TokenType::RBRACKET) {
            return None;
        }

        Some(Box::new(Node::Expression(Expression::IndexExpression(
            expr,
        ))))
    }

    fn register_infix(&mut self, tok: TokenType, fn_: InfixParseFn) {
        self.infix_parse_fns.insert(tok, fn_);
    }

    fn parse_infix_expression(&mut self, left: Box<Node>) -> Option<Box<Node>> {
        let mut expression = InfixExpression {
            token: self.cur_token.clone(),
            left: Some(left),
            operator: self.cur_token.literal.clone(),
            right: None,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        expression.right = self.parse_expression(precedence);

        Some(Box::new(Node::Expression(Expression::Infix(expression))))
    }

    fn parse_function_literal(&mut self) -> Option<Box<Node>> {
        let mut literal = FunctionLiteral {
            token: self.cur_token.clone(),
            parameters: vec![],
            body: None,
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return None;
        }

        literal.parameters = self.parse_function_parameters()?;

        if !self.expect_peek(&TokenType::LBRACE) {
            return None;
        }

        literal.body = self.parse_block_statement();

        Some(Box::new(Node::Expression(Expression::FunctionLiteral(
            literal,
        ))))
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<Node>> {
        let mut idents: Vec<Node> = vec![];

        if self.peek_token_is(&TokenType::RPAREN) {
            self.next_token();
            return Some(idents);
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
            return None;
        }

        Some(idents)
    }

    fn parse_if_expression(&mut self) -> Option<Box<Node>> {
        let mut expression = IfExpression {
            token: self.cur_token.clone(),
            condition: None,
            consequence: None,
            alternative: None,
        };

        if !self.expect_peek(&TokenType::LPAREN) {
            return None;
        }

        self.next_token();
        expression.condition = self.parse_expression(Precedence::LOWEST);

        if !self.expect_peek(&TokenType::RPAREN) {
            return None;
        }

        if !self.expect_peek(&TokenType::LBRACE) {
            return None;
        }

        expression.consequence = self.parse_block_statement();

        if self.peek_token_is(&TokenType::ELSE) {
            self.next_token();

            if !self.expect_peek(&TokenType::LBRACE) {
                return None;
            }

            expression.alternative = self.parse_block_statement();
        }

        Some(Box::new(Node::Expression(Expression::IfExpression(
            expression,
        ))))
    }

    fn parse_block_statement(&mut self) -> Option<Box<Node>> {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBRACE) && !self.cur_token_is(TokenType::EOF) {
            if let Some(stmt) = self.parse_statement() {
                block.statements.push(stmt);
            }
            self.next_token();
        }

        Some(Box::new(Node::Statement(Statement::BlockStatement(block))))
    }

    fn parse_call_expression(&mut self, function: Box<Node>) -> Option<Box<Node>> {
        let arguments = self.parse_expression_list(TokenType::RPAREN)?;
        let expr = CallExpression {
            token: self.cur_token.clone(),
            function,
            arguments,
        };
        Some(Box::new(Node::Expression(Expression::CallExpression(expr))))
    }

    fn parse_hash_literal(&mut self) -> Option<Box<Node>> {
        let mut hash = HashLiteral {
            token: self.cur_token.clone(),
            pairs: HashMap::new(),
        };

        while !self.peek_token_is(&TokenType::RBRACE) {
            self.next_token();
            let key = self.parse_expression(Precedence::LOWEST)?;
            if !self.expect_peek(&TokenType::COLON) {
                return None;
            }

            self.next_token();

            let value = self.parse_expression(Precedence::LOWEST)?;

            hash.pairs.insert(key, value);

            if !self.peek_token_is(&TokenType::RBRACE) && !self.expect_peek(&TokenType::COMMA) {
                return None;
            }
        }

        if !self.expect_peek(&TokenType::RBRACE) {
            return None;
        }

        Some(Box::new(Node::Expression(Expression::HashLiteral(hash))))
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
