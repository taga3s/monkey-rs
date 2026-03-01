use token::token::{Literal, Token, TokenType, lookup_ident};
use utils::context::Context;

pub struct Lexer<'ctx> {
    ctx: &'ctx Context<'ctx>,
    input_chars: Vec<char>,
    cur_position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl<'ctx> Lexer<'ctx> {
    pub fn new(ctx: &'ctx Context) -> Lexer<'ctx> {
        let mut lexer = Lexer {
            ctx,
            input_chars: ctx.get_source().chars().collect(),
            cur_position: 0,
            read_position: 0,
            ch: None,
        };
        lexer.read_char();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let tok = if let Some(current) = self.ch {
            match current {
                '=' => {
                    let start = self.cur_position;
                    if self.peek_char().is_some_and(|c| c == '=') {
                        self.read_char();
                        if let Some(_next) = self.ch {
                            self.new_token(TokenType::EQ, start, self.cur_position + 1)
                        } else {
                            self.new_token(TokenType::ILLEGAL, start, self.cur_position)
                        }
                    } else {
                        self.new_token(TokenType::ASSIGN, start, self.cur_position + 1)
                    }
                }
                ';' => self.new_token(
                    TokenType::SEMICOLON,
                    self.cur_position,
                    self.cur_position + 1,
                ),
                ':' => self.new_token(TokenType::COLON, self.cur_position, self.cur_position + 1),
                '(' => self.new_token(TokenType::LPAREN, self.cur_position, self.cur_position + 1),
                ')' => self.new_token(TokenType::RPAREN, self.cur_position, self.cur_position + 1),
                '[' => self.new_token(
                    TokenType::LBRACKET,
                    self.cur_position,
                    self.cur_position + 1,
                ),
                ']' => self.new_token(
                    TokenType::RBRACKET,
                    self.cur_position,
                    self.cur_position + 1,
                ),
                ',' => self.new_token(TokenType::COMMA, self.cur_position, self.cur_position + 1),
                '+' => self.new_token(TokenType::PLUS, self.cur_position, self.cur_position + 1),
                '-' => self.new_token(TokenType::MINUS, self.cur_position, self.cur_position + 1),
                '!' => {
                    let start = self.cur_position;
                    if self.peek_char().is_some_and(|c| c == '=') {
                        self.read_char();
                        if let Some(_next) = self.ch {
                            self.new_token(TokenType::NOTEQ, start, self.cur_position + 1)
                        } else {
                            self.new_token(TokenType::ILLEGAL, start, self.cur_position + 1)
                        }
                    } else {
                        self.new_token(TokenType::BANG, start, self.cur_position + 1)
                    }
                }
                '*' => self.new_token(
                    TokenType::ASTERISK,
                    self.cur_position,
                    self.cur_position + 1,
                ),
                '<' => self.new_token(TokenType::LT, self.cur_position, self.cur_position + 1),
                '>' => self.new_token(TokenType::GT, self.cur_position, self.cur_position + 1),
                '/' => self.new_token(TokenType::SLASH, self.cur_position, self.cur_position + 1),
                '{' => self.new_token(TokenType::LBRACE, self.cur_position, self.cur_position + 1),
                '}' => self.new_token(TokenType::RBRACE, self.cur_position, self.cur_position + 1),
                '"' => {
                    let (start, end) = self.read_string();
                    self.new_token(TokenType::STRING, start, end)
                }
                _ => {
                    if self.is_letter(current) {
                        let (start, end) = self.read_identifier();
                        let literal = Literal::new(start, end);
                        let literal_ref = literal.with_ref(self.ctx);
                        let token_type = lookup_ident(literal_ref.reference);
                        // Should early return because we have already read_char() in read_identifier()
                        return self.new_token(token_type, start, end);
                    } else if self.is_digit(current) {
                        let (start, end) = self.read_number();
                        // Should early return because we have already read_char() in read_number()
                        return self.new_token(TokenType::INT, start, end);
                    } else {
                        self.new_token(TokenType::ILLEGAL, 0, 0)
                    }
                }
            }
        } else {
            self.new_token(TokenType::EOF, 0, 0)
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input_chars.len() {
            self.ch = None;
        } else {
            self.ch = Some(self.input_chars[self.read_position]);
        }
        self.cur_position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> Option<char> {
        if self.read_position >= self.input_chars.len() {
            None
        } else {
            Some(self.input_chars[self.read_position])
        }
    }

    fn skip_whitespaces(&mut self) {
        while self.ch.is_some_and(|c| c.is_whitespace()) {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> (usize, usize) {
        let start = self.cur_position;
        while let Some(c) = self.ch {
            if !self.is_letter(c) {
                break;
            }
            self.read_char();
        }
        (start, self.cur_position)
    }

    fn read_number(&mut self) -> (usize, usize) {
        let start = self.cur_position;
        while let Some(c) = self.ch {
            if !self.is_digit(c) {
                break;
            }
            self.read_char();
        }
        (start, self.cur_position)
    }

    fn read_string(&mut self) -> (usize, usize) {
        let start = self.cur_position + 1;
        loop {
            self.read_char();
            if self.ch.is_some_and(|c| c == '"') || self.ch.is_none() {
                break;
            }
        }
        (start, self.cur_position)
    }

    fn new_token(&self, token_type: TokenType, start: usize, end: usize) -> Token {
        Token {
            ty: token_type,
            literal: Literal::new(start, end),
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}
