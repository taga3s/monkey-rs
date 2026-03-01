use ::token::token::{Token, TokenType, lookup_ident};

pub struct Lexer {
    input_chars: Vec<char>,
    position: usize,
    read_position: usize,
    ch: Option<char>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut lexer = Lexer {
            input_chars: input.chars().collect(),
            position: 0,
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
                    if self.peek_char().is_some_and(|c| c == '=') {
                        self.read_char();
                        if let Some(next) = self.ch {
                            self.new_token(TokenType::EQ, format!("{}{}", current, next))
                        } else {
                            self.new_token(TokenType::ILLEGAL, current)
                        }
                    } else {
                        self.new_token(TokenType::ASSIGN, current)
                    }
                }
                ';' => self.new_token(TokenType::SEMICOLON, current),
                ':' => self.new_token(TokenType::COLON, current),
                '(' => self.new_token(TokenType::LPAREN, current),
                ')' => self.new_token(TokenType::RPAREN, current),
                '[' => self.new_token(TokenType::LBRACKET, current),
                ']' => self.new_token(TokenType::RBRACKET, current),
                ',' => self.new_token(TokenType::COMMA, current),
                '+' => self.new_token(TokenType::PLUS, current),
                '-' => self.new_token(TokenType::MINUS, current),
                '!' => {
                    if self.peek_char().is_some_and(|c| c == '=') {
                        self.read_char();
                        if let Some(next) = self.ch {
                            self.new_token(TokenType::NOTEQ, format!("{}{}", current, next))
                        } else {
                            self.new_token(TokenType::ILLEGAL, current)
                        }
                    } else {
                        self.new_token(TokenType::BANG, current)
                    }
                }
                '*' => self.new_token(TokenType::ASTERISK, current),
                '<' => self.new_token(TokenType::LT, current),
                '>' => self.new_token(TokenType::GT, current),
                '/' => {
                    if self.peek_char().is_some_and(|c| c == '/') {
                        self.skip_comment();
                        return self.next_token();
                    } else {
                        self.new_token(TokenType::SLASH, current)
                    }
                }
                '{' => self.new_token(TokenType::LBRACE, current),
                '}' => self.new_token(TokenType::RBRACE, current),
                '"' => {
                    let literal = self.read_string();
                    self.new_token(TokenType::STRING, literal)
                }
                _ => {
                    if self.is_letter(current) {
                        let literal = self.read_identifier();
                        let token_type = lookup_ident(&literal);
                        // Should early return because we have already read_char() in read_identifier()
                        return self.new_token(token_type, literal);
                    } else if self.is_digit(current) {
                        let literal = self.read_number();
                        // Should early return because we have already read_char() in read_number()
                        return self.new_token(TokenType::INT, literal);
                    } else {
                        self.new_token(TokenType::ILLEGAL, current)
                    }
                }
            }
        } else {
            self.new_token(TokenType::EOF, "")
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
        self.position = self.read_position;
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

    fn skip_comment(&mut self) {
        loop {
            self.read_char();
            if self.ch.is_some_and(|c| c == '\n') || self.ch.is_none() {
                break;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        while let Some(c) = self.ch {
            if !self.is_letter(c) {
                break;
            }
            self.read_char();
        }
        self.input_chars[position..self.position].iter().collect()
    }

    fn read_number(&mut self) -> String {
        let position = self.position;
        while let Some(c) = self.ch {
            if !self.is_digit(c) {
                break;
            }
            self.read_char();
        }
        self.input_chars[position..self.position].iter().collect()
    }

    fn read_string(&mut self) -> String {
        let position = self.position + 1;
        loop {
            self.read_char();
            if self.ch.is_some_and(|c| c == '"') || self.ch.is_none() {
                break;
            }
        }
        self.input_chars[position..self.position].iter().collect()
    }

    fn new_token<T: Into<String>>(&self, token_type: TokenType, ch: T) -> Token {
        Token {
            ty: token_type,
            literal: ch.into(),
        }
    }

    fn is_letter(&self, ch: char) -> bool {
        ch.is_alphabetic() || ch == '_'
    }

    fn is_digit(&self, ch: char) -> bool {
        ch.is_ascii_digit()
    }
}
