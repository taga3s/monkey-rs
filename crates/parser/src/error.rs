use core::fmt;
use std::error;

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
}

impl error::Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn new_parse_error(message: &str) -> ParseError {
    ParseError {
        message: message.to_string(),
    }
}
