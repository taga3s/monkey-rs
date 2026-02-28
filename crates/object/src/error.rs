use core::fmt;
use std::error;

#[derive(Debug)]
pub struct EvaluateError {
    pub message: String,
}

impl error::Error for EvaluateError {}

impl fmt::Display for EvaluateError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn new_evaluate_error(message: &str) -> EvaluateError {
    EvaluateError {
        message: message.to_string(),
    }
}
