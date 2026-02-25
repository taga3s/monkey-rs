use core::fmt;
use std::error;

#[derive(Debug)]
pub struct EvaluationError {
    pub message: String,
}

impl error::Error for EvaluationError {}

impl fmt::Display for EvaluationError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

pub fn new_evaluation_error(message: &str) -> EvaluationError {
    EvaluationError {
        message: message.to_string(),
    }
}
