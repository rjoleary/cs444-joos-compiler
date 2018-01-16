use std::result;
use std::fmt;

pub type Result<T> = result::Result<T, Error>;

pub struct Error {
    message: &'static str,
    line: usize,
    col: usize
}

impl Error {
    pub fn new(message: &'static str, line: usize, col: usize) -> Error {
        Error { message: message, line: line, col: col }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{}, line {}, col {}",
               self.message, self.line, self.col)
    }
}
