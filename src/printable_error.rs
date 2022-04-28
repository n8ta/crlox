use std::fmt::{Display, Formatter};
use crate::SourceRef;

#[derive(Debug)]
pub struct PrintableError {
    message: String,
    src: SourceRef,
}

impl Display for PrintableError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}\n{}", self.message, self.src))
    }
}

impl PrintableError {
    pub fn new(message: String, src: SourceRef) -> PrintableError {
        PrintableError {
            message,
            src,
        }
    }
}