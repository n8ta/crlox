use std::fmt::{Display, Formatter};
use crate::SourceRef;

pub struct CompilerError {
    pub msg: String,
    pub src: SourceRef,
}

impl CompilerError {
    pub fn new(msg: String, src: SourceRef) -> CompilerError { CompilerError { msg, src } }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Error: {}\n{}", &self.msg, &self.src))
    }
}