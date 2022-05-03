#[cfg(debug_assertions)]
use crate::debug_println;

use crate::{SourceRef};
use crate::ops::Op;
use crate::value::Value;

pub struct Write {
    pub start: usize,
}

impl Write {
    pub fn new(start: usize) -> Self { Write { start } }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<Op>,
    pub sources: Vec<SourceRef>,
    // Same len as code
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn len(&self) -> usize {
        self.code.len()
    }
    pub fn add_const(&mut self, constant: Value) -> u8 {
        match self.constants.iter().enumerate().find(|(idx,c)| **c == constant) {
            None => {
                self.constants.push(constant);
                (self.constants.len() - 1) as u8
            }
            Some((idx,_)) => {
                idx as u8
            }
        }
    }
    pub fn add(&mut self, op: Op, src: SourceRef) -> Write {
        let idx = self.code.len();
        self.code.push(op);
        self.sources.push(src);
        Write::new(idx)
    }
    pub fn overwrite(&mut self, write: &Write, op: Op) {
        self.code[write.start] = op
    }
    pub fn new() -> Chunk { Chunk { code: vec![], sources: vec![], constants: vec![] } }

    pub fn get_source(&self, code_idx: usize) -> Option<&SourceRef> {
        self.sources.get(code_idx)
    }
}