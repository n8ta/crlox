use crate::{Const, Negate, OpTrait, Ret, SourceRef};
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Chunk {
    code: Vec<u8>,
    sources: Vec<SourceRef>, // Same len as code
    constants: Vec<Value>,
}

impl Chunk {
    pub(crate) fn code(&self) -> &Vec<u8> {
        &self.code
    }
    pub(crate) fn consts(&self) -> &Vec<Value> {
        &self.constants
    }

    pub fn add_const(&mut self, constant: Value) -> Const {
        self.constants.push(constant);
        Const { idx: (self.constants.len() - 1) as u8 }
    }
    pub fn add_byte(&mut self, byte: u8, src: SourceRef) {
        self.code.push(byte);
        self.sources.push(src);
    }
    pub fn new() -> Chunk { Chunk { code: vec![], sources: vec![], constants: vec![] } }
    pub fn disassemble(&self) {
        let mut idx = 0;
        loop {
            if let Some(byte) = self.code.get(idx) {
                idx +=  self.disassemble_op(byte, idx + 1)
            } else {
                break;
            }
        }
    }
    pub fn get_source(&self, code_idx: usize) -> Option<&SourceRef> {
        self.sources.get(code_idx)
    }
    fn disassemble_op(&self, byte: &u8, idx: usize) -> usize {
        match *byte {
            Const::CODE => {
                let const_idx = self.code[idx] as usize;
                let const_val = self.constants[const_idx].clone();
                println!("const[{}]{}", const_idx, const_val);
                Const::SIZE
            }
            Ret::CODE => {
                println!("return");
                Ret::SIZE
            }
            Negate::CODE => {
                println!("negate");
                Negate::SIZE
            }
            _ => panic!("Bad op code")
        }
    }
}