use crate::ops::{OpTrait, Add, Const, Div, EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Mult, Negate, Nil, Not, NotEqual, Pop, Print, Ret, Sub, True, SetGlobal, GetGlobal, DefGlobal};
use crate::SourceRef;
use crate::value::Value;

#[derive(Clone, Debug)]
pub struct Chunk {
    code: Vec<u8>,
    sources: Vec<SourceRef>,
    // Same len as code
    constants: Vec<Value>,
}

impl Chunk {
    pub fn code(&self) -> &Vec<u8> {
        &self.code
    }
    pub fn consts(&self) -> &Vec<Value> {
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
                idx += self.disassemble_op(byte, idx + 1)
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
                println!("Const[{}]{}", const_idx, const_val);
                Const::SIZE
            }
            Ret::CODE => {
                println!("Return");
                Ret::SIZE
            }
            Negate::CODE => {
                println!("Negate");
                Negate::SIZE
            }
            Add::CODE => {
                println!("Add");
                Add::SIZE
            }
            Sub::CODE => {
                println!("Sub");
                Sub::SIZE
            }
            Mult::CODE => {
                println!("Mult");
                Mult::SIZE
            }
            Div::CODE => {
                println!("Div");
                Div::SIZE
            }
            True::CODE => {
                println!("True");
                True::SIZE
            }
            False::CODE => {
                println!("False");
                False::SIZE
            }
            Nil::CODE => {
                println!("Nil");
                Nil::SIZE
            }
            Not::CODE => {
                println!("Not");
                Not::SIZE
            }
            EqualEqual::CODE => {
                println!("EqualEqual");
                EqualEqual::SIZE
            }
            NotEqual::CODE => {
                println!("NotEqual");
                NotEqual::SIZE
            }
            Greater::CODE => {
                println!("Greater");
                Greater::SIZE
            }
            Less::CODE => {
                println!("Less");
                Less::SIZE
            }
            GreaterOrEq::CODE => {
                println!("GreaterOrEq");
                GreaterOrEq::SIZE
            }
            LessOrEq::CODE => {
                println!("LessOrEq");
                LessOrEq::SIZE
            }
            Print::CODE => {
                println!("Print");
                Print::SIZE
            }
            Pop::CODE => {
                println!("Pop");
                Pop::SIZE
            }
            DefGlobal::CODE => {
                let (len, op) = DefGlobal::decode(&self.code, idx);
                println!("DefGlobal[{}]=>{}", op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            GetGlobal::CODE => {
                let (len, op) = GetGlobal::decode(&self.code, idx);
                println!("GetGlobal[{}]=>{}", op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            SetGlobal::CODE => {
                let (len, op) = SetGlobal::decode(&self.code, idx);
                println!("SetGlobal[{}]=>{}", op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            _ => panic!("Bad op code {}", byte)
        }
    }
}