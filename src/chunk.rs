use crate::ops::{OpTrait, Add, Const, Div, EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Mult, Negate, Nil, Not, NotEqual, Pop, Print, Ret, Sub, True, SetGlobal, GetGlobal, DefGlobal, GetLocal, SetLocal};
use crate::SourceRef;
use crate::value::Value;

pub struct Write {
    pub start: usize,
    pub len: usize,
}

impl Write {
    pub fn new(start: usize, len: usize) -> Self { write { start, len }}
}

impl Chunk {
    pub fn len(&self) -> usize {
        self.code.len()
    }
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
    pub fn add_bytes(&mut self, bytes: &[u8], src: SourceRef) -> Write {
        let idx = self.code.len();
        for byte in bytes.iter() {
            self.code.push(*byte);
            self.sources.push(src.clone());
        }
        Write::new(idx, bytes.len())
    }
    pub fn add_byte(&mut self, byte: u8, src: SourceRef) -> Write {
        let idx = self.code.len();
        self.code.push(byte);
        self.sources.push(src);
        Write::new(idx, 1)
    }
    pub fn overwrite(&mut self, write: &Write, bytes: &[u8]) {
        if bytes.len() != write.len {
            panic!("Mismatched write and bytes!")
        }
        for offset in 0..write.len {
            self.code[offset + write.start] = bytes[offset]
        }
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
    pub fn disassemble_op(&self, byte: &u8, idx: usize) -> usize {
        match *byte {
            Const::CODE => {
                let const_idx = self.code[idx] as usize;
                let const_val = self.constants[const_idx].clone();
                println!("{} Const[{}]{}", idx-1, const_idx, const_val);
                Const::SIZE
            }
            Ret::CODE => {
                println!("{} Return", idx-1);
                Ret::SIZE
            }
            Negate::CODE => {
                println!("{} Negate", idx-1);
                Negate::SIZE
            }
            Add::CODE => {
                println!("{} Add", idx-1);
                Add::SIZE
            }
            Sub::CODE => {
                println!("{} Sub", idx-1);
                Sub::SIZE
            }
            Mult::CODE => {
                println!("{} Mult", idx-1);
                Mult::SIZE
            }
            Div::CODE => {
                println!("{} Div", idx-1);
                Div::SIZE
            }
            True::CODE => {
                println!("{} True", idx-1);
                True::SIZE
            }
            False::CODE => {
                println!("{} False", idx-1);
                False::SIZE
            }
            Nil::CODE => {
                println!("{} Nil", idx-1);
                Nil::SIZE
            }
            Not::CODE => {
                println!("{} Not", idx-1);
                Not::SIZE
            }
            EqualEqual::CODE => {
                println!("{} EqualEqual", idx-1);
                EqualEqual::SIZE
            }
            NotEqual::CODE => {
                println!("{} NotEqual", idx-1);
                NotEqual::SIZE
            }
            Greater::CODE => {
                println!("{} Greater", idx-1);
                Greater::SIZE
            }
            Less::CODE => {
                println!("{} Less", idx-1);
                Less::SIZE
            }
            GreaterOrEq::CODE => {
                println!("{} GreaterOrEq", idx-1);
                GreaterOrEq::SIZE
            }
            LessOrEq::CODE => {
                println!("{} LessOrEq", idx-1);
                LessOrEq::SIZE
            }
            Print::CODE => {
                println!("{} Print", idx-1);
                Print::SIZE
            }
            Pop::CODE => {
                println!("{} Pop", idx-1);
                Pop::SIZE
            }
            DefGlobal::CODE => {
                let (len, op) = DefGlobal::decode(&self.code, idx);
                println!("{} DefGlobal[{}]=>{}", idx-1, op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            GetGlobal::CODE => {
                let (len, op) = GetGlobal::decode(&self.code, idx);
                println!("{} GetGlobal[{}]=>{}", idx-1, op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            SetGlobal::CODE => {
                let (len, op) = SetGlobal::decode(&self.code, idx);
                println!("{} SetGlobal[{}]=>{}", idx-1, op.idx, self.constants[op.idx as usize]);
                len + 1
            }
            GetLocal::CODE => {
                let (len, op) = GetLocal::decode(&self.code, idx);
                println!("{} GetLocal[{}]", idx-1, op.idx);
                len + 1
            }
            SetLocal::CODE => {
                let (len, op) = SetLocal::decode(&self.code, idx);
                println!("{} SetLocal[{}]", idx-1, op.idx);
                len + 1
            }
            _ => panic!("Bad op code {}", byte)
        }
    }
}