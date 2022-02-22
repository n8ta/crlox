use crate::ops::{OpTrait, Closure, Add, Const, Div, EqualEqual, False, Less, LessOrEq, Mult, Negate, Nil, Not, NotEqual, Pop, Print, Ret, Sub, True, GetLocal, SetLocal, RelJump, RelJumpIfFalse, Call, SmallConst, Stack, RelJumpIfTrue};
use crate::{debug_println, SourceRef};
use crate::value::Value;

pub struct Write {
    pub start: usize,
    pub len: usize,
}

impl Write {
    pub fn new(start: usize, len: usize) -> Self { Write { start, len } }
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub sources: Vec<SourceRef>,
    // Same len as code
    pub constants: Vec<Value>,
}

impl Chunk {
    pub fn len(&self) -> usize {
        self.code.len()
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
                let _const_val = self.constants[const_idx].clone();
                debug_println!("{} [{}] Const[{}]{}", idx - 1, Const::CODE, const_idx, _const_val);
                Const::SIZE
            }
            Ret::CODE => {
                debug_println!("{} [{}] Return", idx - 1, Ret::CODE);
                Ret::SIZE
            }
            Negate::CODE => {
                debug_println!("{} [{}] Negate", idx - 1, Negate::CODE);
                Negate::SIZE
            }
            Add::CODE => {
                debug_println!("{} [{}] Add", idx - 1, Add::CODE);
                Add::SIZE
            }
            Sub::CODE => {
                debug_println!("{} [{}] Sub", idx - 1, Sub::CODE);
                Sub::SIZE
            }
            Mult::CODE => {
                debug_println!("{} [{}] Mult", idx - 1, Mult::CODE);
                Mult::SIZE
            }
            Div::CODE => {
                debug_println!("{} [{}] Div", idx - 1, Div::CODE);
                Div::SIZE
            }
            True::CODE => {
                debug_println!("{} [{}] True", idx - 1, True::CODE);
                True::SIZE
            }
            False::CODE => {
                debug_println!("{} [{}] False", idx - 1, False::CODE);
                False::SIZE
            }
            Nil::CODE => {
                debug_println!("{} [{}] Nil", idx - 1, Nil::CODE);
                Nil::SIZE
            }
            Not::CODE => {
                debug_println!("{} [{}] Not", idx - 1, Not::CODE);
                Not::SIZE
            }
            EqualEqual::CODE => {
                debug_println!("{} [{}] EqualEqual", idx - 1, EqualEqual::CODE);
                EqualEqual::SIZE
            }
            NotEqual::CODE => {
                debug_println!("{} [{}] NotEqual", idx - 1, NotEqual::CODE);
                NotEqual::SIZE
            }
            Less::CODE => {
                debug_println!("{} [{}] Less", idx - 1, Less::CODE);
                Less::SIZE
            }
            LessOrEq::CODE => {
                debug_println!("{} [{}] LessOrEq", idx - 1, LessOrEq::CODE);
                LessOrEq::SIZE
            }
            Print::CODE => {
                debug_println!("{} [{}] Print", idx - 1, Print::CODE);
                Print::SIZE
            }
            Pop::CODE => {
                debug_println!("{} [{}] Pop", idx - 1, Pop::CODE);
                Pop::SIZE
            }
            // DefGlobal::CODE => {
            //     let (len, op) = DefGlobal::decode(&self.code, idx);
            //     debug_println!("{} [{}] DefGlobal[{}]=>{}", idx - 1, DefGlobal::CODE, op.idx, self.constants[op.idx as usize]);
            //     len + 1
            // }
            // GetGlobal::CODE => {
            //     let (len, op) = GetGlobal::decode(&self.code, idx);
            //     debug_println!("{} [{}] GetGlobal[{}]=>{}", idx - 1, GetGlobal::CODE, op.idx, self.constants[op.idx as usize]);
            //     len + 1
            // }
            // SetGlobal::CODE => {
            //     let (len, op) = SetGlobal::decode(&self.code, idx);
            //     debug_println!("{} [{}] SetGlobal[{}]=>{}", idx - 1, SetGlobal::CODE, op.idx, self.constants[op.idx as usize]);
            //     len + 1
            // }
            GetLocal::CODE => {
                let (len, _op) = GetLocal::decode(&self.code, idx);
                debug_println!("{} [{}] GetLocal[{}]", idx - 1, GetLocal::CODE, _op.idx);
                len + 1
            }
            SetLocal::CODE => {
                let (len, _op) = SetLocal::decode(&self.code, idx);
                debug_println!("{} [{}] SetLocal[{}]", idx - 1, SetLocal::CODE, _op.idx);
                len + 1
            }
            RelJump::CODE => {
                let (len, _op) = RelJump::decode(&self.code, idx);
                debug_println!("{} [{}] RelJump[{}]", idx - 1, RelJump::CODE, _op.idx);
                len + 1
            }
            RelJumpIfFalse::CODE => {
                let (len, _op) = RelJumpIfFalse::decode(&self.code, idx);
                debug_println!("{} [{}] RelJumpIfFalse[{}]", idx - 1, RelJumpIfFalse::CODE, _op.idx);
                len + 1
            }
            RelJumpIfTrue::CODE => {
                let (len, _op) = RelJumpIfTrue::decode(&self.code, idx);
                debug_println!("{} [{}] RelJumpIfTrue[{}]", idx - 1, RelJumpIfFalse::CODE, _op.idx);
                len + 1
            }
            Call::CODE => {
                let (len, _op) = Call::decode(&self.code, idx);
                debug_println!("{} [{}] Call[arity-{}]", idx - 1, Call::CODE, _op.arity);
                len + 1
            }
            SmallConst::CODE => {
                let (len, _op) = SmallConst::decode(&self.code, idx);
                debug_println!("{} [{}] SmallConst[{}]", idx - 1, SmallConst::CODE, _op.val);
                len  + 1
            }
            Closure::CODE => {
                let (len, op) = Closure::decode(&self.code, idx);
                if let Value::Func(func) = &self.constants[op.idx as usize] {
                    debug_println!("{} [{}] Closure[{}]", idx - 1, Closure::CODE, op.idx);
                    debug_println!("\tupvalues: {}", func.upvalues);
                    len  + 1 + (func.upvalues * 2)
                } else {
                    panic!("Closure not encoded properly");
                }
            }
            Stack::CODE => {
                let (_len, _op) = Stack::decode(&self.code, idx);
                debug_println!("{} [{}] Print Stack", idx - 1, SmallConst::CODE);
                1
            }
            _ => panic!("Bad op code {}", byte)
        }
    }
}