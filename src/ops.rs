// use crate::{Chunk, Compiler, SourceRef};
// use crate::chunk::Write;
// use crate::compiler::Upvalue;

use std::fmt::{Display, Formatter};
use crate::chunk::Write;
use crate::{Chunk, Compiler, SourceRef};

#[derive(Clone, Debug)]
pub enum Op {
    Ret,
    Const(u8),
    Negate,
    Add,
    Sub,
    Mult,
    Div,
    True,
    False,
    Nil,
    Not,
    EqEq,
    NotEq,
    LessThan,
    LessThanEq,
    Print,
    Pop,
    GetLocal(u8),
    SetLocal(u8),
    GetUpvalue(u8),
    SetUpvalue(u8),
    RelJumpIfFalse(i16),
    RelJump(i16),
    Call(u8),
    SmallConst(u8),
    Closure(u8, u8), // function idx, ret values idx
    Stack,
    RelJumpIfTrue(i16),
}

impl Op {
    pub(crate) fn write(self, chunk: &mut Chunk, src: SourceRef) -> Write {
        chunk.add(self, src)
    }
    pub(crate) fn emit(&self, compiler: &mut Compiler) -> Write {
        let prev = compiler.prev_source();
        compiler.chunk().add(self.clone(), prev)
    }
    pub(crate) fn overwrite(&self, chunk: &mut Chunk, write: &Write) {
        let offset: i64 = (chunk.len() - write.start) as i64;
        if offset > (i16::MAX as i64) || offset < (i16::MIN as i64) {
            panic!("Jump too big")
        }
        match self {
            Op::RelJumpIfFalse(_) => {
                chunk.overwrite(write, Op::RelJumpIfFalse(offset as i16))
            }
            Op::RelJump(_) => {
                chunk.overwrite(write, Op::RelJump(offset as i16))
            }
            Op::RelJumpIfTrue(_) => {
                chunk.overwrite(write, Op::RelJumpIfTrue(offset as i16))
            }
            _ => panic!("Bad overwrite"),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Op::Ret => "Ret".to_string(),
            Op::Const(_) => "Const".to_string(),
            Op::Negate => "Negate".to_string(),
            Op::Add => "Add".to_string(),
            Op::Sub => "Sub".to_string(),
            Op::Mult => "Mult".to_string(),
            Op::Div => "Div".to_string(),
            Op::True => "True".to_string(),
            Op::False => "False".to_string(),
            Op::Nil => "Nil".to_string(),
            Op::Not => "Not".to_string(),
            Op::EqEq => "EqEq".to_string(),
            Op::NotEq => "NotEq".to_string(),
            Op::LessThan => "LessThan".to_string(),
            Op::LessThanEq => "LessThanEq".to_string(),
            Op::Print => "Print".to_string(),
            Op::Pop => "Pop".to_string(),
            Op::GetLocal(idx) => format!("GetLocal idx-{}", idx),
            Op::SetLocal(idx) => format!("SetLocal idx-{}", idx),
            Op::GetUpvalue(idx) => format!("GetUpvalue idx-{}", idx),
            Op::SetUpvalue(idx) => format!("SetUpvalue idx-{}", idx),
            Op::RelJumpIfFalse(offset) => format!("RelJumpIfFalse offset-{}", offset),
            Op::RelJump(offset) => format!("RelJump offset-{}", offset),
            Op::Call(arity) => format!("Call arity-{}", arity),
            Op::SmallConst(val) => format!("SmallConst val-{}", val),
            Op::Closure(func_idx, retval_idx) => format!("Closure funcidx-{} retval-{}", func_idx, retval_idx),
            Op::Stack => "Stack".to_string(),
            Op::RelJumpIfTrue(offset) => format!("RelJumpIfTrue offset-{}", offset),
        };
        f.write_str(&s)
    }
}
//     pub upvalues: Vec<crate::compiler::Upvalue>,


// pub trait OpJumpTrait {
//     fn overwrite(&self, chunk: &mut Chunk, write: &Write);
// }
//
// pub trait OpTrait {
//     const CODE: u8;
//     const SIZE: usize;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write;
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self);
//     fn emit(&self, compiler: &mut Compiler) -> Write {
//         let prev = compiler.prev_source();
//         self.write(&mut compiler.chunk(), prev)
//     }
// }
//
// pub trait OpU8 {
//     fn emit_u8(compiler: &mut Compiler, idx: u8);
// }
//
// #[derive(Debug)]
// pub struct Ret {}
//
// impl OpTrait for Ret {
//     const CODE: u8 = 0;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Ret {}) }
// }
//
// #[derive(Debug)]
// pub struct Const {
//     pub idx: u8,
// }
//
// impl OpTrait for Const {
//     const CODE: u8 = 1;
//     const SIZE: usize = 2;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.idx], src)
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         (1, Const { idx: *code.get(idx).expect("Bad bytecode") })
//     }
// }
//
// #[derive(Debug)]
// pub struct Negate {}
//
// impl OpTrait for Negate {
//     const CODE: u8 = 2;
//     const SIZE: usize = 1;
//
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Negate {}) }
// }
//
// #[derive(Debug)]
// pub struct Add {}
//
// impl OpTrait for Add {
//     const CODE: u8 = 3;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Add {}) }
// }
//
// #[derive(Debug)]
// pub struct Sub {}
//
// impl OpTrait for Sub {
//     const CODE: u8 = 4;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Sub {}) }
// }
//
// #[derive(Debug)]
// pub struct Mult {}
//
// impl OpTrait for Mult {
//     const CODE: u8 = 5;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Mult {}) }
// }
//
// #[derive(Debug)]
// pub struct Div {}
//
// impl OpTrait for Div {
//     const CODE: u8 = 6;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Div {}) }
// }
//
// #[derive(Debug)]
// pub struct True {}
//
// impl OpTrait for True {
//     const CODE: u8 = 7;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, True {}) }
// }
//
// #[derive(Debug)]
// pub struct False {}
//
// impl OpTrait for False {
//     const CODE: u8 = 8;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, False {}) }
// }
//
// #[derive(Debug)]
// pub struct Nil {}
//
// impl OpTrait for Nil {
//     const CODE: u8 = 9;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Nil {}) }
// }
//
// #[derive(Debug)]
// pub struct Not {}
//
// impl OpTrait for Not {
//     const CODE: u8 = 10;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Not {}) }
// }
//
// #[derive(Debug)]
// pub struct EqualEqual {}
//
// impl OpTrait for EqualEqual {
//     const CODE: u8 = 11;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, EqualEqual {}) }
// }
//
// #[derive(Debug)]
// pub struct NotEqual {}
//
// impl OpTrait for NotEqual {
//     const CODE: u8 = 12;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, NotEqual {}) }
// }
//
// #[derive(Debug)]
// pub struct Less {}
//
// impl OpTrait for Less {
//     const CODE: u8 = 14;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Less {}) }
// }
//
//
// #[derive(Debug)]
// pub struct LessOrEq {}
//
// impl OpTrait for LessOrEq {
//     const CODE: u8 = 16;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, LessOrEq {}) }
// }
//
// #[derive(Debug)]
// pub struct Print {}
//
// impl OpTrait for Print {
//     const CODE: u8 = 17;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Print {}) }
// }
//
// #[derive(Debug)]
// pub struct Pop {}
//
// impl OpTrait for Pop {
//     const CODE: u8 = 18;
//     const SIZE: usize = 1;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_byte(Self::CODE, src)
//     }
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Pop {}) }
// }
//
// impl OpU8 for SetLocal {
//     fn emit_u8(compiler: &mut Compiler, idx: u8) {
//         SetLocal { idx }.emit(compiler);
//     }
// }
//
// impl OpU8 for GetLocal {
//     fn emit_u8(compiler: &mut Compiler, idx: u8) {
//         GetLocal { idx }.emit(compiler);
//     }
// }
//
// pub struct GetLocal {
//     pub idx: u8,
// }
//
// impl OpTrait for GetLocal {
//     const CODE: u8 = 22;
//     const SIZE: usize = 2;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.idx], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) { (1, GetLocal { idx: code[idx] }) }
// }
//
//
// pub struct SetLocal {
//     pub idx: u8,
// }
//
// impl OpTrait for SetLocal {
//     const CODE: u8 = 23;
//     const SIZE: usize = 2;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.idx], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) { (1, SetLocal { idx: code[idx] }) }
// }
//
//
// pub struct GetUpvalue {
//     pub idx: u8,
// }
//
// impl OpTrait for GetUpvalue {
//     const CODE: u8 = 19;
//     const SIZE: usize = 3;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.idx], src)
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) { (1, GetUpvalue { idx: code[idx] }) }
// }
//
// impl OpU8 for GetUpvalue {
//     fn emit_u8(compiler: &mut Compiler, idx: u8) {
//         GetUpvalue { idx }.emit(compiler);
//     }
// }
//
// pub struct SetUpvalue {
//     pub idx: u8,
// }
//
// impl OpTrait for SetUpvalue {
//     const CODE: u8 = 20;
//     const SIZE: usize = 3;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.idx], src)
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) { (1, SetUpvalue { idx: code[idx] }) }
// }
//
// impl OpU8 for SetUpvalue {
//     fn emit_u8(compiler: &mut Compiler, idx: u8) {
//         SetUpvalue { idx }.emit(compiler);
//     }
// }
//
// pub struct RelJumpIfFalse {
//     pub idx: i16,
// }
//
// impl OpTrait for RelJumpIfFalse {
//     const CODE: u8 = 24;
//     const SIZE: usize = 3;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         let bytes = i16::to_be_bytes(self.idx);
//         code.add_bytes(&[Self::CODE, bytes[0], bytes[1]], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         let bytes = [code[idx], code[idx + 1]];
//         let idx = i16::from_be_bytes(bytes);
//         (2, RelJumpIfFalse { idx })
//     }
// }
//
// impl OpJumpTrait for RelJumpIfFalse {
//     fn overwrite(&self, chunk: &mut Chunk, write: &Write) {
//         let offset: i64 = (chunk.len() - write.start) as i64;
//         if offset > (i16::MAX as i64) || offset < (i16::MIN as i64) {
//             panic!("Jump too big")
//         }
//         let bytes = i16::to_be_bytes(offset as i16);
//         chunk.overwrite(write, &[Self::CODE, bytes[0], bytes[1]]);
//     }
// }
//
// pub struct RelJump {
//     pub idx: i16,
// }
//
// impl OpTrait for RelJump {
//     const CODE: u8 = 25;
//     const SIZE: usize = 3;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         let bytes = i16::to_be_bytes(self.idx);
//         code.add_bytes(&[Self::CODE, bytes[0], bytes[1]], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         let bytes = [code[idx], code[idx + 1]];
//         let idx = i16::from_be_bytes(bytes);
//         (2, RelJump { idx })
//     }
// }
//
// impl OpJumpTrait for RelJump {
//     fn overwrite(&self, chunk: &mut Chunk, write: &Write) {
//         let offset: i64 = (chunk.len() - write.start) as i64;
//         if offset > (i16::MAX as i64) || offset < (i16::MIN as i64) {
//             panic!("Jump too big")
//         }
//         let bytes = i16::to_be_bytes(offset as i16);
//         chunk.overwrite(write, &[Self::CODE, bytes[0], bytes[1]]);
//     }
// }
//
// pub struct Call {
//     pub arity: u8,
// }
//
// impl OpTrait for Call {
//     const CODE: u8 = 26;
//     const SIZE: usize = 2;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.arity], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         (1, Call { arity: code[idx] })
//     }
// }
//
//
// pub struct SmallConst {
//     pub val: u8,
// }
//
// impl OpTrait for SmallConst {
//     const CODE: u8 = 27;
//     const SIZE: usize = 2;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE, self.val], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         (1, SmallConst { val: code[idx] })
//     }
// }
//
// pub struct Closure {
//     pub idx: u8,
//     pub upvalues: Vec<crate::compiler::Upvalue>,
// }
//
// impl OpTrait for Closure {
//     const CODE: u8 = 28;
//     const SIZE: usize = 2;
//
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         let w = code.add_bytes(&[Self::CODE, self.idx, self.upvalues.len() as u8], src.clone());
//         for up in &self.upvalues {
//             code.add_byte(up.idx, src.clone());
//             code.add_byte(if up.local { 1 } else { 0 }, src.clone());
//         }
//         w
//     }
//
//     fn decode(code: &Vec<u8>, start_idx: usize) -> (usize, Self) {
//         let idx = code[start_idx];
//         let count = code[(start_idx as usize) + 1];
//         let mut upvalues = vec![];
//         let base = (start_idx + 2) as usize;
//         for i in 0..(count as usize) {
//             upvalues.push(Upvalue { idx: code[base + i * 2], local: code[base + i * 2 + 1] == 1 })
//         }
//         (2 + (count as usize) * 2, Closure { idx, upvalues })
//     }
// }
//
// pub struct Stack {}
//
// impl OpTrait for Stack {
//     const CODE: u8 = 19;
//     const SIZE: usize = 1;
//
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         code.add_bytes(&[Self::CODE], src.clone())
//     }
//
//     fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) {
//         (0, Stack {})
//     }
// }
//
// pub struct RelJumpIfTrue {
//     pub idx: i16,
// }
//
// impl OpTrait for RelJumpIfTrue {
//     const CODE: u8 = 29;
//     const SIZE: usize = 3;
//     fn write(&self, code: &mut Chunk, src: SourceRef) -> Write {
//         let bytes = i16::to_be_bytes(self.idx);
//         code.add_bytes(&[Self::CODE, bytes[0], bytes[1]], src.clone())
//     }
//     fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
//         let bytes = [code[idx], code[idx + 1]];
//         let idx = i16::from_be_bytes(bytes);
//         (2, RelJumpIfTrue { idx })
//     }
// }
//
// impl OpJumpTrait for RelJumpIfTrue {
//     fn overwrite(&self, chunk: &mut Chunk, write: &Write) {
//         let offset: i64 = (chunk.len() - write.start) as i64;
//         if offset > (i16::MAX as i64) || offset < (i16::MIN as i64) {
//             panic!("Jump too big")
//         }
//         let bytes = i16::to_be_bytes(offset as i16);
//         chunk.overwrite(write, &[Self::CODE, bytes[0], bytes[1]]);
//     }
// }