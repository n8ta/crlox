use std::fmt::{Display, Formatter};
use crate::chunk::{Chunk, Write};
use crate::compiler_ast::Compiler;
use crate::SourceRef;

#[derive(Clone, Debug, PartialEq)]
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
    Closure(u8),
    Stack,
    RelJumpIfTrue(i16),
    Class(u8),
    SetProperty(u8),
    GetProperty(u8),
    Method(u8)
}

impl Op {
    pub(crate) fn write(self, chunk: &mut Chunk, src: SourceRef) -> Write {
        chunk.add(self, src)
    }
    pub(crate) fn emit(&self, compiler: &mut Compiler) -> Write {
        let prev = SourceRef::simple();
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
            Op::RelJumpIfFalse(offset) => format!("RelJumpIfFalse offset:{}", offset),
            Op::RelJump(offset) => format!("RelJump offset:{}", offset),
            Op::Call(arity) => format!("Call arity-{}", arity),
            Op::SmallConst(val) => format!("SmallConst val-{}", val),
            Op::Closure(func_idx) => format!("Closure funcidx {}", func_idx),
            Op::Stack => "Stack".to_string(),
            Op::RelJumpIfTrue(offset) => format!("RelJumpIfTrue offset:{}", offset),
            Op::Class(idx) => format!("Class idx-{}", idx),
            Op::SetProperty(idx) => format!("SetProp idx-{}", idx),
            Op::GetProperty(idx) => format!("GetPrp idx-{}", idx),
            Op::Method(idx) => format!("Method idx-{}", idx),
        };
        f.write_str(&s)
    }
}

pub fn print_ops(ops: &Vec<Op>) -> String {
    let mut res = format!("");
    for (ip, op) in ops.iter().enumerate() {
        let s = match op {
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
            Op::RelJumpIfFalse(offset) => format!("RelJumpIfFalse to:{}", (ip as i64) + (*offset as i64)),
            Op::RelJump(offset) => format!("RelJump to:{}", (ip as i64) + (*offset as i64)),
            Op::Call(arity) => format!("Call arity-{}", arity),
            Op::SmallConst(val) => format!("SmallConst val-{}", val),
            Op::Closure(func_idx) => format!("Closure funcidx {}", func_idx),
            Op::Stack => "Stack".to_string(),
            Op::RelJumpIfTrue(offset) => format!("RelJumpIfTrue to:{}", (ip as i64) + (*offset as i64)),
            Op::Class(idx) => format!("Class idx-{}", idx),
            Op::SetProperty(idx) => format!("SetProp idx-{}", idx),
            Op::GetProperty(idx) => format!("GetPrp idx-{}", idx),
            Op::Method(idx) => format!("Method idx-{}", idx),
        };
        res.push_str(&format!("{} ", ip));
        res.push_str(&s);
        res.push_str("\n");
    }
    res
}