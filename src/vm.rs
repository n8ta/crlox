use crate::{Add, Chunk, Const, Div, Mult, Negate, OpTrait, Ret, Sub};
use crate::trie::Trie;
use crate::value::Value;

pub enum InterpError {
    CompilerError,
    RuntimeError,
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    code: &'a Vec<u8>,
    consts: &'a Vec<Value>,
    ip: usize,
    stack: Vec<Value>,
}

impl<'a> VM<'a> {
    pub fn interpret(chunk: &'a Chunk) -> Result<(), InterpError> {
        let mut vm = VM {
            chunk,
            stack: vec![],
            code: chunk.code(),
            consts: chunk.consts(),
            ip: 0,
        };
        vm.run()
    }
    fn read_byte(&mut self) -> Result<u8, InterpError> {
        match self.code.get(self.ip) {
            Some(byte) => Ok(*byte),
            None => Err(InterpError::CompilerError)
        }
    }
    fn run(&mut self) -> Result<(), InterpError> {
        loop {
            let inst = self.read_byte()?;
            let len = match inst {
                Ret::CODE => {
                    let (len, _ret) = Ret::decode(self.code, self.ip + 1);
                    let popped = self.stack.pop().expect("TODO: except");
                    len
                }
                Const::CODE => {
                    let (len, con) = Const::decode(self.code, self.ip + 1);
                    let value = *self.consts.get(con.idx as usize).expect("TODO: exception");
                    self.stack.push(value); //
                    len
                }
                Negate::CODE => {
                    let (len, _con) = Negate::decode(self.code, self.ip + 1);
                    let popped = self.stack.pop().expect("TODO: proper exception here");
                    self.stack.push(-popped);
                    len
                }
                Add::CODE => {
                    let (len, _op) = Add::decode(self.code, self.ip + 1);
                    let a = self.stack.pop().expect("TODO: except");
                    let b = self.stack.pop().expect("TODO: except");
                    self.stack.push(b + a);
                    len
                }
                Sub::CODE => {
                    let (len, _op) = Sub::decode(self.code, self.ip + 1);
                    let a = self.stack.pop().expect("TODO: except");
                    let b = self.stack.pop().expect("TODO: except");
                    self.stack.push(b - a);
                    len
                }
                Mult::CODE => {
                    let (len, _op) = Mult::decode(self.code, self.ip + 1);
                    let a = self.stack.pop().expect("TODO: except");
                    let b = self.stack.pop().expect("TODO: except");
                    self.stack.push(b * a);
                    len
                }
                Div::CODE => {
                    let (len, _op) = Div::decode(self.code, self.ip + 1);
                    let a = self.stack.pop().expect("TODO: except");
                    let b = self.stack.pop().expect("TODO: except");
                    self.stack.push(b / a);
                    len
                }
                _ => return Err(InterpError::CompilerError)
            };
            self.ip += 1 + len;
        }
    }
}
