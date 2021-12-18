use std::fmt::{Display, Formatter};
use crate::{Add, Chunk, Const, Div, Mult, Negate, OpTrait, Ret, SourceRef, Sub};
use crate::ops::{EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Nil, Not, NotEqual, True};
use crate::source_ref::Source;
use crate::trie::Trie;
use crate::value::Value;

#[derive(Debug)]
pub enum InterpErrorType {
    CompilerError,
    RuntimeError,
}

impl Display for InterpErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            InterpErrorType::CompilerError => "compiler error",
            InterpErrorType::RuntimeError => "runtime error",
        })
    }
}

pub struct InterpError {
    typ: InterpErrorType,
    source: Option<SourceRef>,
    msg: String,
}

impl Display for InterpError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.source {
            None => f.write_str(&format!("A {} occurred: {}\n{}", self.typ, self.msg, "--unable to find source code--")),
            Some(src) => f.write_str(&format!("A {} occurred: {}\n{}", self.typ, self.msg, src.clone())),
        }
    }
}

impl InterpError {
    pub fn runtime(source: Option<SourceRef>, msg: String) -> InterpError {
        InterpError { typ: InterpErrorType::RuntimeError, source, msg }
    }
    pub fn compile(source: Option<SourceRef>, msg: String) -> InterpError {
        InterpError { typ: InterpErrorType::CompilerError, source, msg }
    }
}

pub struct VM<'a> {
    chunk: &'a Chunk,
    code: &'a Vec<u8>,
    consts: &'a Vec<Value>,
    ip: usize,
    stack: Vec<Value>,
}

enum Test {
    Str,
    Num
}

impl<'a> VM<'a> {
    pub fn interpret(chunk: &'a Chunk) -> Result<Value, InterpError> {
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
            None => {
                match self.chunk.get_source(self.ip) {
                    None => Err(InterpError::compile(None, format!("Tried to read a byte but were unable to!"))),
                    Some(src) => Err(InterpError::compile(Some(src.clone()), format!("Tried to read a byte but were unable to!"))),
                }
            }
        }
    }

    fn pop(&mut self) -> Result<Value, InterpError> {
        match self.stack.pop() {
            None => {
                let src: Option<SourceRef> = self.chunk.get_source(self.ip).and_then(|f| Some(f.clone()));
                Err(InterpError::compile(src, format!("Compiler error: unable to load value for this option")))
            }
            Some(val) => {
                println!("Popping {:?}", &val);
                Ok(val)
            },
        }
    }

    fn push(&mut self, v: Value) {
        println!("Pushing {:?}", v);
        self.stack.push(v);
    }

    fn run(&mut self) -> Result<Value, InterpError> {
        loop {
            let inst = self.read_byte()?;
            let len = match inst {
                Ret::CODE => {
                    let (len, _ret) = Ret::decode(self.code, self.ip + 1);
                    let popped = self.pop()?;
                    return Ok(popped);
                }
                Const::CODE => {
                    let (_len, con) = Const::decode(self.code, self.ip + 1);
                    let value: Value = self.consts.get(con.idx as usize).expect("Compiler error").clone();
                    self.push(value);
                    Const::SIZE-1
                }
                Negate::CODE => {
                    let popped = self.pop()?;
                    if let Value::Num(n) = popped {
                        self.push(Value::Num(-n));
                    } else {
                        return Err(InterpError::runtime(
                            Some(self.chunk.get_source(self.ip).expect("Compiler error").clone()),
                            format!("Tried to negate (-) a {}. You can only negate numbers.", popped.tname()),
                        ));
                    }
                    0
                }
                Add::CODE => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                        (Value::String(str1), Value::String(str2)) => {
                            Value::String(format!("{}{}", str1,str2))
                        },
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot add a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    0
                }
                Sub::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(b - a),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot subtract a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    0
                }
                Mult::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(b * a),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot multiply a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    0
                }
                Div::CODE => {
                    let (len, _op) = Div::decode(self.code, self.ip + 1);
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(b / a),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot divide a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    len
                }
                True::CODE => {
                    self.push(Value::Bool(true));
                    0
                }
                False::CODE => {
                    self.push(Value::Bool(false));
                    0
                }
                Nil::CODE => {
                    self.push(Value::Nil);
                    0
                }
                Not::CODE => {
                    let truthy = self.pop()?.truthy();
                    self.push(Value::Bool(!truthy));
                    0
                }
                EqualEqual::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(Value::Bool(a == b));
                    0
                }
                NotEqual::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(Value::Bool(a != b));
                    0
                }
                Greater::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b > a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                        format!("Cannot compare {} and {} with the > op", a.tname(), b.tname())))
                    }
                    0
                },
                GreaterOrEq::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b >= a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                        format!("Cannot compare {} and {} with the >= operation", a.tname(), b.tname())))
                    }
                    0
                },
                Less::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b < a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                        format!("Cannot compare {} and {} with the < operation", a.tname(), b.tname())))
                    }
                    0
                },
                LessOrEq::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b <= a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                        format!("Cannot compare {} and {} with the <= operation", a.tname(), b.tname())))
                    }
                    0
                },
                _ => return Err(InterpError::compile(None, format!("Hit an unknown bytecode opcode {}, this is a compiler bug", inst)))
            };
            self.ip += 1 + len;
        }
    }
}
