use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use crate::ops::{EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Nil, Not, NotEqual, True, Print, Ret, Const, Negate, Add, Sub, Mult, Div, Pop, DefGlobal, GetGlobal, SetGlobal, GetLocal, SetLocal, RelJumpIfFalse, RelJump};
use crate::value::Value;
use crate::ops::OpTrait;
use crate::{Chunk, SourceRef, Symbol, Symbolizer};
use crate::vm::InterpErrorType::CompilerError;

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
    ip: *const u8,
    ip_idx: usize,
    stack: Vec<Value>,
    globals: HashMap<Symbol, Value>,
    symbolizer: Symbolizer,
}

enum Test {
    Str,
    Num,
}

impl<'a> VM<'a> {
    pub fn interpret(chunk: &'a Chunk, symbolizer: Symbolizer) -> Result<Value, InterpError> {
        let ptr: *const u8 =  (&chunk.code()[0]);
        let mut vm = VM {
            symbolizer,
            chunk,
            stack: vec![],
            code: chunk.code(),
            consts: chunk.consts(),
            ip: ptr,
            ip_idx: 0,
            globals: HashMap::new(),
        };
        vm.run()
    }
    fn read_byte(&mut self) -> Result<u8, InterpError> {
        unsafe {
            Ok(*self.ip)
        }
        // match self.code.get()
        //     Some(byte) => Ok(*byte),
        //     None => {
        //         match self.chunk.get_source(self.ip) {
        //             None => Err(InterpError::compile(None, format!("Tried to read a byte but were unable to!"))),
        //             Some(src) => Err(InterpError::compile(Some(src.clone()), format!("Tried to read a byte but were unable to!"))),
        //         }
        //     }
        // }
    }

    fn pop(&mut self) -> Result<Value, InterpError> {
        match self.stack.pop() {
            None => {
                let src: Option<SourceRef> = self.chunk.get_source(self.ip).and_then(|f| Some(f.clone()));
                Err(InterpError::compile(src, format!("Compiler error: unable to load value for this option")))
            }
            Some(val) => {
                // println!("<== Popping {:?}", &val);
                Ok(val)
            }
        }
    }

    fn peek_at(&self, idx: u8) -> Result<Value, InterpError> {
        // // println!("Peeking at {}, len: {}", dist, self.stack.len());
        match self.stack.get(idx as usize) {
            None => Err(InterpError::compile(None, format!("Tried to peek at index {} in the stack but the stack has length {}", idx, self.stack.len()))),
            Some(v) => Ok(v.clone())
        }
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn push(&mut self, v: Value) {
        // println!("==> Pushing {:?}", v);
        self.stack.push(v);
    }

    fn bump_ip(&mut self) {
        self.ip_idx += 1;
        self.ip = unsafe { self.ip.add(1) };
    }

    fn offset_ip(&mut self, offset: isize) {
        if offset > 0 {
            self.ip_idx += offset as usize;
        } else {
            self.ip_idx -= offset.abs() as usize;
        }

        self.ip = unsafe { self.ip.offset(offset) };
    }

    fn run(&mut self) -> Result<Value, InterpError> {
        loop {
            let inst = self.read_byte()?;
            // println!("Read byte {}", inst);
            // self.chunk.disassemble_op(&inst, self.ip_idx+1);
            // println!("\tstack: {:?}", self.stack);
            match inst {
                Ret::CODE => {
                    let (_len, _ret) = Ret::decode(self.code, self.ip_idx + 1);
                    // let popped = self.pop()?;
                    // return Ok(popped);
                    return Ok(Value::Num(1.0));
                }
                Const::CODE => {
                    let (len, con) = Const::decode(self.code, self.ip_idx + 1);
                    let value: Value = self.consts.get(con.idx as usize).expect("Compiler error").clone();
                    self.push(value);
                    self.offset_ip(1 + len as isize);
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
                    self.bump_ip();
                }
                Add::CODE => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
                        (Value::String(str1), Value::String(str2)) => {
                            let mut new_str = format!("{}{}", str1.to_str(), str2.to_str());
                            Value::String(self.symbolizer.get_symbol(new_str))
                        }
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot add a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    self.bump_ip();
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
                    self.bump_ip();
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
                    self.bump_ip();
                }
                Div::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    let res = match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => Value::Num(b / a),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot divide a {} and a {}", a.tname(), b.tname())))
                    };
                    self.push(res);
                    self.bump_ip();
                }
                True::CODE => {
                    self.push(Value::Bool(true));
                    self.bump_ip();
                }
                False::CODE => {
                    self.push(Value::Bool(false));
                    self.bump_ip();
                }
                Nil::CODE => {
                    self.push(Value::Nil);
                    self.bump_ip();
                }
                Not::CODE => {
                    let truthy = self.pop()?.truthy();
                    self.push(Value::Bool(!truthy));
                    self.bump_ip();
                }
                EqualEqual::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;

                    self.push(Value::Bool(
                        match (&a, &b) {
                            (Value::Num(a), Value::Num(b)) => (a == b),
                            (Value::String(a), Value::String(b)) => (a == b),
                            (Value::Bool(a), Value::Bool(b)) => (a == b),
                            (Value::Nil, Value::Nil) => (a == b),
                            _ => return Err(InterpError::runtime(
                                Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                format!("Cannot compare a {} and a {}", a.tname(), b.tname()),
                            ))
                        }));
                    self.bump_ip();
                }
                NotEqual::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    self.push(Value::Bool(a != b));
                    self.bump_ip();
                }
                Greater::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b > a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot compare {} and {} with the > op", a.tname(), b.tname())))
                    }
                    self.bump_ip();
                }
                GreaterOrEq::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b >= a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot compare {} and {} with the >= operation", a.tname(), b.tname())))
                    }
                    self.bump_ip();
                }
                Less::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b < a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot compare {} and {} with the < operation", a.tname(), b.tname())))
                    }
                    self.bump_ip();
                }
                LessOrEq::CODE => {
                    let a = self.pop()?;
                    let b = self.pop()?;
                    match (&a, &b) {
                        (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b <= a)),
                        _ => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                             format!("Cannot compare {} and {} with the <= operation", a.tname(), b.tname())))
                    }
                    self.bump_ip();
                }
                Print::CODE => {
                    let value = self.pop()?;
                    println!("{}", value);
                    self.bump_ip();
                }
                Pop::CODE => {
                    self.pop()?;
                    self.bump_ip();
                }
                DefGlobal::CODE => {
                    let (len, def_global) = DefGlobal::decode(self.code, self.ip_idx + 1);
                    let global_name = self.consts.get(def_global.idx as usize).unwrap();
                    let popped = self.pop()?;
                    // println!("Defining global '{}' as '{}'", global_name, popped);
                    if let Value::String(sym) = global_name {
                        self.globals.insert(sym.clone(), popped);
                    } else {
                        panic!("Compiler error, non-string constant passed to DefGlobal");
                    }
                    self.offset_ip(1 + len as isize);
                }
                GetGlobal::CODE => {
                    let (len, get_global) = GetGlobal::decode(self.code, self.ip_idx + 1);
                    let global_idx = get_global.idx as usize;
                    let global_name = self.consts.get(global_idx).expect("Compiler error, bad index in get global op");
                    if let Value::String(global_name) = global_name {
                        // println!("Getting global {}", global_name);
                        match self.globals.get(global_name) {
                            None => return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                                    format!("Global variable {} not found", global_name))),
                            Some(v) => {
                                // println!("\tGlobal is {}", v);
                                self.push(v.clone())
                            }
                        }
                    } else {
                        panic!("Compiler error, non-string constant passed to GetGlobal");
                    }
                    self.offset_ip(1 + len as isize);
                }
                SetGlobal::CODE => {
                    let (len, set_global) = SetGlobal::decode(self.code, self.ip_idx + 1);
                    let global_name = self.consts.get(set_global.idx as usize).expect("Compiler error, bad index in get global op");
                    let new_val = self.peek();
                    // println!("Setting global {} to {}", global_name, new_val);
                    if let Value::String(global_name) = global_name {
                        if self.globals.contains_key(global_name) {
                            self.globals.insert(global_name.clone(), new_val.clone());
                        } else {
                            return Err(InterpError::runtime(Some(self.chunk.get_source(self.ip).unwrap().clone()),
                                                            format!("Cannot assign to global variable {} since it has not been declared", global_name)));
                        }
                    } else {
                        panic!("Compiler error, non-string constant passed to GetGlobal");
                    }
                    self.offset_ip(1 + len as isize);
                }
                GetLocal::CODE => {
                    let (len, get_local) = GetLocal::decode(self.code, self.ip_idx + 1);
                    let peeked = self.peek_at(get_local.idx)?;
                    self.push(peeked);
                    self.offset_ip(1 + len as isize);
                }
                SetLocal::CODE => {
                    let (len, set_local) = SetLocal::decode(self.code, self.ip_idx + 1);
                    let stack_len = self.stack.len();
                    // println!("Setting local to {}", self.peek().clone());
                    self.stack[set_local.idx as usize] = self.peek().clone();
                    self.offset_ip(1 + len as isize);
                }
                RelJumpIfFalse::CODE => {
                    let (len, jump) = RelJumpIfFalse::decode(self.code, self.ip_idx + 1);
                    if !self.peek().truthy() {
                        self.offset_ip(jump.idx as isize);
                    } else {
                        self.offset_ip(1 + len as isize);
                    }
                }
                RelJump::CODE => {
                    let (_, jump) = RelJump::decode(self.code, self.ip_idx + 1);
                    self.offset_ip(jump.idx as isize);
                }
                _ => return Err(InterpError::compile(None, format!("Hit an unknown bytecode opcode {}, this is a compiler bug", inst)))
            };
        }
    }
}
