use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::swap;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};
use crate::ops::{Op};
use crate::value::Value;
use crate::{debug_println, SourceRef, Symbol, Symbolizer};
use crate::class::Class;
use crate::closure::WrappedValue;
use crate::compiler::Upvalue;
use crate::func::Func;
use crate::native_func::NativeFunc;

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

// static MAX_LOCALS: usize = 255;
// static MAX_STACK_SIZE: usize = 255;

pub struct VM {
    stack: Vec<Value>,

    globals: HashMap<Symbol, Value>,
    symbolizer: Symbolizer,
    frames: Vec<CallFrame>,
    frame: CallFrame,
}

struct CallFrame {
    pub closure: crate::closure::RtClosure,
    pub ip: usize,
    pub frame_offset: usize,
}

impl VM {
    pub fn interpret(main_func: Func, symbolizer: Symbolizer) -> Result<Value, InterpError> {
        let mut vm = VM {
            symbolizer,
            stack: vec![],
            globals: HashMap::new(),
            frames: vec![],
            frame: CallFrame {
                closure: crate::closure::RtClosure::new(main_func.clone()),
                ip: 0,
                frame_offset: 0,
            },
        };

        let clock = NativeFunc::new("clock", 0, |_vec|
            match SystemTime::now().duration_since(UNIX_EPOCH) {
                Ok(t) => Value::Num(t.as_secs_f64() * 1000.0),
                Err(_) => Value::Nil
            });
        vm.globals.insert(vm.symbolizer.get_symbol(format!("clock")), Value::Native(clock));
        vm.run()
    }

    #[cfg(debug_assertions)]
    fn print_stack_frame(&self, msg: &str) {
        debug_println!("{} {:?}", msg, &self.stack);
        debug_println!("\t{:?}", &self.stack[self.frame.frame_offset..self.stack.len()]);
    }
    fn debug_stack(&self, msg: &str) {
        println!("{} {:?}", msg, &self.stack);
        println!("\t{:?}", &self.stack[self.frame.frame_offset..self.stack.len()]);
    }

    fn pop(&mut self) -> Result<Value, InterpError> {
        match self.stack.pop() {
            None => {
                let ip = self.frame.ip;
                let src: Option<SourceRef> = self.frame.closure.chunk.get_source(ip).and_then(|f| Some(f.clone()));
                Err(InterpError::compile(src, "Tried to pop a value from the stack but it was empty!, src".to_string()))
            }
            Some(val) => {
                debug_println!("<== Popping {:?}", &val);
                Ok(val)
            }
        }
    }

    fn peek_at(&self, idx: u8) -> Result<Value, InterpError> {

        #[cfg(debug_assertions)]
            self.print_stack_frame(&format!("peek at stack idx-{} frame-{}: ", idx, self.frame.frame_offset));

        match self.stack.get(self.frame.frame_offset + idx as usize) {
            None => panic!("bad stack access idx-{} offset-{}", idx, self.frame.frame_offset),
            Some(v) => {
                debug_println!("Got {}", v.clone());
                Ok(v.clone())
            }
        }
    }

    fn peek(&self) -> &Value {
        self.stack.last().unwrap()
    }

    fn push(&mut self, v: Value) {
        debug_println!("==> Pushing {:?}", v);
        self.stack.push(v);
    }

    fn bump_ip(&mut self) {
        self.frame.ip += 1;
    }

    fn offset_ip(&mut self, offset: isize) {
        if offset > 0 {
            self.frame.ip += offset as usize;
        } else {
            self.frame.ip -= offset.abs() as usize;
        }
    }

    fn offset_ip_pos(&mut self, offset: usize) {
        self.frame.ip += offset as usize;
    }


    fn run(&mut self) -> Result<Value, InterpError> {
        loop {
            let ip = self.frame.ip;

            debug_println!("====================\n{} {}", ip, self.frame.closure.chunk.code.get(ip).unwrap());
            match self.frame.closure.chunk.code.get(ip).unwrap() {
                Op::Ret => {
                    #[cfg(debug_assertions)]
                    self.print_stack_frame("Pre ret stack");
                    if let Some(frame) = self.frames.pop() {
                        let ret = self.pop()?;

                        while self.stack.len() > self.frame.frame_offset {
                            self.pop()?;
                        }

                        self.frame = frame;
                        debug_println!("Function returned {}", ret);
                        // self.pop()?;
                        self.push(ret);

                        #[cfg(debug_assertions)]
                        self.print_stack_frame("Post ret stack");

                    } else {
                        #[cfg(debug_assertions)]
                            self.print_stack_frame("Final return stack");
                        return Ok(Value::Nil);
                    }
                }
                Op::Const(idx) => {
                    self.load_const(*idx)?;
                }
                Op::Negate => {
                    self.negate()?;
                }
                Op::Add => {
                    self.add()?;
                }
                Op::Sub => {
                    self.sub()?;
                }
                Op::Mult => {
                    self.mult()?;
                }
                Op::Div => {
                    self.div()?;
                }
                Op::True => {
                    self.push(Value::Bool(true));
                    self.bump_ip();
                }
                Op::False => {
                    self.push(Value::Bool(false));
                    self.bump_ip();
                }
                Op::Nil => {
                    self.push(Value::Nil);
                    self.bump_ip();
                }
                Op::Not => {
                    let truthy = self.pop()?.truthy();
                    self.push(Value::Bool(!truthy));
                    self.bump_ip();
                }
                Op::EqEq => {
                    self.ee()?;
                }
                Op::NotEq => {
                    self.ne()?;
                }
                Op::LessThan => {
                    self.lt()?;
                }
                Op::LessThanEq => {
                    self.lt_eq()?;
                }
                Op::Print => {
                    let value = self.pop()?;
                    println!("{}", value);
                    self.bump_ip();
                }
                Op::Pop => {
                    self.pop()?;
                    self.bump_ip();
                }
                Op::GetLocal(idx) => {
                    self.get_local(*idx)?;
                }
                Op::SetLocal(idx) => {
                    self.set_local(*idx)?;
                }
                Op::RelJumpIfFalse(offset) => {
                    self.rel_jump_if_false(*offset)?;
                }
                Op::RelJumpIfTrue(offset) => {
                    self.rel_jump_if_true(*offset)?;
                }
                Op::RelJump(offset) => {
                    self.rel_jump(*offset)?;
                }
                Op::Call(arity) => {
                    self.call(*arity)?;
                }
                Op::SmallConst(val) => {
                    self.small_const(*val)?;
                }
                Op::Closure(a) => {
                    self.closure(*a)?;
                }
                Op::Stack => {
                    self.debug_stack("debug");
                    self.bump_ip();
                }
                Op::GetUpvalue(idx) => {
                    self.get_up_value(*idx)?;
                }
                Op::SetUpvalue(idx) => {
                    self.set_up_value(*idx)?;
                }
                Op::Class(idx) => {
                    self.class(*idx)?;
                }
            };
        }
    }

    fn class(&mut self, idx: u8) -> Result<(), InterpError> {
        if let Value::String(sym) = &self.frame.closure.chunk.constants[idx as usize] {
            self.push(Value::Class(Class::new(sym.clone())))
        }  else {
            panic!("Compiler error, class_idx should be a string in the constant array")
        }
        self.bump_ip();
        Ok(())
    }

    fn set_up_value(&mut self, idx: u8) -> Result<(), InterpError> {
        debug_println!("Setting up value to {:?}", self.peek().clone());
        let new = self.peek().clone();
        {
            let mut inner = self.frame.closure.upvalues[idx as usize].borrow_mut();
            inner.inner_value = new;
        }
        self.bump_ip();
        Ok(())
    }

    fn get_up_value(&mut self, idx: u8) -> Result<(), InterpError> {
        let value = self.frame.closure.upvalues[idx as usize].borrow().inner_value.clone();
        self.push(value);
        self.bump_ip();
        Ok(())
    }

    fn capture_upvalue(&mut self, idx: u8) -> Rc<RefCell<WrappedValue>> {
        debug_println!("Capturing {:?} closure upvalues len {}", self.frame.closure.upvalues.get(idx as usize), self.frame.closure.upvalues.len());
        debug_println!("Func upvalues len: {:?}", self.frame.closure.func.upvalues);
        self.frame.closure.upvalues[idx as usize].clone()
    }

    fn closure(&mut self, func_idx: u8) -> Result<(), InterpError> {
        let func = &self.frame.closure.func.chunk.constants[func_idx as usize];
        if let Value::Func(f) = func {
            let mut closure = crate::closure::RtClosure::new(f.clone());

            debug_println!("Function {} {:?}", f.name, f.upvalues);

            for (idx, up) in f.upvalues.iter().enumerate() {
                match up {
                    Upvalue::FromParent(parent_idx) => {
                        debug_println!("Capturing from parent idx {}", parent_idx);
                        closure.upvalues[idx] = self.frame.closure.upvalues[*parent_idx as usize].clone();
                    }
                    Upvalue::Root(_) => {}
                }
            }
            self.push(Value::Closure(closure));
            self.bump_ip();
            Ok(())
        } else {
            panic!("Compile error: While making a closure expected a function on the stack but found a {}", func.tname())
        }
    }

    fn small_const(&mut self, val: u8) -> Result<(), InterpError> {
        debug_println!("Small const! {}", val);
        self.push(Value::Num(val as f64));
        self.bump_ip();
        Ok(())
    }

    fn div(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        let res = match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => Value::Num(b / a),
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot divide a {} and a {}", a.tname(), b.tname())))
        };
        self.push(res);
        self.bump_ip();
        Ok(())
    }

    fn mult(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        let res = match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => Value::Num(b * a),
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot multiply a {} and a {}", a.tname(), b.tname())))
        };
        self.push(res);
        self.bump_ip();
        Ok(())
    }

    fn sub(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        let res = match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => Value::Num(b - a),
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot subtract a {} and a {}", a.tname(), b.tname())))
        };
        self.push(res);
        self.bump_ip();
        Ok(())
    }

    fn load_const(&mut self, idx: u8) -> Result<(), InterpError> {
        let value: Value = self.frame.closure.chunk.constants.get(idx as usize).expect("Compiler error").clone();
        self.push(value);
        self.bump_ip();
        Ok(())
    }

    fn negate(&mut self) -> Result<(), InterpError> {
        let popped = self.pop()?;
        if let Value::Num(n) = popped {
            self.push(Value::Num(-n));
        } else {
            return Err(InterpError::runtime(
                Some(self.frame.closure.chunk.get_source(self.frame.ip).expect("Compiler error").clone()),
                format!("Tried to negate (-) a {}. You can only negate numbers.", popped.tname()),
            ));
        }
        self.bump_ip();
        Ok(())
    }

    fn add(&mut self) -> Result<(), InterpError> {
        let b = self.pop()?;
        let a = self.pop()?;
        let res = match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
            (Value::String(str1), Value::String(str2)) => {
                let new_str = format!("{}{}", str1.to_str(), str2.to_str());
                Value::String(self.symbolizer.get_symbol(new_str))
            }
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot add a {} and a {}", a.tname(), b.tname())))
        };
        self.push(res);
        self.bump_ip();
        Ok(())
    }

    fn ee(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;

        self.push(Value::Bool(
            match (&a, &b) {
                (Value::Num(a), Value::Num(b)) => (a == b),
                (Value::String(a), Value::String(b)) => (a == b),
                (Value::Bool(a), Value::Bool(b)) => (a == b),
                (Value::Nil, Value::Nil) => (a == b),
                _ => return Err(InterpError::runtime(
                    Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                    format!("Cannot compare a {} and a {}", a.tname(), b.tname()),
                ))
            }));
        self.bump_ip();
        Ok(())
    }

    fn ne(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        self.push(Value::Bool(a != b));
        self.bump_ip();
        Ok(())
    }

    fn lt(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b < a)),
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot compare {} and {} with the < or > operation", a.tname(), b.tname())))
        }
        self.bump_ip();
        Ok(())
    }

    fn lt_eq(&mut self) -> Result<(), InterpError> {
        let a = self.pop()?;
        let b = self.pop()?;
        match (&a, &b) {
            (Value::Num(a), Value::Num(b)) => self.push(Value::Bool(b <= a)),
            _ => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                 format!("Cannot compare {} and {} with the <= or >= operation", a.tname(), b.tname())))
        }
        self.bump_ip();
        Ok(())
    }

    fn get_local(&mut self, idx: u8) -> Result<(), InterpError> {
        #[cfg(debug_assertions)]
            self.print_stack_frame("get local");
        let peeked = self.peek_at(idx)?;
        self.push(peeked);
        self.bump_ip();
        Ok(())
    }

    fn set_local(&mut self, idx: u8) -> Result<(), InterpError> {
        self.stack[self.frame.frame_offset + idx as usize] = self.peek().clone();
        self.bump_ip();
        Ok(())
    }

    fn rel_jump_if_false(&mut self, offset: i16) -> Result<(), InterpError> {
        if !self.peek().truthy() {
            self.offset_ip(offset as isize);
        } else {
            self.bump_ip();
        }
        Ok(())
    }

    fn rel_jump_if_true(&mut self, offset: i16) -> Result<(), InterpError> {
        if self.peek().truthy() {
            self.offset_ip(offset as isize);
        } else {
            self.bump_ip();
        }
        Ok(())
    }

    fn rel_jump(&mut self, offset: i16) -> Result<(), InterpError> {
        self.offset_ip(offset as isize);
        Ok(())
    }

    fn call(&mut self, arity: u8) -> Result<(), InterpError> {
        self.bump_ip();
        #[cfg(debug_assertions)]
        self.print_stack_frame("Pre call stack: ");


        let peeked = self.peek_at((self.stack.len() - self.frame.frame_offset - (arity as usize) - 1) as u8)?;
        match peeked {
            Value::Closure(closure) => {
                debug_println!("{:?}", closure.func);
                debug_println!("Upvalues: {:?}", closure.func.upvalues);
                if arity != closure.arity {
                    return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()), format!("Function {} expects {} arguments but got {}", closure.name, closure.arity, arity)));
                }


                let frame_offset = self.stack.len() - (arity as usize) - 1;
                debug_println!("Frame offset is len({}) - arity({}) - 1 == {}" ,self.stack.len(), arity, frame_offset);

                let mut new_frame = CallFrame {
                    closure: closure.clone(),
                    ip: 0,
                    frame_offset,
                };

                swap(&mut new_frame, &mut self.frame);
                self.frames.push(new_frame);
                #[cfg(debug_assertions)]
                self.print_stack_frame(&format!("Post call stack frame-offset {}", self.frame.frame_offset));
            }
            Value::Native(native) => {
                if arity != native.arity() {
                    return Err(InterpError::runtime(
                        Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                        format!("Function {} expects {} arguments but got {}", native.name(), native.arity(), arity)));
                }
                self.push(native.call(vec![]));
            }
            _ => {
                return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                format!("Cannot call {}", peeked.tname())));
            }
        }
        Ok(())
    }
}
