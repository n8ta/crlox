use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::swap;
use std::rc::Rc;
use std::slice::from_raw_parts;
use std::time::{SystemTime, UNIX_EPOCH};
use crate::ops::{EqualEqual, SetUpValue, GetUpValue, False, Less, LessOrEq, Nil, Not, NotEqual, True, Print, Ret, Const, Negate, Add, Sub, Mult, Div, Pop, DefGlobal, GetGlobal, SetGlobal, GetLocal, SetLocal, RelJumpIfFalse, RelJump, Call, SmallConst, Closure};
use crate::value::Value;
use crate::ops::OpTrait;
use crate::{debug_println, SourceRef, Symbol, Symbolizer};
use crate::func::Func;
use crate::native_func::NativeFunc;
use crate::scanner::TType::PRINT;
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

static MAX_LOCALS: usize = 255;
static MAX_STACK_SIZE: usize = 255;

pub struct VM {
    stack_idx: usize,
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
            stack_idx: 0,
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

    fn read_byte(&mut self) -> Result<u8, InterpError> {
        let ip = self.frame.ip;
        match self.frame.closure.chunk.code.get(ip) {
            Some(byte) => Ok(*byte),
            None => {
                match self.frame.closure.chunk.get_source(ip) {
                    None => Err(InterpError::compile(None, format!("Tried to read a byte but were unable to!"))),
                    Some(src) => Err(InterpError::compile(Some(src.clone()), format!("Tried to read a byte but were unable to!"))),
                }
            }
        }
    }

    fn pop(&mut self) -> Result<Value, InterpError> {
        match self.stack.pop() {
            None => {
                let ip = self.frame.ip;
                let src: Option<SourceRef> = self.frame.closure.chunk.get_source(ip).and_then(|f| Some(f.clone()));
                panic!("Tried to pop a value from the stack but it was empty!")
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
            None => panic!("panic"),
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
            let inst = self.read_byte()?;
            let ip = self.frame.ip;
            self.frame.closure.chunk.disassemble_op(&inst, ip + 1);
            match inst {
                Ret::CODE => {
                    #[cfg(debug_assertions)]
                        self.print_stack_frame("Pre ret stack");
                    let (_len, _ret) = Ret::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
                    if let Some(frame) = self.frames.pop() {
                        let ret = self.pop()?;
                        for _ in 0..self.frame.closure.arity + 1 {
                            self.pop()?;
                        }
                        self.frame = frame;
                        debug_println!("Function returned {}", ret);
                        self.push(ret);
                        #[cfg(debug_assertions)]
                            self.print_stack_frame("Post ret stack");
                    } else {
                        #[cfg(debug_assertions)]
                            self.print_stack_frame("Final return stack");
                        return Ok(Value::Nil);
                    }
                }
                Const::CODE => {
                    self.load_const()?;
                }
                Negate::CODE => {
                    self.negate()?;
                }
                Add::CODE => {
                    self.add()?;
                }
                Sub::CODE => {
                    self.sub()?;
                }
                Mult::CODE => {
                    self.mult()?;
                }
                Div::CODE => {
                    self.div()?;
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
                    self.ee()?;
                }
                NotEqual::CODE => {
                    self.ne()?;
                }
                Less::CODE => {
                    self.lt()?;
                }
                LessOrEq::CODE => {
                    self.lt_eq()?;
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
                    self.def_global()?;
                }
                GetGlobal::CODE => {
                    self.get_global()?;
                }
                SetGlobal::CODE => {
                    self.set_global()?;
                }
                GetLocal::CODE => {
                    self.get_local()?;
                }
                SetLocal::CODE => {
                    self.set_local()?;
                }
                RelJumpIfFalse::CODE => {
                    self.rel_jump_if_false()?;
                }
                RelJump::CODE => {
                    self.rel_jump()?;
                }
                Call::CODE => {
                    self.call()?;
                }
                SmallConst::CODE => {
                    self.small_const()?;
                }
                Closure::CODE => {
                    self.closure()?;
                }
                GetUpValue::CODE => {
                    self.get_up_value()?;
                }
                SetUpValue::CODE => {
                    self.set_up_value()?;
                }
                _ => return Err(InterpError::compile(None, format!("Hit an unknown bytecode opcode {}, this is a compiler bug", inst)))
            };
        }
    }

    fn set_up_value(&mut self) -> Result<(), InterpError> {
        let (_len, op) = SetUpValue::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        self.offset_ip_pos(2);
        self.frame.closure.upvalues[op.idx as usize] = Rc::new(self.peek().clone());
        Ok(())
    }

    fn get_up_value(&mut self) -> Result<(), InterpError> {
        let (_len, op) = GetUpValue::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        self.push((*self.frame.closure.upvalues[op.idx as usize].clone()).clone());
        self.offset_ip_pos(2);
        Ok(())
    }
    fn upvalue(&mut self) -> Result<(), InterpError> {
        Ok(())
    }

    fn closure(&mut self) -> Result<(), InterpError> {
        let (len, op) = Closure::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        let func = &self.frame.closure.func.chunk.constants[op.idx as usize];
        if let Value::Func(f) = func {
            let mut closure = crate::closure::RtClosure::new(f.clone());

            let base = self.frame.ip + 2;
            debug_println!("Function {} has {} upvalues", f.name, f.upvalues);

            for i in 0..f.upvalues {
                let idx = self.frame.closure.func.chunk.code[base + (i * 2)];
                let is_local = self.frame.closure.func.chunk.code[base + (i * 2) + 1] == 1;
                if is_local {
                    closure.upvalues.push(Rc::new(self.peek_at(idx)?));
                } else {
                    // todo: non-local upvalues
                    panic!("not done yet")
                }
            }
            self.push(Value::Closure(closure));
            self.offset_ip_pos(len + 1 + f.upvalues * 2);
            Ok(())
        } else {
            panic!("Compile error: While making a closure expected a function on the stack but found a {}", func.tname())
        }
    }

    fn small_const(&mut self) -> Result<(), InterpError> {
        let (_len, op) = SmallConst::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        debug_println!("Small const! {}", op.val);
        self.push(Value::Num(op.val as f64));
        self.offset_ip_pos(2);
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

    fn set_global(&mut self) -> Result<(), InterpError> {
        let (_, set_global) = SetGlobal::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        let global_name = self.frame.closure.chunk.constants.get(set_global.idx as usize).expect("Compiler error, bad index in get global op");
        let global_name = if let Value::String(global_name_sym) = global_name {
            global_name_sym.clone()
        } else { panic!("Compiler error, non-string constant passed to GetGlobal"); };

        let new_val = self.peek();
        if self.globals.contains_key(&global_name) {
            self.globals.insert(global_name.clone(), new_val.clone());
        } else {
            return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                            format!("Cannot assign to global variable {} since it has not been declared", global_name)));
        }
        self.offset_ip_pos(2);
        Ok(())
    }

    fn get_global(&mut self) -> Result<(), InterpError> {
        let (_, get_global) = GetGlobal::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        let global_idx = get_global.idx as usize;
        let global_name = self.frame.closure.chunk.constants.get(global_idx).expect("Compiler error, bad index in get global op");
        let global_name = if let Value::String(global_name) = global_name {
            debug_println!("Getting global {}", global_name);
            global_name.clone()
        } else {
            panic!("Compiler error, non-string constant passed to GetGlobal");
        };
        match self.globals.get(&global_name) {
            None => return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                                                    format!("Global variable {} not found", global_name))),
            Some(v) => {
                debug_println!("\tGlobal is {}", v);
                self.push(v.clone())
            }
        }
        self.offset_ip_pos(2);
        Ok(())
    }

    fn def_global(&mut self) -> Result<(), InterpError> {
        let (_, def_global) = DefGlobal::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        let popped = self.pop()?;
        let global_name = self.frame.closure.chunk.constants.get(def_global.idx as usize).unwrap();
        debug_println!("Defining global '{}' as '{}'", global_name, popped);
        let sym = if let Value::String(sym) = global_name {
            sym.clone()
        } else {
            panic!("Compiler error, non-string constant passed to DefGlobal");
        };
        self.globals.insert(sym, popped);
        self.offset_ip_pos(2);
        Ok(())
    }

    fn load_const(&mut self) -> Result<(), InterpError> {
        let (_, con) = Const::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        let value: Value = self.frame.closure.chunk.constants.get(con.idx as usize).expect("Compiler error").clone();
        self.push(value);
        self.offset_ip_pos(2);
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
                let mut new_str = format!("{}{}", str1.to_str(), str2.to_str());
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

    fn get_local(&mut self) -> Result<(), InterpError> {
        let (_, get_local) = GetLocal::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        debug_println!("Pre get local[idx {}]", get_local.idx);
        #[cfg(debug_assertions)]
            self.print_stack_frame("get local");
        let peeked = self.peek_at(get_local.idx)?;
        debug_println!("Got local {:?}", &peeked);
        self.push(peeked);
        self.offset_ip_pos(2);
        Ok(())
    }

    fn set_local(&mut self) -> Result<(), InterpError> {
        let (_, set_local) = SetLocal::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        // let stack_len = self.stack.len();
        debug_println!("Setting local to {}", self.peek().clone());
        self.stack[self.frame.frame_offset + set_local.idx as usize] = self.peek().clone();
        self.offset_ip_pos(2);
        Ok(())
    }

    fn rel_jump_if_false(&mut self) -> Result<(), InterpError> {
        let (_, jump) = RelJumpIfFalse::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        if !self.peek().truthy() {
            self.offset_ip(jump.idx as isize);
        } else {
            self.offset_ip_pos(3);
        }
        Ok(())
    }

    fn rel_jump(&mut self) -> Result<(), InterpError> {
        let (_, jump) = RelJump::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        self.offset_ip(jump.idx as isize);
        Ok(())
    }

    fn call(&mut self) -> Result<(), InterpError> {
        let (_, call) = Call::decode(&self.frame.closure.chunk.code, self.frame.ip + 1);
        self.offset_ip_pos(2);
        #[cfg(debug_assertions)]
            self.print_stack_frame("Pre call stack: ");
        let peeked = self.peek_at((self.stack.len() - self.frame.frame_offset - (call.arity as usize) - 1) as u8)?;
        match peeked {
            Value::Closure(closure) => {
                if call.arity != closure.arity {
                    return Err(InterpError::runtime(Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()), format!("Function {} expects {} arguments but got {}", closure.name, closure.arity, call.arity)));
                }
                debug_println!("new func disass");
                #[cfg(debug_assertions)]
                closure.chunk.disassemble();

                let frame_offset = self.stack.len() - (call.arity as usize) - 1;
                debug_println!("Frame offset is len({}) - arity({}) - 1 == {}" ,self.stack.len(), call.arity, frame_offset);

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
                if call.arity != native.arity() {
                    return Err(InterpError::runtime(
                        Some(self.frame.closure.chunk.get_source(self.frame.ip).unwrap().clone()),
                        format!("Function {} expects {} arguments but got {}", native.name(), native.arity(), call.arity)));
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
