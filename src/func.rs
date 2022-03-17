use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::{Chunk, Symbol, Value};
use crate::ops::print_ops;

#[derive(Clone)]
pub struct Func {
    inner: &'static FuncInner,
}


impl Debug for Func {
    /// Print this function and any function in it's constants
    /// Hope there's no loops!
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("func<{}>\n", self.inner.name))?;
        f.write_str(&format!("{}", print_ops(&self.chunk.code)))?;

        f.write_str("\n")?;
        for constant in self.chunk.constants.iter() {
            if let Value::Func(const_func) = constant {
                f.write_str(&format!("{:?}", const_func))?;
            }
        }
        Ok(())

    }
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.inner, other.inner)
    }
}

impl Deref for Func {
    type Target = &'static FuncInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

#[derive(Debug)]
pub struct FuncInner {
    pub name: Symbol,
    pub chunk: Chunk,
    pub arity: u8,
    pub ftype: FuncType,
    pub upvalues: Vec<crate::compiler::Upvalue>,

}

impl Func {
    pub fn global(chunk: Chunk) -> Func {
        let inner: &'static FuncInner = Box::leak(Box::new(FuncInner {
            name: Symbol { sym: Rc::new(format!("Main function body")) },
            chunk,
            arity: 0,
            ftype: FuncType::Script,
            upvalues: vec![],
        }));
        Func {
            inner,
        }
    }
    pub fn new(name: Symbol, arity: u8, ftype: FuncType, chunk: Chunk, upvalues: Vec<crate::compiler::Upvalue>) -> Func {
        let inner: &'static FuncInner = Box::leak(Box::new(FuncInner {
            name,
            chunk,
            arity,
            ftype,
            upvalues,
        }));

        Func {
            inner,
        }
    }
}

#[derive(Debug)]
pub enum FuncType {
    Function,
    Script,
}