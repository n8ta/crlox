use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::{Chunk, Symbol};

#[derive(Clone)]
pub struct Func {
    inner: &'static FuncInner,
}

impl Debug for Func {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("func<{}>", self.inner.name))
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
    pub upvalues: usize,
}

impl Func {
    pub fn global(chunk: Chunk) -> Func {
        let inner: &'static FuncInner = Box::leak(Box::new(FuncInner {
            name: Symbol { sym: Rc::new(format!("Main function body")) },
            chunk,
            arity: 0,
            ftype: FuncType::Script,
            upvalues: 0,
        }));
        Func {
            inner,
        }
    }
    pub fn new(name: Symbol, arity: u8, ftype: FuncType, chunk: Chunk, upvalues: usize) -> Func {
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