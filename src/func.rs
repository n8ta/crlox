use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;
use crate::{Chunk, Symbol};

#[derive(Clone, Debug)]
pub struct Func {
    inner: Rc<FuncInner>,
}

impl PartialEq for Func {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(self, other)
    }
}

impl Deref for Func {
    type Target = Rc<FuncInner>;

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
}

impl Func {
    pub fn global(chunk: Chunk) -> Func {
        Func {
            inner: Rc::new(FuncInner {
                name: Symbol { sym: Rc::new(format!("Main function body")) },
                chunk,
                arity: 0,
                ftype: FuncType::Script,
            })
        }
    }
    pub fn new(name: Symbol, arity: u8, ftype: FuncType, chunk: Chunk) -> Func {
        Func {
            inner: Rc::new(FuncInner {
                name,
                chunk,
                arity,
                ftype,
            })
        }
    }
}

#[derive(Debug)]
pub enum FuncType {
    Function,
    Script,
}