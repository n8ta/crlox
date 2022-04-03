use std::cell::RefCell;
use crate::source_ref::SourceRef;
use std::rc::Rc;
use crate::ast::types::Stmt;
use crate::Symbol;

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum Upvalue {
    FromParent(u8),
    // this u8 is an index in the parent's Vec<Rc<Value>> upvalues
    Root(u8), // this u8 is the index of the local
}

#[derive(Clone, Debug)]
/// Parser Representation of a Func
pub struct ParserFunc {
    pub inner: Rc<ParserFuncInner>,
}

#[derive(Clone, Debug)]
pub struct ParserFuncInner {
    pub name: Symbol,
    pub args: Vec<Symbol>,
    pub body: RefCell<Stmt>,
    pub name_context: SourceRef,
    pub context: SourceRef,
    pub upvalues: Vec<Upvalue>,
}

impl ParserFuncInner {
    fn new(name: Symbol, args: Vec<Symbol>, body: Stmt, name_context: SourceRef, context: SourceRef, upvalues: Vec<Upvalue>) -> ParserFuncInner {
        ParserFuncInner { name, args, body: RefCell::new(body), name_context, context, upvalues }
    }
}

impl ParserFunc {
    pub fn new(name: Symbol, args: Vec<Symbol>, body: Stmt, name_context: SourceRef, context: SourceRef, upvalues: Vec<Upvalue>) -> ParserFunc {
        ParserFunc { inner: Rc::new(ParserFuncInner::new(name, args, body, name_context, context, upvalues)) }
    }
    pub fn name(&self) -> &Symbol {
        &self.inner.name
    }
}