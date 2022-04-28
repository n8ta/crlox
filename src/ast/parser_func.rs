use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::source_ref::SourceRef;
use crate::ast::types::Stmt;
use crate::{Symbol, Symbolizer};


#[derive(Clone, Debug)]
pub struct ParserFunc<DeclT: Display + Clone + PartialEq, RefT: Display + Clone + PartialEq> {
    inner: Rc<ParserFuncInner<DeclT, RefT>>,
}

#[derive(Clone, Debug)]
pub struct ParserFuncInner<DeclT: Display + Clone + PartialEq, RefT: Display + Clone + PartialEq> {
    pub name: DeclT,
    pub args: Vec<DeclT>,
    pub body: Box<Stmt<DeclT, RefT>>,
    pub name_context: SourceRef,
    pub context: SourceRef,
}

impl<T: Display + Clone + PartialEq, S: Display + Clone + PartialEq> Deref for ParserFunc<T, S> {
    type Target = ParserFuncInner<T, S>;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Display + Clone + PartialEq, S: Display + Clone + PartialEq> Display for ParserFunc<T, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}(", self.name)?;
        for (i, arg) in self.inner.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ") \n{}\n\n", self.inner.body)
    }
}

impl<T: Clone + PartialEq + Display, S: Clone + PartialEq + Display> PartialEq for ParserFunc<T, S> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}


impl<T: Clone + PartialEq + Display, S: Clone + PartialEq + Display> ParserFunc<T, S> {
    pub fn new(name: T, args: Vec<T>, body: Stmt<T, S>, name_context: SourceRef, context: SourceRef) -> ParserFunc<T, S> {
        let inner = ParserFuncInner { name, args, body: Box::new(body), name_context, context };
        ParserFunc { inner: Rc::new(inner) }
    }
    pub fn root(symbolizer: Symbolizer, stmt: Stmt<Symbol, Symbol>) -> ParserFunc<Symbol, Symbol> {
        let mut s = symbolizer.clone();
        let name = s.get_symbol("root-func".to_string());
        let inner = ParserFuncInner { name, args: vec![], body: Box::new(stmt), name_context: SourceRef::simple(), context: SourceRef::simple() };
        ParserFunc { inner: Rc::new(inner) }
    }
}