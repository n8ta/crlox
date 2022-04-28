
use std::cell::RefCell;
use std::fmt::{Display, Formatter, UpperExp};
use std::rc::Rc;
use crate::ast::types::{Expr, ExprInContext, ExprTy, Stmt};
use crate::{Source, SourceRef, Symbol, Symbolizer};
use crate::ast::parser_func::ParserFunc;
use crate::printable_error::PrintableError;
use crate::uniq_pass::uniq_symbol::{UniqSymbol, UniqSymbolizer};
use crate::uniq_pass::var_decl::VarDecl;


impl Display for VarRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("r{}", self.decl))
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct VarRef {
    decl: VarDecl
}

impl VarRef {
    pub fn new(decl: &VarDecl) -> Self {
        Self { decl: decl.clone() }
    }
}