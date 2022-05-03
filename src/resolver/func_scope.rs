use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, UpperExp};
use std::rc::Rc;
use crate::ast::types::{Expr, ExprInContext, ExprTy, Stmt};
use crate::{Source, SourceRef, Symbol, Symbolizer};
use crate::ast::parser::Parser;
use crate::ast::parser_func::{ParserFunc, ParserFuncInner};
use crate::printable_error::PrintableError;
use crate::resolver::resolved_func::ResolvedFunc;
use crate::resolver::uniq_symbol::{UniqSymbol, UniqSymbolizer};
use crate::resolver::{Upvalue, UpvalueType};
use crate::resolver::var_decl::VarDecl;
use crate::resolver::var_ref::VarRef;


#[derive(Clone)]
pub struct FuncScope {
    pub upvalues: Vec<Upvalue>,
    pub scopes: Vec<Vec<VarDecl>>,
}

impl Debug for FuncScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Func - {:?}\n", self.upvalues))?;
        f.write_str(&format!("    scopes: {:?}\n", self.scopes))
    }
}
impl FuncScope {
    pub fn new() -> Self {
        FuncScope {
            upvalues: vec![],
            scopes: vec![vec![]],
        }
    }
    pub fn add_root(&mut self, var: UniqSymbol) -> u8 {
        if let Some((idx, _)) = self.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == var) {
            idx as u8
        } else {
            self.upvalues.push(Upvalue::new(var.clone(), UpvalueType::Root));
            (self.upvalues.len() - 1) as u8

        }
    }
    pub fn capture_upvalue(&mut self, parent_idx: u8, sym: UniqSymbol) -> u8 {
        if let Some((idx, _)) = self.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == sym) {
            idx as u8
        } else {
            self.upvalues.push(Upvalue::new(sym, UpvalueType::Captured(parent_idx)));
            (self.upvalues.len() - 1) as u8
        }
    }
}