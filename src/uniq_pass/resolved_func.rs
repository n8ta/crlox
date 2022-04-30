use std::ops::Deref;
use crate::ast::parser_func::ParserFunc;
use crate::Symbol;
use crate::uniq_pass::uniq_symbol::UniqSymbol;
use crate::uniq_pass::var_decl::VarDecl;
use crate::uniq_pass::var_ref::VarRef;

pub struct ResolvedFunc {
    upvalues: Vec<UniqSymbol>,
    func: ParserFunc<VarDecl, VarRef>,
}

impl ResolvedFunc {
    pub fn new(func: ParserFunc<VarDecl, VarRef>) -> ResolvedFunc {
        ResolvedFunc { upvalues: vec![], func }
    }
    pub fn add_upvalue(&mut self, up: UniqSymbol) -> u8{
        self.upvalues.push(up);
        (self.upvalues.len() - 1) as u8
    }
}

impl Deref for ResolvedFunc {
    type Target = ParserFunc<VarDecl, VarRef>;
    fn deref(&self) -> &Self::Target {
        &self.func
    }
}

#[derive(Clone)]
pub enum VarResolution {
    Local(u8),
    Upvalue(u8),
}
