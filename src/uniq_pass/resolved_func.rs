use std::ops::Deref;
use crate::ast::parser_func::ParserFunc;
use crate::Symbol;
use crate::uniq_pass::uniq_symbol::UniqSymbol;
use crate::uniq_pass::Upvalue;
use crate::uniq_pass::var_decl::VarDecl;
use crate::uniq_pass::var_ref::VarRef;

pub struct ResolvedFunc {
    upvalues: Vec<Upvalue>,
    func: ParserFunc<VarDecl, VarRef>,
}

impl ResolvedFunc {
    pub fn new(func: ParserFunc<VarDecl, VarRef>, upvalues: Vec<Upvalue>) -> ResolvedFunc {
        ResolvedFunc { upvalues, func }
    }
}

impl Deref for ResolvedFunc {
    type Target = ParserFunc<VarDecl, VarRef>;
    fn deref(&self) -> &Self::Target {
        &self.func
    }
}