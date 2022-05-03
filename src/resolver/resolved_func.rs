use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::ast::parser_func::ParserFunc;
use crate::ast::types::Stmt;
use crate::{Source, SourceRef, Symbol};
use crate::resolver::uniq_symbol::UniqSymbol;
use crate::resolver::Upvalue;
use crate::resolver::var_decl::VarDecl;
use crate::resolver::var_ref::VarRef;

#[derive(PartialEq, Clone, Debug)]
pub struct ResolvedFunc<RefT: Display + Clone + PartialEq> {
    pub upvalues: Vec<Upvalue>,
    pub func: Box<Stmt<VarDecl, RefT, Self>>,
    pub name: VarDecl,
    pub args: Vec<VarDecl>,
    pub name_context: SourceRef,
    pub body_context: SourceRef,
}

impl<RefT: Display + Clone + PartialEq> Display for ResolvedFunc<RefT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "fun {}", self.name)?;
        f.write_str("[")?;
        for (i, up) in self.upvalues.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }

            write!(f, "{}{}", up.sym, up.typ)?;
        }
        f.write_str("](")?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ") \n{}\n\n", self.func)
    }
}

impl<T: Display + Clone + PartialEq> ResolvedFunc<T> {
    pub fn new(name: VarDecl,
               args: Vec<VarDecl>,
               func: Stmt<VarDecl, T, Self>,
               name_context: SourceRef,
               body_context: SourceRef,
               upvalues: Vec<Upvalue>) -> ResolvedFunc<T> {
        ResolvedFunc { upvalues, func: Box::new(func), name, args, name_context, body_context }
    }
}