use std::fmt::{Display, Formatter};
use std::ops::Deref;
use crate::ast::parser_func::ParserFunc;
use crate::ast::types::Stmt;
use crate::{Source, SourceRef, Symbol};
use crate::resolver::uniq_symbol::UniqSymbol;
use crate::resolver::Upvalue;
use crate::resolver::upvalue_update::VarRefResolved;
use crate::resolver::var_decl::VarDecl;
use crate::resolver::var_ref::VarRef;

#[derive(PartialEq, Clone, Debug)]
pub struct ResolvedFunc<DeclT: Display + Clone + PartialEq, RefT: Display + Clone + PartialEq> {
    pub upvalues: Vec<Upvalue>,
    pub func: Box<Stmt<DeclT, RefT, Self>>,
    pub name: DeclT,
    pub args: Vec<DeclT>,
    pub name_context: SourceRef,
    pub body_context: SourceRef,
}

impl<RefT: Display + Clone + PartialEq, DeclT: Display + Clone + PartialEq> Display for ResolvedFunc<RefT, DeclT> {
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

impl<DeclT: Display + Clone + PartialEq, RefT: Display + Clone + PartialEq> ResolvedFunc<DeclT, RefT> {
    pub fn new(name: DeclT,
               args: Vec<DeclT>,
               func: Stmt<DeclT, RefT, Self>,
               name_context: SourceRef,
               body_context: SourceRef,
               upvalues: Vec<Upvalue>) -> ResolvedFunc<DeclT, RefT> {
        ResolvedFunc { upvalues, func: Box::new(func), name, args, name_context, body_context }
    }
}