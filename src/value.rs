use std::fmt::{Debug, Display, Formatter};
use crate::ast::parser_func::ParserFunc;
use crate::native_func::NativeFunc;
use crate::Symbol;
use crate::resolver::resolved_func::ResolvedFunc;
// use crate::closure::RtClosure;
use crate::resolver::uniq_symbol::UniqSymbol;
use crate::resolver::upvalue_update::VarRefResolved;

#[derive(Clone, PartialEq)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    String(Symbol),
    Func(ResolvedFunc<VarRefResolved>),
    Native(NativeFunc),
    // Closure(RtClosure),
    // Class(Class),
    // Instance(Instance),
}

impl Value {
    pub fn tname(&self) -> &str {
        match self {
            Value::Num(_) => "number",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::String(_) => "string",
            Value::Func(_) => "fn",
            Value::Native(_) => "native fn",
            // Value::Closure(_) => "closure",
            // Value::Class(_) => "class",
            // Value::Instance(_) => "instance",
            // Value::BoundClosure(_) => "bound-closure",
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Value::Num(_) => true,
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Func(_) => true,
            Value::Native(_) => true,
            // Value::Closure(_) => true,
            // Value::Class(_) => true,
            // Value::Instance(_) => true,
            // Value::BoundClosure(_) => true,
            _ => true,
        }
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self))
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => f.write_str(&format!("{}", n)),
            Value::Bool(b) => f.write_str(&format!("{}", b)),
            Value::Nil => f.write_str("NIL"),
            Value::String(s) => f.write_str(&format!("{}", s)),
            Value::Func(lfn) => f.write_str(&format!("fn-{}[{}]", lfn.name, lfn.args.len())),
            Value::Native(nfn) => f.write_str(&format!("native-[{}]{}", nfn.name, nfn.arity)),
            // Value::Closure(c) => f.write_str(&format!("closure-{}[{}]", c.name(), c.arity())),
            // Value::Class(class) => f.write_str(&format!("class-{}",class.name())),
            // Value::Instance(inst) => f.write_str(&format!("instance-{}",inst.name())),
            // Value::BoundClosure(bc) => f.write_str(&format!("bound-closure-{}", bc.name()))
        }
    }
}