use std::fmt::{Display, Formatter};
use crate::func::Func;
use crate::Symbol;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
    String(Symbol),
    Func(Func)
}

// #[derive(Clone, Debug, PartialEq)]
// pub enum HValue {
//     String(String),
// }
// impl Display for HValue {
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         match self {
//             HValue::String(s) => f.write_str(&format!("{}", &s))
//         }
//     }
// }

// impl HValue {
//     pub fn tname(&self) -> &str {
//         match self {
//             HValue::String(_) => "string",
//         }
//     }
// }

impl Value {
    pub fn tname(&self) -> &str {
        match self {
            Value::Num(_) => "number",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
            Value::String(_) => "string",
            Value::Func(_) => "fn",
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Value::Num(_) => true,
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Func(_) => true,
            _ => true,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => f.write_str(&format!("{}", n)),
            Value::Bool(b) => f.write_str(&format!("{}", b)),
            Value::Nil => f.write_str("nil"),
            Value::String(s) => f.write_str(&format!("str'{}'", s)),
            Value::Func(lfn) => f.write_str(&format!("fn[{}]<{}>", lfn.arity, lfn.name)),
        }
    }
}