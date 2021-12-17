use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub enum Value {
    Num(f64),
    Bool(bool),
    Nil,
}

impl Value {
    pub fn tname(&self) -> &str {
        match self {
            Value::Num(_) => "number",
            Value::Bool(_) => "bool",
            Value::Nil => "nil",
        }
    }
    pub fn truthy(&self) -> bool {
        match self {
            Value::Num(_) => true,
            Value::Nil => false,
            Value::Bool(b) => *b,
            //             Value::STRING(_) => true,
            //             Value::FUNC(_) => true,
            //             Value::INSTANCE(_) => true,
            //             Value::CLASS(_) => true,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Num(n) => f.write_str(&format!("{}", n)),
            Value::Bool(b) => f.write_str(&format!("{}", b)),
            Value::Nil => f.write_str("nil"),
        }
    }
}