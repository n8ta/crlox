use std::fmt::{Display, Formatter};
use crate::resolver::UniqSymbol;

#[derive(Clone, Debug, PartialEq)]
pub struct Upvalue {
    pub sym: UniqSymbol,
    pub typ: UpvalueType,
}

impl Upvalue {
    pub fn new(sym: UniqSymbol, typ: UpvalueType) -> Self { Upvalue { sym, typ } }
}

#[derive(Clone, Debug, PartialEq)]
pub enum UpvalueType {
    Root,
    Captured(u8),
}

impl Display for UpvalueType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UpvalueType::Root => write!(f, "r"),
            UpvalueType::Captured(idx) => write!(f, "c{}", idx),
        }
    }
}