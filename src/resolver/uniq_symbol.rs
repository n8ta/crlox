use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use crate::{Symbol, Symbolizer};

#[derive(Clone)]
pub struct UniqSymbol {
    pub id: usize,
    pub symbol: Symbol,
}

impl PartialEq<Symbol> for UniqSymbol {
    fn eq(&self, other: &Symbol) -> bool {
        &self.symbol == other
    }
}

impl PartialEq for UniqSymbol {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Debug for UniqSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.id))
    }
}

impl Display for UniqSymbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.id))
    }
}

pub struct UniqSymbolizer {
    next: usize,
    symbolizer: Symbolizer,
}

impl UniqSymbolizer {
    pub fn new(symbolizer: Symbolizer) -> UniqSymbolizer { UniqSymbolizer { symbolizer, next: 1 } }
    pub fn gen(&mut self, symbol: Symbol) -> UniqSymbol {
        self.next += 1;
        UniqSymbol { id: self.next - 1, symbol }
    }
    pub fn root(&mut self) -> UniqSymbol {
        UniqSymbol { symbol: self.symbolizer.clone().get_symbol("root-function".to_string()), id: 0 }
    }
}