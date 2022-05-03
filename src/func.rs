use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use crate::chunk::Chunk;
use crate::ops::print_ops;
use crate::resolver::uniq_symbol::UniqSymbol;
use crate::resolver::upvalue_update::VarRefResolved;
use crate::value::Value;

#[derive(Clone, PartialEq)]
pub struct Func {
    inner: FuncInner,
}

impl Deref for Func {
    type Target = FuncInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}


impl Debug for Func {
    /// Print this function and any function in it's constants
    /// Hope there's no loops!
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("func<{}>\n", self.inner.name))?;
        f.write_str(&format!("{}", print_ops(&self.inner.chunk.code)))?;

        f.write_str("\n")?;
        for constant in self.inner.chunk.constants.iter() {
            if let Value::Func(const_func) = constant {
                f.write_str(&format!("{:?}", const_func))?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FuncInner {
    pub name: UniqSymbol,
    pub chunk: Chunk,
    pub arity: u8,
    pub ftype: FuncType,

}

impl Func {
    pub fn name(&self) -> UniqSymbol {
        self.inner.name.clone()
    }
    pub fn arity(&self) -> u8 {
        self.inner.arity
    }
    pub fn chunk(&self) -> &Chunk {
        &self.inner.chunk
    }

    pub fn new(name: UniqSymbol, arity: u8, ftype: FuncType, chunk: Chunk) -> Func {
        let inner = FuncInner {
            name,
            chunk,
            arity,
            ftype,
        };

        Func {
            inner,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum FuncType {
    Method,
    Function,
}