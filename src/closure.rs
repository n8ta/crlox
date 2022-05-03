use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use crate::func::Func;
use crate::resolver::uniq_symbol::UniqSymbol;

/// Runtime closure
#[derive(Clone, PartialEq)]
pub struct RtClosure {
    pub func: Func,
}

impl Debug for RtClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("closure-{}", self.name))
    }
}

impl RtClosure {
    pub fn new(func: Func) -> Self {
        RtClosure { func }
    }
    pub fn name(&self) -> UniqSymbol {
        self.func.name()
    }
    pub fn arity(&self) -> u8 {
        self.func.arity()
    }
}

impl Deref for RtClosure {
    type Target = Func;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}