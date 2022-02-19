use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::{Chunk, Symbol};
use crate::func::Func;
use crate::value::Value;

/// Runtime closure
#[derive(Clone, PartialEq)]
pub struct RtClosure {
    pub func: Func,
    pub upvalues: Vec<Rc<Value>>,
}

impl Debug for RtClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("closure-{}", self.name))
    }
}

impl RtClosure {
    pub fn new(func: Func) -> Self {
        RtClosure { func, upvalues: Vec::new() }
    }
}

impl Deref for  RtClosure {
    type Target = Func;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}