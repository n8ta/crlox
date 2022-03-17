use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::func::Func;
use crate::value::Value;

pub type UpvalueList = Vec<Rc<RefCell<WrappedValue>>>;

#[derive(Debug)]
pub struct WrappedValue {
    pub inner_value: Value,
}

/// Runtime closure
#[derive(Clone)]
pub struct RtClosure {
    pub func: Func,
    pub upvalues: UpvalueList,
}

impl PartialEq for RtClosure {
    fn eq(&self, other: &Self) -> bool {
        let mut eq = self.func == other.func && self.upvalues.len() == other.upvalues.len();
        if !eq { return false; }
        for (a,b) in self.upvalues.iter().zip(other.upvalues.iter()) {
            eq = eq && Rc::ptr_eq(a,b);
        }
        eq
    }
}

impl Debug for RtClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("closure-{}", self.name))
    }
}

impl RtClosure {
    pub fn new(func: Func) -> Self {
        let mut upvalues: UpvalueList = vec![];
        for _ in 0..func.upvalues.len() {
            upvalues.push(Rc::new(RefCell::new(WrappedValue{inner_value: Value::Nil})))
        }
        RtClosure { func, upvalues }
    }
}

impl Deref for  RtClosure {
    type Target = Func;

    fn deref(&self) -> &Self::Target {
        &self.func
    }
}