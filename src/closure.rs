use std::cell::RefCell;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::rc::Rc;
use crate::debug_println;
use crate::func::Func;
use crate::resolver::uniq_symbol::UniqSymbol;
use crate::resolver::UpvalueType;
use crate::value::Value;

#[derive(Debug)]
pub struct WrappedValue {
    pub inner_value: Value,
}

/// Runtime closure
#[derive(Clone)]
pub struct RtClosure {
    pub func: Func,
    pub live_upvalues: Vec<Rc<RefCell<WrappedValue>>>,
}

impl Debug for RtClosure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("closure-{}", self.name))
    }
}

impl PartialEq for RtClosure {
    fn eq(&self, other: &Self) -> bool {
        let mut eq = self.func == other.func && self.upvalues.len() == other.upvalues.len();
        if !eq { return false; }
        for (a, b) in self.live_upvalues.iter().zip(other.live_upvalues.iter()) {
            eq = eq && Rc::ptr_eq(a, b);
        }
        eq
    }
}

impl RtClosure {
    pub fn new(func: Func, parent_upvalues: &mut Vec<Rc<RefCell<WrappedValue>>>) -> Self {
        debug_println!("Creating closure for {} with {} parent upvalues", func.name.symbol.symbol,parent_upvalues.len());
        let mut live_upvalues = vec![];
        for up in &func.upvalues {
            live_upvalues.push(match up.typ {
                UpvalueType::Root => Rc::new(RefCell::new(WrappedValue { inner_value: Value::Nil })),
                UpvalueType::Captured(idx) => parent_upvalues[idx as usize].clone(),
            });
        }
        RtClosure { func, live_upvalues }
    }
    pub fn name(&self) -> UniqSymbol {
        self.func.name().symbol
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