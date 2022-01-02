use std::rc::Rc;
use crate::value::Value;


#[derive(Clone, Debug)]
pub struct NativeFunc {
    inner: Rc<NativeFuncInner>,
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}

impl NativeFunc {
    pub fn new(name: &str, arity: u8, func: fn(Vec<Value>) -> Value) -> Self {
        NativeFunc {
            inner: Rc::new(NativeFuncInner {
                name: name.to_string(),
                func,
                arity,
            })
        }
    }
    pub fn arity(&self) -> u8 {
        self.inner.arity
    }
    pub fn name(&self) -> &str {
        &self.inner.name
    }
    pub fn call(&self, args: Vec<Value>) -> Value {
        (self.inner.func)(args)
    }
}

#[derive(Debug)]
struct NativeFuncInner {
    arity: u8,
    name: String,
    func: fn(Vec<Value>) -> Value,
}