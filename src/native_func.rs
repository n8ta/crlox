use std::ops::Deref;
use crate::value::Value;


#[derive(Clone, Debug)]
pub struct NativeFunc {
    inner: &'static NativeFuncInner,
}


impl Deref for NativeFunc {
    type Target = &'static NativeFuncInner;
    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.inner, other.inner)
    }
}

impl NativeFunc {
    pub fn new(name: &str, arity: u8, func: fn(Vec<Value>) -> Value) -> Self {
        let inner: &'static NativeFuncInner =  Box::leak(Box::new(NativeFuncInner {
            name: name.to_string(),
            func,
            arity,
        }));
        NativeFunc {
            inner
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
pub struct NativeFuncInner {
    pub arity: u8,
    pub name: String,
    func: fn(Vec<Value>) -> Value,
}