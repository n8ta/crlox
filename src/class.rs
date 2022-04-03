use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use crate::{Symbol, Value};
use crate::closure::RtClosure;
use crate::func::Func;

#[derive(Clone, Debug)]
pub struct Class {
    pub inner: Rc<RefCell<ClassInner>>,
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}
impl Class {
    pub fn new(name: Symbol) -> Self {
        Class { inner: Rc::new( RefCell::new(ClassInner { name, methods: HashMap::new() } )) }
    }
    pub fn name(&self) -> Symbol {
        self.inner.borrow().name.clone()
    }
    pub fn add_method(&self, closure: RtClosure) {
        self.inner.borrow_mut().methods.insert(closure.name.clone(), closure);
    }
    pub fn get_method(&self, name: &Symbol) -> Option<RtClosure> {
        self.inner.borrow().methods.get(name).cloned()
    }
}

#[derive(Debug)]
pub struct ClassInner {
    pub name: Symbol,
    pub methods: HashMap<Symbol, RtClosure>
}

#[derive(Clone)]
pub struct Instance {
    inner: Rc<RefCell<InstanceInner>>,
}
#[derive(Clone)]
struct InstanceInner {
    class: Class,
    props: HashMap<Symbol, Value>
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Instance { inner: Rc::new(RefCell::new(InstanceInner { class, props: HashMap::new() } )) }
    }
    pub fn name(&self) -> Symbol {
        self.inner.borrow().class.name().clone()
    }
    pub fn get_prop(&self, name: &Symbol) -> Option<Value> {
        match self.inner.borrow().props.get(name) {
            None => None,
            Some(v) => Some(v.clone())
        }
    }
    pub fn set_prop(&self, name: &Symbol, value: Value) {
        self.inner.borrow_mut().props.insert(name.clone(), value);
    }
}
impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}