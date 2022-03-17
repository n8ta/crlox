use std::cell::RefCell;
use std::rc::Rc;
use crate::Symbol;

#[derive(Clone)]
pub struct Class {
    inner: Rc<ClassInner>,
}

impl PartialEq for Class {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}
impl Class {
    pub fn new(name: Symbol) -> Self {
        Class { inner: Rc::new( ClassInner { name } ) }
    }
    pub fn name(&self) -> &Symbol {
        &self.inner.name
    }
}

struct ClassInner {
    name: Symbol,
}

pub struct Instance {
    inner: Rc<RefCell<InstanceInner>>,
}
struct InstanceInner {
    class: Class,
}

impl Instance {
    pub fn new(class: Class) -> Self {
        Instance { inner: Rc::new(RefCell::new(InstanceInner { class } )) }
    }
    pub fn name(&self) -> Symbol {
        self.inner.borrow().class.name().clone()
    }
}
impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}