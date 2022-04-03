use crate::class::Instance;
use crate::closure::RtClosure;
use crate::Symbol;

#[derive(Clone)]
pub struct BoundClosure {
    closure: RtClosure,
    receiver: Instance,
}

impl PartialEq for BoundClosure {
    fn eq(&self, other: &Self) -> bool {
        self.closure == other.closure && self.receiver == other.receiver
    }
}

impl BoundClosure {
    pub fn new(closure: RtClosure, receiver: Instance) -> Self {
        BoundClosure { closure, receiver }
    }
    pub fn name(&self) -> &Symbol {
        &self.closure.name
    }
}
