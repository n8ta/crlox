
use std::fmt::{Display, Formatter};
use crate::resolver::var_decl::VarDecl;


impl Display for VarRef {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("r{}", self.decl))
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct VarRef {
    pub decl: VarDecl
}

impl VarRef {
    pub fn new(decl: &VarDecl) -> Self {
        Self { decl: decl.clone() }
    }
}