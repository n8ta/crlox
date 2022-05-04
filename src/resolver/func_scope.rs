use std::fmt::{Debug, Formatter};
use crate::resolver::uniq_symbol::{UniqSymbol};
use crate::resolver::{Upvalue, UpvalueType};
use crate::resolver::var_decl::VarDecl;


#[derive(Clone)]
pub struct FuncScope {
    pub upvalues: Vec<Upvalue>,
    pub scopes: Vec<Vec<VarDecl>>,
}

impl Debug for FuncScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Func - {:?}\n", self.upvalues))?;
        f.write_str(&format!("    scopes: {:?}\n", self.scopes))
    }
}
impl FuncScope {
    pub fn new() -> Self {
        FuncScope {
            upvalues: vec![],
            scopes: vec![vec![]],
        }
    }
    pub fn add_root(&mut self, var: UniqSymbol) -> u8 {
        if let Some((idx, _)) = self.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == var) {
            idx as u8
        } else {
            self.upvalues.push(Upvalue::new(var.clone(), UpvalueType::Root));
            (self.upvalues.len() - 1) as u8

        }
    }
    pub fn capture_upvalue(&mut self, parent_idx: u8, sym: UniqSymbol) -> u8 {
        if let Some((idx, _)) = self.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == sym) {
            idx as u8
        } else {
            self.upvalues.push(Upvalue::new(sym, UpvalueType::Captured(parent_idx)));
            (self.upvalues.len() - 1) as u8
        }
    }
}