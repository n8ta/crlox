use crate::{SourceRef, Symbol};
use crate::compiler::{CompilerError};

#[derive(Debug, Clone)]
pub struct Local {
    name: Symbol,
    src: SourceRef,
    initialized: bool,
}

impl Local {
    pub fn new(name: Symbol, src: SourceRef) -> Self {
        Local { name, src, initialized: false }
    }
}

fn flat(scopes: &Vec<Vec<Local>>) -> Vec<&Local> {
    let mut flattened = vec![];
    for scope in scopes {
        for local in scope {
            flattened.push(local)
        }
    }
    flattened
}

pub struct Resolver {
    scopes: Vec<Vec<Local>>,
}

impl Resolver {
    pub fn resolve_local(&self, name: &Symbol, _src: &SourceRef) -> Option<u8> {
        let flat = flat(&self.scopes);
        if let Some((idx, _local)) = flat.iter().enumerate().rev().find(|(_, local)| &local.name == name) {
            Some(idx as u8)
        } else {
            None
        }
    }
    pub fn new() -> Resolver {
        Resolver { scopes: vec![vec![]] }
    }
    pub fn add_local(&mut self, sym: Symbol, src: SourceRef) -> Result<(), CompilerError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.len() >= 255 {
                Err(CompilerError::new(format!("Hit maximum of 255 local variables in scope"), src))
            } else {
                scope.push(Local::new(sym, src));
                Ok(())
            }
        } else {
            Err(CompilerError::new(format!("Expected to be in a scope when adding a local"), src))
        }
    }
    pub fn mark_initialized(&mut self, sym: Symbol, src: SourceRef) -> Result<(), CompilerError> {
        let mut found_var = false;
        for scope in self.scopes.iter_mut().rev() {
            if let Some(found) = scope.iter_mut().find(|l| l.name == sym) {
                found.initialized = true;
                found_var = true;
                break;
            }
        }
        if !found_var {
            return Err(CompilerError::new(format!("Unable to find variable {} to init", sym), src));
        }
        Ok(())
    }
    pub fn begin_scope(&mut self) {
        self.scopes.push(vec![])
    }
    pub fn end_scope(&mut self) -> Vec<Local> {
        self.scopes.pop().unwrap()
    }
    pub fn declare_variable(&mut self, name: Symbol, src: SourceRef) -> Result<(), CompilerError> {
        if let Some(scope) = self.scopes.last() {
            for local in scope {
                if local.name == name {
                    return Err(CompilerError::new(format!("Cannot define two variables with the name {}", name), src));
                }
            }
            self.add_local(name, src)
        } else {
            Err(CompilerError::new(format!("Compiler bug, no scope found {}", name), src))
        }
    }
}

// #[test]
// fn scopes() {
//     use crate::Symbolizer;
//     let src = || { SourceRef::simple() };
//     let mut symbolizer = Symbolizer::new();
//     let a = symbolizer.get_symbol("a".to_string());
//     let mut res = Resolver::new();
//     res.declare_variable(a.clone(), src()).ok().unwrap();
//     res.mark_initialized(a.clone(), src()).ok().unwrap();
//     assert_eq!(res.resolve_local(&a.clone(), &src()).ok().unwrap(), Resolution::Local(0));
//     assert_ne!(res.resolve_local(&a.clone(), &src()).ok().unwrap(), Resolution::Local(1));
//
//     res.begin_scope();
//     res.declare_variable(a.clone(), src()).ok().unwrap();
//     res.mark_initialized(a.clone(), src()).ok().unwrap();
//     assert_eq!(res.resolve_local(&a.clone(), &src()).ok().unwrap(), Resolution::Local(1));
//     res.end_scope();
//
//     assert_eq!(res.resolve_local(&a.clone(), &src()).ok().unwrap(), Resolution::Local(0));
// }
//
