use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::resolver::UniqSymbol;
use crate::Symbol;


#[derive(Debug, Clone)]
pub enum VarDeclType {
    Upval,
    Local,
    ProgramRoot // root function technically has a name and var decl
}

#[derive(Debug)]
struct VarDeclInner  {
    symbol: UniqSymbol,
    typ: VarDeclType,
}


#[derive(Clone)]
pub struct VarDecl {
    inner: Rc<RefCell<VarDeclInner>>
}
impl VarDecl {
    pub fn new(sym: UniqSymbol) -> VarDecl {
        VarDecl {
            inner: Rc::new(RefCell::new(VarDeclInner { symbol: sym, typ: VarDeclType::Local}))
        }
    }
    pub fn make_upvalue(&self)  {
        self.inner.borrow_mut().typ = VarDeclType::Upval;
    }
    pub fn sym(&self) -> UniqSymbol {
        self.inner.borrow().symbol.clone()
    }
    pub fn typ(&self) -> VarDeclType {
        self.inner.borrow().typ.clone()
    }
}

impl PartialEq<Symbol> for VarDecl {
    fn eq(&self, other: &Symbol) -> bool {
        &self.inner.borrow().symbol.symbol == other
    }
}
impl PartialEq for VarDecl {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner, &other.inner)
    }
}
impl Display for VarDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let typ = match self.inner.borrow().typ {
            VarDeclType::Upval => "u",
            VarDeclType::Local => "l",
            VarDeclType::ProgramRoot => "r",
        };
        f.write_str(&format!("{}", typ))
    }
}

impl Debug for VarDecl {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let typ = match self.inner.borrow().typ {
            VarDeclType::Upval => "u",
            VarDeclType::Local => "l",
            VarDeclType::ProgramRoot => "r",
        };
        f.write_str(&format!("{}{}:{}", typ, self.inner.borrow().symbol.id, self.inner.borrow().symbol.symbol))
    }
}