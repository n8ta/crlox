use std::fmt::{Display, Formatter};
use std::mem::swap;
use crate::ast::types::{Expr, ExprInContext, ExprTy, Stmt};
use crate::printable_error::PrintableError;
use crate::resolver::map_result;
use crate::resolver::resolved_func::ResolvedFunc;
use crate::resolver::uniq_symbol::{UniqSymbol, UniqSymbolizer};
use crate::resolver::var_decl::{VarDecl, VarDeclType};
use crate::resolver::var_ref::VarRef;
use crate::SourceRef;


#[derive(Clone, Debug, PartialEq)]
pub struct VarRefResolved {
    pub symbol: UniqSymbol,
    pub typ: VarRefResolvedType,
}

impl VarRefResolved {
    pub fn new(symbol: UniqSymbol, typ: VarRefResolvedType) -> VarRefResolved {
        VarRefResolved { symbol, typ }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum VarRefResolvedType {
    Upvalue(u8),
    Stack(u8),
    Root,
}

impl Display for VarRefResolved {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.typ {
            VarRefResolvedType::Upvalue(idx) => write!(f, "u{}", idx),
            VarRefResolvedType::Stack(idx) => write!(f, "l{}", idx),
            VarRefResolvedType::Root => write!(f, "root"),
        }
    }
}

struct Updater {
    stack: Vec<(FuncIn, Vec<Vec<VarDecl>>)>,
}

type FuncIn = ResolvedFunc<VarDecl, VarRef>;
type FuncOut = ResolvedFunc<VarRefResolved, VarRefResolved>;
type StmtIn = Stmt<VarDecl, VarRef, FuncIn>;
type StmtOut = Stmt<VarRefResolved, VarRefResolved, FuncOut>;
type ExprTyIn = ExprTy<VarDecl, VarRef, FuncIn>;
type ExprTyOut = ExprTy<VarRefResolved, VarRefResolved, FuncOut>;

pub fn update_upvalues(func: FuncIn) -> Result<FuncOut, PrintableError> {
    let mut up = Updater {
        stack: vec![],
    };
    up.func(func, true)
}

impl Updater {
    pub fn func(&mut self, mut func: FuncIn, is_root: bool) -> Result<FuncOut, PrintableError> {
        if !is_root {
            self.declare(func.name.clone());
        }
        let mut func_body = Box::new(StmtIn::Return(None));

        swap(&mut func.func, &mut func_body);

        self.stack.push((func, vec![vec![]]));
        self.begin_scope();

        if !is_root {
            self.declare(self.stack.last().unwrap().0.name.clone());
        }

        let mut new_args = vec![];
        for var in self.stack.last().unwrap().0.args.clone() {
            new_args.push(self.declare(var.clone()))
        }
        self.begin_scope();
        let new_body = self.stmt(*func_body)?;
        self.end_scope();
        self.end_scope();
        let (func, _scopes) = self.stack.pop().unwrap();

        let func_name: VarRefResolved = if is_root {
            VarRefResolved::new(func.name.sym(), VarRefResolvedType::Root)
        } else {
            self.resolve_decl(func.name)?
        };

        let f = ResolvedFunc::new(func_name, new_args, new_body, func.body_context, func.name_context, func.upvalues);
        Ok(f)
    }
    fn stmt(&mut self, stmt: StmtIn) -> Result<StmtOut, PrintableError> {
        let s = match stmt {
            StmtIn::Expr(expr) => StmtOut::Expr(self.expr(expr)?),
            StmtIn::Block(blk, _size_unset) => {
                self.begin_scope();
                let body = Box::new(map_result(*blk, |stmt| self.stmt(stmt))?);
                let size_set = self.end_scope();
                StmtOut::Block(body, size_set)
            }
            StmtIn::Print(expr) => StmtOut::Print(self.expr(expr)?),
            StmtIn::Variable(decl, value, src) => {
                let var_ref = self.declare(decl.clone());
                StmtOut::Variable(var_ref, self.expr(value)?, src)
            }
            StmtIn::If(test, true_blk, false_blk) => {
                let test = self.expr(test)?;
                self.begin_scope();
                let if_so = Box::new(self.stmt(*true_blk)?);
                self.end_scope();
                self.begin_scope();
                let if_not = if let Some(blk) = false_blk { Some(Box::new(self.stmt(*blk)?)) } else { None };
                self.end_scope();
                StmtOut::If(test, if_so, if_not)
            }
            StmtIn::While(test, body) => {
                let test = self.expr(test)?;
                self.begin_scope();
                let body = Box::new(self.stmt(*body)?);
                self.end_scope();
                StmtOut::While(test, body)
            }
            StmtIn::Function(func) => {
                StmtOut::Function(self.func(func, false)?)
            }
            StmtIn::Return(ret) => {
                StmtOut::Return(if let Some(ret) = ret { Some(self.expr(ret)?) } else { None })
            }
        };
        Ok(s)
    }
    fn expr(&mut self, expr: ExprTyIn) -> Result<ExprTyOut, PrintableError> {
        let ex = match expr.expr {
            Expr::Binary(a, op, b) => Expr::Binary(self.expr(a)?, op, self.expr(b)?),
            Expr::Call(callee, args) =>
                Expr::Call(self.expr(callee)?, map_result(args, |a| self.expr(a))?),
            Expr::Grouping(g) => Expr::Grouping(self.expr(g)?),
            Expr::Get(base, field) => Expr::Get(self.expr(base)?, field),
            Expr::Literal(lit) => Expr::Literal(lit),
            Expr::Unary(op, base) => Expr::Unary(op, self.expr(base)?),
            Expr::Set(base, field, value) => Expr::Set(self.expr(base)?, field, self.expr(value)?),
            Expr::Variable(var) => Expr::Variable(self.resolve(var)?),
            Expr::Super(s) => Expr::Super(s),
            Expr::Assign(var, vaue) => Expr::Assign(self.resolve(var)?, self.expr(vaue)?),
            Expr::Logical(a, op, b) => Expr::Logical(self.expr(a)?, op, self.expr(b)?),
            Expr::This() => Expr::This(),
        };
        Ok(ExprTyOut::new(ExprInContext::new(ex, expr.context)))
    }
    fn begin_scope(&mut self) {
        self.stack.last_mut().unwrap().1.push(vec![]);
    }
    fn end_scope(&mut self) -> usize {
        self.stack.last_mut().unwrap().1.pop().unwrap().len()
    }
    fn declare(&mut self, var: VarDecl) -> VarRefResolved {
        if let VarDeclType::Local = var.typ() {
            self.stack.last_mut().unwrap().1.last_mut().unwrap().push(var.clone());
            let idx = flat(&self.stack.last_mut().unwrap().1).len() -1;
            VarRefResolved::new(var.sym(), VarRefResolvedType::Stack(idx as u8))
        } else {
            let (idx, _up) = self.stack.last_mut().unwrap().0.upvalues.iter().enumerate().find(|(idx, up)| up.sym == var.sym()).unwrap();
            VarRefResolved::new(var.sym(), VarRefResolvedType::Upvalue(idx as u8))
        }
    }
    fn resolve(&self, var: VarRef) -> Result<VarRefResolved, PrintableError> {
        self.resolve_decl(var.decl)
    }
    fn resolve_decl(&self, decl: VarDecl) -> Result<VarRefResolved, PrintableError> {
        let symbol = decl.sym();
        match decl.typ() {
            VarDeclType::Upval => {
                let (idx, _up) = self.stack.last().unwrap().0.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == symbol).expect("This upvalue to exist");
                Ok(VarRefResolved::new(symbol, VarRefResolvedType::Upvalue(idx as u8)))
            }
            VarDeclType::Local => {
                let flattened = flat(&self.stack.last().unwrap().1);
                let (idx, _local) = match flattened.iter().enumerate().find(|(idx, local)| local.sym() == symbol) {
                    None => return Err(PrintableError::new(format!("Unable to find {} on the stack\n{:?}", symbol.symbol, flattened), SourceRef::simple())),
                    Some(i) => i,
                };
                Ok(VarRefResolved::new(symbol, VarRefResolvedType::Stack(idx as u8)))
            }
            VarDeclType::ProgramRoot => panic!("Cannot resolve program root varref"),
        }
    }
}

fn flat(scopes: &Vec<Vec<VarDecl>>) -> Vec<VarDecl> {
    let mut init = vec![];
    for scope in scopes {
        for var in scope {
            init.push(var.clone())
        }
    }
    init
}