use std::fmt::{Display, Formatter};
use std::mem::swap;
use crate::ast::types::{Expr, ExprInContext, ExprTy, Stmt};
use crate::printable_error::PrintableError;
use crate::resolver::map_result;
use crate::resolver::resolved_func::ResolvedFunc;
use crate::resolver::uniq_symbol::{UniqSymbol, UniqSymbolizer};
use crate::resolver::upvalue_update::VarRefResolved::Upvalue;
use crate::resolver::var_decl::{VarDecl, VarDeclType};
use crate::resolver::var_ref::VarRef;
use crate::SourceRef;

#[derive(Clone, Debug, PartialEq)]
pub enum VarRefResolved {
    Upvalue(u8),
    Stack(u8),
}

impl Display for VarRefResolved {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VarRefResolved::Upvalue(idx) => write!(f, "u{}", idx),
            VarRefResolved::Stack(idx) => write!(f, "l{}", idx),
        }
    }
}

struct Updater {
    stack: Vec<(ResolvedFunc<VarRef>, Vec<Vec<VarDecl>>)>,
}

type FuncIn = ResolvedFunc<VarRef>;
type FuncOut = ResolvedFunc<VarRefResolved>;
type StmtIn = Stmt<VarDecl, VarRef, FuncIn>;
type StmtOut = Stmt<VarDecl, VarRefResolved, FuncOut>;
type ExprTyIn = ExprTy<VarDecl, VarRef, FuncIn>;
type ExprTyOut = ExprTy<VarDecl, VarRefResolved, FuncOut>;

pub fn update_upvalues(func: ResolvedFunc<VarRef>) -> Result<ResolvedFunc<VarRefResolved>, PrintableError> {
    let mut up = Updater {
        stack: vec![],
    };
    up.func(func, true)
}

impl Updater {
    pub fn func(&mut self, mut func: ResolvedFunc<VarRef>, is_root: bool) -> Result<ResolvedFunc<VarRefResolved>, PrintableError> {

        if !is_root {
            self.declare(func.name.clone());
        }
        let mut func_body = Box::new(Stmt::Return(None));
        swap(&mut func.func, &mut func_body);

        self.stack.push((func, vec![vec![]]));
        self.begin_scope();

        if !is_root {
            self.declare(self.stack.last().unwrap().0.name.clone());
        }

        for var in self.stack.last().unwrap().0.args.clone() {
            self.declare(var.clone())
        }
        self.begin_scope();
        let new_body = self.stmt(*func_body)?;
        self.end_scope();
        self.end_scope();
        let (func, _scopes) = self.stack.pop().unwrap();

        let f = ResolvedFunc::new(func.name, func.args, new_body, func.body_context, func.name_context, func.upvalues);
        Ok(f)
    }
    fn stmt(&mut self, stmt: StmtIn) -> Result<StmtOut, PrintableError> {
        let s = match stmt {
            StmtIn::Expr(expr) => StmtOut::Expr(self.expr(expr)?),
            StmtIn::Block(blk) => {
                self.begin_scope();
                let body = Box::new(map_result(*blk, |stmt| self.stmt(stmt))?);
                self.end_scope();
                StmtOut::Block(body)
            }
            StmtIn::Print(expr) => StmtOut::Print(self.expr(expr)?),
            StmtIn::Variable(decl, value, src) => {
                self.declare(decl.clone());
                StmtOut::Variable(decl, self.expr(value)?, src)
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
    fn end_scope(&mut self) {
        self.stack.last_mut().unwrap().1.pop();
    }
    fn declare(&mut self, var: VarDecl) {
        self.stack.last_mut().unwrap().1.last_mut().unwrap().push(var);
    }
    fn resolve(&self, var: VarRef) -> Result<VarRefResolved, PrintableError> {
        let symbol = var.decl.sym();
        match var.decl.typ() {
            VarDeclType::Upval => {
                let (idx, _up) = self.stack.last().unwrap().0.upvalues.iter().enumerate().find(|(_idx, up)| up.sym == symbol).expect("This upvalue to exist");
                Ok(VarRefResolved::Upvalue(idx as u8))
            }
            VarDeclType::Local => {
                let flattened = flat(&self.stack.last().unwrap().1);
                let (idx, _local) = match flattened.iter().enumerate().find(|(idx, local)| local.sym() == symbol) {
                    None => return Err(PrintableError::new(format!("Unable to find {} on the stack\n{:?}", symbol.symbol, flattened), SourceRef::simple())),
                    Some(i) => i,
                };
                Ok(VarRefResolved::Stack(idx as u8))
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