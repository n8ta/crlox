use std::fmt::{Debug, Formatter};
use crate::ast::{Expr, ExprInContext, ExprTy, ParserFunc, Stmt};
use crate::resolver::{FuncScope, ResolvedFunc, UniqSymbolizer, VarDecl, VarRef};
use crate::lexer::{Symbol};
use crate::printable_error::PrintableError;
use crate::SourceRef;
use crate::util::map_result;

impl<'a> Debug for PartialResolver<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in &self.stack {
            f.write_str(&format!("{:?}", i))?;
        }
        Ok(())
    }
}


pub struct PartialResolver<'a> {
    uniq: &'a mut UniqSymbolizer,
    // Stack of Functions
    //  Stack of Scopes
    //   List of Variables
    stack: Vec<FuncScope>,
}

type FuncIn = ParserFunc<Symbol, Symbol>;
type FuncOut = ResolvedFunc<VarDecl, VarRef>;
pub type StmtIn = Stmt<Symbol, Symbol, FuncIn>;
pub type StmtOut = Stmt<VarDecl, VarRef, FuncOut>;
type ExprTyIn = ExprTy<Symbol, Symbol, FuncIn>;
type ExprTyOut = ExprTy<VarDecl, VarRef, FuncOut>;

impl<'a> PartialResolver<'a> {
    pub fn new(uniq: &'a mut UniqSymbolizer) -> PartialResolver { PartialResolver { uniq, stack: vec![FuncScope::new()] } }
    pub fn stmt(&mut self, stmt: StmtIn) -> Result<StmtOut, PrintableError> {
        let s: StmtOut = match stmt {
            Stmt::Expr(ex) => Stmt::Expr(self.expr(ex)?),
            Stmt::Block(stmts, _size) => {
                self.begin_scope();
                let s = Stmt::Block(Box::new(map_result(*stmts, |r| self.stmt(r))?), _size);
                self.end_scope();
                s
            }
            Stmt::Print(print) => Stmt::Print(self.expr(print)?),
            Stmt::Variable(name, init, src) => Stmt::Variable(self.declare(name), self.expr(init)?, src),
            Stmt::If(test, a, b) => {
                let test = self.expr(test)?;
                self.begin_scope();
                let aa = self.stmt(*a)?;
                self.end_scope();
                let bb: Option<Box<StmtOut>> = if let Some(b) = b {
                    self.begin_scope();
                    let bb = self.stmt(*b)?;
                    self.end_scope();
                    Some(Box::new(bb))
                } else {
                    None
                };
                Stmt::If(test, Box::new(aa), bb)
            }
            Stmt::While(test, body) => {
                let test = self.expr(test)?;
                self.begin_scope();
                let body = self.stmt(*body)?;
                self.end_scope();
                Stmt::While(test, Box::new(body))
            }
            Stmt::Function(f) => {
                let name = self.declare(f.name.clone());
                self.begin_func();
                let new_args = f.args.iter().map(|a| self.declare(a.clone())).collect();

                // a recursive function is really defined in two scopes so define it again inside itself here
                // with the same uniq value...
                self.stack.last_mut().unwrap().scopes.last_mut().unwrap().push(name.clone());

                self.begin_scope();
                let body = self.stmt(*f.body.clone())?; // todo: this could be pretty slow
                self.end_scope();
                let func_scope = self.end_func();
                let rfunc = ResolvedFunc::new(name, new_args, body, f.name_context.clone(), f.context.clone(), func_scope.upvalues);
                Stmt::Function(rfunc)
            }
            Stmt::Return(val) => {
                match val {
                    None => Stmt::Return(None),
                    Some(v) => Stmt::Return(Some(self.expr(v)?))
                }
            }
        };
        Ok(s)
    }
    fn expr(&mut self, expr: ExprTyIn) -> Result<ExprTyOut, PrintableError> {
        let ex = match expr.expr {
            Expr::Binary(a, op, b) => Expr::Binary(self.expr(a)?, op, self.expr(b)?),
            Expr::Call(callee, args) => {
                let callee = self.expr(callee)?;
                let args = map_result(args, |arg| self.expr(arg))?;
                Expr::Call(callee, args)
            }
            Expr::Grouping(g) => Expr::Grouping(self.expr(g)?),
            Expr::Get(a, b) => Expr::Get(self.expr(a)?, b),
            Expr::Literal(lit) => Expr::Literal(lit),
            Expr::Unary(op, val) => Expr::Unary(op, self.expr(val)?),
            Expr::Set(base, field, value) => Expr::Set(self.expr(base)?, field, self.expr(value)?),
            Expr::Variable(v) => Expr::Variable(self.resolve(v, &expr.context)?),
            Expr::Super(_) => todo!("super"),
            Expr::Assign(var, value) => Expr::Assign(self.resolve(var, &expr.context)?, self.expr(value)?),
            Expr::Logical(a, op, b) => Expr::Logical(self.expr(a)?, op, self.expr(b)?),
            Expr::This() => todo!("this"),
        };
        Ok(Box::new(ExprInContext::new(ex, expr.context)))
    }
    fn resolve(&mut self, symbol: Symbol, src: &SourceRef) -> Result<VarRef, PrintableError> {
        let mut result = None;
        for (func_idx, func) in self.stack.iter_mut().enumerate().rev() {
            for scope in func.scopes.iter_mut().rev() {
                for var in scope.iter_mut().rev() {
                    if var == &symbol {
                        result = Some((func_idx, var.clone()));
                        break;
                    }
                }
                if result.is_some() { break; }
            }
            if result.is_some() { break; }
        }
        if let Some((func_idx, decl)) = result {
            if func_idx == self.stack.len() - 1 {
                Ok(VarRef::new(&decl))
            } else {
                decl.make_upvalue();
                let vr = VarRef::new(&decl);
                let mut capture_idx = self.stack[func_idx].add_root(decl.sym());
                for func in &mut self.stack[func_idx + 1..] {
                    capture_idx = func.capture_upvalue(capture_idx, decl.sym());
                }
                Ok(vr)
            }
        } else {
            Err(PrintableError::new(format!("Unable to resolve symbol: {}", symbol), src.clone()))
        }
    }
    fn begin_func(&mut self) {
        self.stack.push(FuncScope::new());
    }
    pub fn end_func(&mut self) -> FuncScope {
        self.stack.pop().unwrap()
    }
    fn begin_scope(&mut self) {
        self.stack.last_mut().unwrap().scopes.push(vec![]);
    }
    fn end_scope(&mut self) {
        self.stack.last_mut().unwrap().scopes.pop().unwrap();
    }
    fn declare(&mut self, symbol: Symbol) -> VarDecl {
        let sym = self.uniq.gen(symbol);
        let vd = VarDecl::new(sym.clone());
        self.stack.last_mut().unwrap().scopes.last_mut().unwrap().push(vd.clone());
        vd
    }
}