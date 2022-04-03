use crate::ast::types::{BinOp, Expr, ExprInContext, ExprResult, ExprTy, LogicalOp, ParserError, Stmt, Tokens, UnaryOp, Variable};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use crate::ast::parser_func::ParserFunc;
use crate::source_ref::{SourceRef};
use crate::Symbol;

pub type ResolverResult = Result<(), ResolverError>;

pub struct ResolverError {
    #[allow(dead_code)]
    message: String,
}

fn flat(scopes: &Vec<HashMap<Symbol, (usize, bool)>>) -> Vec<Symbol> {
    let mut flattened = vec![];
    for scope in scopes {
        for (sym, _) in scope {
            flattened.push(sym.clone())
        }
    }
    flattened
}

pub type ScopeSize = usize;

#[derive(Copy, Clone, Debug)]
enum ClassType {
    None,
    Class,
    SubClass,
}

impl Display for ResolverError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Resolver Error: {}\n", &self.message))
        // f.write_str(&format!("{}", self.source))
    }
}

#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub struct Resolved {
    pub offset: usize,
}

impl Resolved {
    pub fn new(offset: usize) -> Resolved {
        Resolved { offset }
    }
}

impl ResolverError {
    pub fn new(message: String) -> ResolverError { ResolverError { message } }
}

pub fn resolve(prog: &mut Stmt) -> ResolverResult {
    let mut res = Resolver::new();
    res.resolve(prog)
}

#[derive(Debug, Clone)]
struct Resolver {
    scopes: Vec<HashMap<Symbol, (usize, bool)>>,
    current_class: ClassType,
}

impl Resolver {
    pub fn new() -> Resolver { Resolver { scopes: vec![], current_class: ClassType::None } }

    pub fn resolve(&mut self, program: &mut Stmt) -> ResolverResult {
        self.resolve_stmt(program)?;
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new())
    }
    fn end_scope(&mut self) {
        self.scopes.pop();
    }
    fn declare(&mut self, name: &Symbol) {
        // // println!("Declare {} at scope {}", name, self.scopes.len() - 1);
        let last_size = self.last_size();
        if let Some(scope) = self.scopes.last_mut() {
            let size = match scope.get(&name) {
                None => last_size,
                Some((offset, _bool)) => *offset,
            };
            scope.insert(name.clone(), (size, false));
        }
    }
    fn define(&mut self, name: &Symbol) {
        // // println!("Define {} at scope {}", name, self.scopes.len() - 1);
        let last_size = self.last_size();
        if let Some(scope) = self.scopes.last_mut() {
            let (size, bool) = match scope.get(&name) {
                None => (last_size, true),
                Some((offset, _)) => (*offset, true),
            };
            scope.insert(name.clone(), (size, bool));
        }
    }

    fn last_size(&self) -> usize {
        self.scopes.last().unwrap().len()
    }

    fn resolve_local(&mut self, name: &Symbol, resolved: &mut Option<Resolved>) -> ResolverResult {
        if self.scopes.len() == 0 {
            return Ok(());
        }
        let flat = flat(&self.scopes);

        if let Some((idx, _local)) = flat.iter().enumerate().rev().find(|(_, local)| *local == name) {
            let res = Resolved { offset: idx };
            println!("Resolved {} to {}", name, idx);
            println!("{:?}", flat);
            resolved.insert(res);
        }
        if let None = resolved {
            // if name.string != "clock" {
            return Err(ResolverError::new(format!("Variable {} was never defined", name)));
            // }
        }
        Ok(())
    }
    fn resolve_stmt(&mut self, stmt: &mut Stmt) -> ResolverResult {
        match stmt {
            Stmt::Expr(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts.iter_mut() {
                    self.resolve(stmt)?;
                }
                self.end_scope();
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Variable(name, init, resolved, _context) => {
                self.declare(name);
                self.resolve_expr(init)?;
                self.define(name);
                self.resolve_local(name, resolved)?;
            }
            Stmt::If(test, if_branch, else_branch) => {
                self.resolve_expr(test)?;
                self.resolve(if_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve(else_branch)?;
                }
            }
            Stmt::While(test, body) => {
                self.resolve_expr(test)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Function(func) => {
                self.declare(func.name());
                self.define(func.name());
                // self.resolve_local(func.name(), resolved)?;
                self.resolve_func(func)?;
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr {
                    self.resolve_expr(expr)?;
                }
            }
            Stmt::Class(class) => {
                todo!("Classes!")
            }
        }
        Ok(())
    }
    fn resolve_func(&mut self, func: &mut ParserFunc) -> ResolverResult {
        self.begin_scope();
        for name in func.inner.args.iter() {
            self.declare(&name);
            self.define(&name);
        }
        let mut body_mut = func.inner.body.borrow_mut();
        self.resolve(&mut body_mut)?;

        self.end_scope();
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &mut ExprTy) -> ResolverResult {
        match &mut expr.expr {
            Expr::Super(_method, resolved) => {
                todo!("Super")
            }
            Expr::This(resolved) => {
                todo!("Super")
            }
            Expr::Binary(left, _op, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call(callee, args) => {
                self.resolve_expr(callee)?;
                for expr in args {
                    self.resolve_expr(expr)?;
                }
            }
            Expr::Grouping(group) => {
                self.resolve_expr(group)?;
            }
            Expr::Literal(_) => {}
            Expr::Unary(_op, expr) => {
                self.resolve_expr(expr)?;
            }
            Expr::Variable(var) => {
                if let Some(scope) = self.scopes.last() {
                    if let Some((_offset, defined)) = scope.get(&var.name) {
                        if !defined {
                            return Err(ResolverError::new("Can't read local variable in its own initializer.".to_string()));
                        }
                    }
                }
                self.resolve_local(&var.name, &mut var.resolved)?;
            }
            Expr::Assign(name, value, resolved) => {
                self.resolve_expr(value)?;
                self.resolve_local(name, resolved)?;
            }
            Expr::Logical(left, _op, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Get(expr, _getter_name) => {
                self.resolve_expr(expr)?;
            }
            Expr::Set(left, _field, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
        }
        Ok(())
    }
}

// #[test]
// fn resolver_basic() {
//     use std::rc::Rc;
//     use crate::scanner::scanner;
//     use crate::parser::parse;
//     // var name = true;
//     // print name;
//     let src = "fun test() {\n
//         var x = 1;\n\
//         print x;
//     }\n".to_string();
//     let src = Rc::new(Source::new(src));
//     let mut stmts: Vec<Stmt> = parse(scanner(src.clone()).unwrap(), src.clone()).unwrap();
//     match resolve(&mut stmts) {
//         Err(_) => panic!("failed!"),
//         _ => {}
//     }
//
//     if let Stmt::Function(func) = &stmts[0] {
//         if let Stmt::Print(val) = &func.body[1] {
// // //             println!("Checking {:?}", val);
//             assert!(val.scope.is_some(), "resolver should assign a scope to a stack variable");
//         } else {
//             assert!(false)
//         }
//     } else {
//         assert!(false)
//     }
// }
//
// #[test]
// fn resolver_recursive() {
//     use std::rc::Rc;
//     use crate::scanner::scanner;
//     use crate::parser::parse;
//     let src = "fun rec() {\nprint rec();\n}".to_string();
//     let src = Rc::new(Source::new(src));
//     let mut stmts: Vec<Stmt> = parse(scanner(src.clone()).unwrap(), src.clone()).unwrap();
//     if let Stmt::Function(func) = &stmts[0] {
//         if let Stmt::Print(expr) = &func.body[0] {
//             if let Expr::Call(callee, args) = &expr.expr {
//                 assert!(callee.scope.is_none(), "None because it should be in the global scope");
//             } else {
//                 assert!(false, "should be a call inside print");
//             }
//         } else {
//             assert!(false, "should be a print stmt");
//         }
//     } else {
//         assert!(false, "should be a function")
//     }
// }