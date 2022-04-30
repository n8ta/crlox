pub mod uniq_symbol;
pub(crate) mod var_ref;
pub(crate) mod var_decl;
mod resolved_func;

use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter, UpperExp};
use std::rc::Rc;
use crate::ast::types::{Expr, ExprInContext, ExprTy, Stmt};
use crate::{Source, SourceRef, Symbol, Symbolizer};
use crate::ast::parser::Parser;
use crate::ast::parser_func::{ParserFunc, ParserFuncInner};
use crate::printable_error::PrintableError;
use crate::uniq_pass::uniq_symbol::{UniqSymbol, UniqSymbolizer};
use crate::uniq_pass::var_decl::VarDecl;
use crate::uniq_pass::var_ref::VarRef;

#[derive(Clone, Debug)]
pub struct Upvalue {
    sym: UniqSymbol,
    typ: UpvalueType,
}

impl Upvalue {
    pub fn new(sym: UniqSymbol, typ: UpvalueType) -> Self { Upvalue { sym, typ } }
}

#[derive(Clone, Debug)]
pub enum UpvalueType {
    Root,
    Captured(u8),
}

#[derive(Clone)]
pub struct FuncScope {
    upvalues: Vec<Upvalue>,
    scopes: Vec<Vec<VarDecl>>,
}

impl Debug for FuncScope {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Func - {:?}\n", self.upvalues))?;
        f.write_str(&format!("    scopes: {:?}\n", self.scopes))
    }
}

impl Debug for PartialResolver {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for i in &self.stack {
            f.write_str(&format!("{:?}", i))?;
        }
        Ok(())
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
            self.upvalues.push(Upvalue::new(var, UpvalueType::Root));
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

struct PartialResolver {
    uniq: UniqSymbolizer,
    // Stack of Functions
    //  Stack of Scopes
    //   List of Variables
    stack: Vec<FuncScope>,
}

// Run proc on every input and return the vec of the results or the first
// PrintableError.
pub fn map_result<InputT, OutputT, ErrT, F: FnMut(InputT) -> Result<OutputT, ErrT>>(items: Vec<InputT>, mut proc: F) -> Result<Vec<OutputT>, ErrT> {
    let mut passing = vec![];
    for item in items {
        passing.push(proc(item)?);
    }
    Ok(passing)
}

type ParserFuncIn = ParserFunc<Symbol, Symbol>;
type ParserFuncOut = ParserFunc<VarDecl, VarRef>;
type StmtIn = Stmt<Symbol, Symbol, ParserFuncIn>;
type StmtOut = Stmt<VarDecl, VarRef, ParserFuncOut>;
type ExprTyIn = ExprTy<Symbol, Symbol, ParserFuncIn>;
type ExprTyOut = ExprTy<VarDecl, VarRef, ParserFuncOut>;

impl PartialResolver {
    pub fn new(symbolizer: Symbolizer) -> PartialResolver { PartialResolver { uniq: UniqSymbolizer::new(symbolizer), stack: vec![FuncScope::new()] } }
    pub fn stmt(&mut self, stmt: StmtIn) -> Result<StmtOut, PrintableError> {
        let s: StmtOut = match stmt {
            Stmt::Expr(ex) => Stmt::Expr(self.expr(ex)?),
            Stmt::Block(stmts) => {
                self.begin_scope();
                let s = Stmt::Block(Box::new(map_result(*stmts, |r| self.stmt(r))?));
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
                self.end_func();
                Stmt::Function(ParserFunc::new(name, new_args, body, f.name_context.clone(), f.context.clone()))
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
        // let mut outermost_func = true;
        println!("Resolving {}", symbol);
        println!("{:?}", self);


        let mut result = None;
        let len = self.stack.len();
        for (func_idx, func) in self.stack.iter_mut().enumerate().rev() {
            let len_scopes = func.scopes.len();
            for (scope_idx, scope) in func.scopes.iter_mut().enumerate().rev() {
                for var in scope.iter_mut().rev() {
                    if var == &symbol {
                        println!("Found in func {}/{} scope {}/{}", func_idx + 1, len, scope_idx + 1, len_scopes);
                        // If this variable wasn't defined in the current function we
                        // are closing over it. Mark is it an upvalue root.
                        // if !outermost_func {
                        //     var.make_upvalue();
                        //     func.root_upvalues.push(var.clone());
                        // }
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
                println!("Making upvalue {:?}", &decl);
                decl.make_upvalue();
                let vr = VarRef::new(&decl);
                let mut capture_idx = self.stack[func_idx].add_root(decl.sym());
                for func in &mut self.stack[func_idx + 1..] {
                    func.capture_upvalue(capture_idx, decl.sym());
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
    fn end_func(&mut self) {
        self.stack.pop();
    }
    fn begin_scope(&mut self) {
        self.stack.last_mut().unwrap().scopes.push(vec![]);
    }
    fn end_scope(&mut self) {
        self.stack.last_mut().unwrap().scopes.pop();
    }
    fn declare(&mut self, symbol: Symbol) -> VarDecl {
        let sym = self.uniq.gen(symbol);
        let vd = VarDecl::new(sym.clone());
        self.stack.last_mut().unwrap().scopes.last_mut().unwrap().push(vd.clone());
        vd
    }
}


/// After this pass all refernece to a single variable are given unique ids.
/// This means code like
/// var x = 0;
/// {
///  var x = 1;
/// }
/// x += 1;
/// {
///  var = 2 ;
/// }
/// Will be transformed into
/// var "0-x" = 0;
/// {
///  var "1-x" = 1;
/// }
/// "0-x" += 1;
/// {
///  var = "2-x" ;
/// }
/// So future passes can easily compare by the number. This makes implementing closures
/// much easier.
pub fn uniq(ast: StmtIn, symbolizer: Symbolizer) -> Result<StmtOut, PrintableError> {
    PartialResolver::new(symbolizer).stmt(ast)
}

#[test]
fn test_plain_text() {
    use std::rc::Rc;

    fn remove_spaces_eq(expected: &str, actual: &str) {
        let exp: String = expected.chars().filter(|c| *c != ' ' && *c != '\n').collect();
        let act: String = actual.chars().filter(|c| *c != ' ' && *c != '\n').collect();
        if exp != act {
            eprintln!("Expected: \n{}\n\nActual: \n{}", expected, actual);
        }
        assert_eq!(act, exp);
    }

    fn pass(input: &str, exp: &str) {
        println!("{}", input);
        let symbolizer = Symbolizer::new();
        let source = Rc::new(Source::new(input.to_string()));
        let tokens = crate::ast::scanner::scanner(source.clone(), symbolizer.clone()).unwrap();
        let ast = crate::ast::parser::parse(tokens, source).unwrap();
        let uniq_ast = crate::uniq_pass::uniq(ast, symbolizer).unwrap();
        let uniq_ast_str = format!("{}", uniq_ast);
        let uniq_ast_str_rm_outer_scope: Vec<char> = uniq_ast_str.chars().collect();
        let uniq_cln: String = uniq_ast_str_rm_outer_scope[1..uniq_ast_str_rm_outer_scope.len() - 3].iter().collect::<String>();

        remove_spaces_eq(exp, &uniq_cln);
    }

    pass("var x = 0; print x;", "var l0 = 0; print rl0;");
    pass("var x = 0; {{{ print x; }}}", "var l0 = 0; {{{print rl0;}}}");
    pass("var x = 0; { var x = 1; {{ print x; }}}", "var l0 = 0; { var l1 = 1;  {{ print rl1; }} }");
    pass("var x = 0; { var x = 1; } print x;", "var l0 = 0; { var l1 = 1; } print rl0;");
    pass("var x = 0; { var x = 1; var z = 3; } print x;", "var l0 = 0; { var l1 = 1; var l2 = 3; } print rl0;");
    pass("var x = 0; { var x = 1; var z = 3; print x; print z; print x; } print x;",
         "var l0 = 0; { var l1 = 1; var l2 = 3; print rl1; print rl2; print rl1; } print rl0;");
    pass("var x = 0; var y = x + x;", "var l0 = 0; var l1 = rl0 + rl0;");
    pass("var x = 0; var y = x + x; print y;",
         "var l0 = 0; var l1 = rl0 + rl0; print rl1;");
    pass("{ var x = 123; var y = x; var z = y + x; }",
         "{ var l0 = 123;\n var l1 = rl0;\n var l2 = rl1 + rl0; }");
    pass("{ var x = 123; var y = x; var z = ((y)) + (x+2); }",
         "{ var l0 = 123; var l1 = rl0; var l2 = ((rl1)) + (rl0+2); }");
    pass("fun test() { print 1; }", "fun l0() { print 1; }");
    pass("fun test() { print test; }", "fun l0() { print rl0; }");
    pass("fun test() { var test = 0; print test; }",
         "fun l0() { var l1 = 0; print rl1; }");
    pass("fun test() { var x = 123; fun testinner() { return x; } }",
         "fun l0() { var u1 = 123; fun l2() { return ru1;  } }");
    pass("var aa = 11; fun test() { var x = 123; fun testinner() { return x + aa; } }",
         "var u0 = 11; fun l1() { var u2 = 123; fun l3() { return ru2 + ru0;  } }");
    pass("fun ii(a, b, c) { print c + b + a + ii; }", "fun l0(l1, l2, l3) { print rl3 + rl2 + rl1 +rl0; }");
    pass("fun ii(a, b, c) { {{{ print c + b + a; }}} }", "fun l0(l1, l2, l3) { {{{ print rl3 + rl2 + rl1; }}} }");
    pass("fun ii(a, b, c) { var a = 0; print c + b + a; }", "fun l0(l1, l2, l3) { var l4 = 0; print rl3 + rl2 + rl4; }");
    pass("fun makeClosure() { var local = \"local\"; fun closure() { print local; } return closure; } var closure = makeClosure(); closure();",
         "fun l0() { var u1 = local; fun l2() { print ru1; } return rl2; } var l3 = rl0(); rl3();");
}