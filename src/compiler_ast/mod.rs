use crate::ast::types::{BinOp, Expr, ExprTy, Stmt};
use crate::{Chunk, Symbol, Symbolizer};
use crate::func::{Func, FuncType};
use crate::ops::Op;

pub(crate) struct Compiler {
    chunk: Chunk,
}

type CompilerResult = Result<(), String>;

impl Compiler {
    pub fn chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
    fn stmt(&mut self, stmt: &mut Stmt) -> CompilerResult {
        match stmt {
            Stmt::Expr(expr) => self.expr(expr)?,
            Stmt::Block(block) => {
                for stmt in block.iter_mut() {
                    self.stmt(stmt)?;
                }
            }
            Stmt::Print(val) => {
                self.expr(val)?;
                Op::Print.emit(self);
            }
            Stmt::Variable(name, init, resolved, src) => {
                self.expr(init)?;
                let offset = resolved.clone().unwrap().offset;
                println!("Setlocal {} @ {}", name, offset);
                Op::SetLocal(offset as u8).emit(self);
            }
            Stmt::If(_, _, _) => { todo!("") }
            Stmt::While(_, _) => { todo!("") }
            Stmt::Function(_) => { todo!("") }
            Stmt::Return(_) => { todo!("") }
            Stmt::Class(_) => { todo!("") }
        }
        Ok(())
    }
    fn expr(&mut self, expr: &mut ExprTy) -> CompilerResult {
        match &mut expr.expr {
            Expr::Binary(left, op, right) => {
                self.expr(left)?;
                self.expr(right)?;
                self.binop(op);
            }
            Expr::Call(callee, args) => {
                todo!("Calls")
            }
            Expr::Grouping(_) => { todo!("") }
            Expr::Get(_, _) => { todo!("") }
            Expr::Literal(lit) => {
                let idx = self.chunk.add_const(lit.clone());
                Op::Const(idx as u8).emit(self);
            }
            Expr::Unary(_, _) => { todo!("") }
            Expr::Set(_, _, _) => { todo!("") }
            Expr::Variable(var) => {
                let resolved = var.resolved.clone().unwrap();
                Op::GetLocal(resolved.offset as u8).emit(self);
            }
            Expr::Super(_, _) => { todo!("") }
            Expr::Assign(_, _, _) => { todo!("") }
            Expr::Logical(_, _, _) => { todo!("") }
            Expr::This(_) => { todo!("") }
        }
        Ok(())
    }
    fn binop(&mut self, binop: &BinOp) {
        match binop {
            BinOp::EQUAL_EQUAL => Op::EqEq.emit(self),
            BinOp::BANG_EQUAL => Op::NotEq.emit(self),
            BinOp::LESS => Op::LessThan.emit(self),
            BinOp::LESS_EQUAL => Op::LessThanEq.emit(self),
            BinOp::PLUS => Op::Add.emit(self),
            BinOp::SLASH => Op::Div.emit(self),
            BinOp::MULT => Op::Mult.emit(self),
            BinOp::MINUS => Op::Sub.emit(self),
            BinOp::GREATER => {
                Op::LessThanEq.emit(self);
                Op::Not.emit(self)
            }
            BinOp::GREATER_EQUAL => {
                Op::LessThan.emit(self);
                Op::Not.emit(self)
            }
            BinOp::AND => todo!("and / or"),
            BinOp::OR => todo!("and / or"),
        };
    }
}

pub fn compile(stmt: &mut Stmt, mut symbolizer: Symbolizer) -> Func {
    let mut c = Compiler { chunk: Chunk::new() };
    c.stmt(stmt);
    let mut chunk = c.chunk.clone();
    chunk.code.push(Op::Ret);
    let sym = symbolizer.get_symbol(format!("program root"));
    Func::new(sym,
              0, FuncType::Function, chunk, vec![])
}