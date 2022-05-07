mod chunk;

pub use chunk::{Chunk, Write};

use crate::ast::{BinOp, Expr, ExprTy, LogicalOp, Stmt, UnaryOp};
use crate::runtime::{Func, FuncType};
use crate::ops::Op;
use crate::resolver::{ResolvedFunc, VarRefResolved, VarRefResolvedType};
use crate::lexer::SourceRef;
use crate::runtime::Value;

struct SubCompiler {
    chunk: Chunk,
    func: ResolvedFunc<VarRefResolved, VarRefResolved>,
}

type StmtT = Stmt<VarRefResolved, VarRefResolved, ResolvedFunc<VarRefResolved, VarRefResolved>>;
type ExprTyT = ExprTy<VarRefResolved, VarRefResolved, ResolvedFunc<VarRefResolved, VarRefResolved>>;

pub(crate) struct Compiler {
    stack: Vec<SubCompiler>,
}

type CompilerResult = Result<(), String>;

impl Compiler {
    pub fn chunk(&mut self) -> &mut Chunk {
        &mut self.stack.last_mut().unwrap().chunk
    }
    pub fn func(&mut self, func: &ResolvedFunc<VarRefResolved, VarRefResolved>, is_root: bool) -> Result<Func, String> {
        self.stack.push(SubCompiler { func: func.clone(), chunk: Chunk::new() });
        let inner = func.func.clone();
        self.stmt(&inner)?;
        let mut sub = self.stack.pop().unwrap();
        sub.chunk.add(Op::Nil, SourceRef::simple());
        sub.chunk.add(Op::Ret, SourceRef::simple());
        let f = Func::new(func.name.clone(), func.args.len() as u8, FuncType::Function, sub.chunk, func.upvalues.clone());
        if !is_root {
            let idx = self.chunk().add_const(Value::Func(f.clone()));
            Op::Closure(idx).emit(self);
        }
        Ok(f)
    }
    fn stmt(&mut self, stmt: &StmtT) -> CompilerResult {
        match stmt {
            Stmt::Expr(expr) => {
                self.expr(expr)?;
                Op::Pop.emit(self);
            },
            Stmt::Block(block, scope_size) => {
                for stmt in block.iter() {
                    self.stmt(stmt)?;
                }
                for _ in 0..*scope_size {
                    Op::Pop.emit(self);
                }
            }
            Stmt::Print(val) => {
                self.expr(val)?;
                Op::Print.emit(self);
            }
            Stmt::Variable(name, init, _src) => {
                match name.typ {
                    VarRefResolvedType::Upvalue(idx) => {
                        self.expr(init)?;
                        Op::SetUpvalue(idx).emit(self);
                    }
                    VarRefResolvedType::Stack(_idx) => {
                        self.expr(init)?;
                    }
                    VarRefResolvedType::Root => panic!("program root!")
                }

            }
            Stmt::If(test, body, else_body) => {
                /*
                                Expression
                                FalseJump :IfNotJump
                                Stmt: if-body
                                :IfNotJump
                                rest
                                Expression
                                FalseJump :IfNotJump
                                Stmt: if-body
                                Jump :IfDoneJump
                                :IfNotJump
                                Stmt: else-body
                                :IfDoneJump
                */
                if let Some(else_stmt) = else_body {
                    self.expr(test)?;
                    let false_jump = Op::RelJumpIfFalse(1337);
                    let false_jump_write = false_jump.emit(self);
                    self.stmt(body)?;
                    let done_jump = Op::RelJump(1337);
                    let done_jump_write = done_jump.emit(self);
                    false_jump.overwrite(self.chunk(), &false_jump_write);
                    self.stmt(else_stmt)?;
                    done_jump.overwrite(self.chunk(), &done_jump_write);
                } else {
                    self.expr(test)?;
                    let write = Op::RelJumpIfFalse(1337).emit(self);
                    self.stmt(body)?;
                    Op::RelJumpIfFalse(1337).overwrite(self.chunk(), &write);
                }
            }
            Stmt::While(test, body) => {
                /*
                :Test
                [Test]
                JumpIfFalse :Done
                Pop
                [Body]
                Jump :Test
                :Done
                Pop
                 */
                let loop_start = self.chunk().len() as i16;
                self.expr(test)?;
                let exit_jump = Op::RelJumpIfFalse(1337); // jump to :done if expression is false
                let exit_jump_write = exit_jump.emit(self);
                Op::Pop.emit(self); // pop while loop expression
                self.stmt(body)?;
                let jump_to_start = Op::RelJump(-((self.chunk().len() as i16) - loop_start));
                jump_to_start.emit(self); // jump to :start
                exit_jump.overwrite(&mut self.chunk(), &exit_jump_write); // :done
                Op::Pop.emit(self); // pop while loop expression
            }
            Stmt::Function(func) => {
                self.func(func, false)?;
            }
            Stmt::Return(ret) => {
                if let Some(expr) = ret {
                    self.expr(expr)?;
                } else {
                    Op::Nil.emit(self);
                }
                Op::Ret.emit(self);
            }
            // Stmt::Class(_) => { todo!("") }
        }
        Ok(())
    }
    fn expr(&mut self, expr: &ExprTyT) -> CompilerResult {
        match &expr.expr {
            Expr::Binary(left, op, right) => {
                self.expr(left)?;
                self.expr(right)?;
                self.binop(op);
            }
            Expr::Call(callee, args) => {
                self.expr(callee)?;
                for arg in args.iter() {
                    self.expr(arg)?;
                }
                Op::Call(args.len() as u8).emit(self);
            }
            Expr::Grouping(group) => {
                self.expr(group)?;
            }
            Expr::Get(object, field) => {
                self.expr(object)?;
                let idx = self.chunk().add_const(Value::String(field.clone()));
                Op::GetProperty(idx);
            }
            Expr::Literal(lit) => {
                let idx = self.chunk().add_const(lit.clone());
                Op::Const(idx as u8).emit(self);
            }
            Expr::Unary(op, expr) => {
                self.expr(expr)?;
                match op {
                    UnaryOp::MINUS => Op::Negate.emit(self),
                    UnaryOp::BANG => Op::Not.emit(self),
                };
            }
            Expr::Set(inst, field, value) => {
                self.expr(inst)?;
                let idx = self.chunk().add_const(Value::String(field.clone()));
                self.expr(value)?;
                Op::SetProperty(idx);
            }
            Expr::Variable(var) => {
                match var.typ {
                    VarRefResolvedType::Upvalue(up_idx) => Op::GetUpvalue(up_idx).emit(self),
                    VarRefResolvedType::Stack(local_idx) => Op::GetLocal(local_idx).emit(self),
                    VarRefResolvedType::Root => panic!("program root!"),
                };
                // let resolved = var.resolved.clone().unwrap();
                // Op::GetLocal(resolved.offset as u8).emit(self);
            }
            Expr::Super(_, ) => todo!(""),
            Expr::Assign(a, rhs) => {
                self.expr(rhs)?;
                match a.typ {
                    VarRefResolvedType::Upvalue(idx) => Op::SetUpvalue(idx).emit(self),
                    VarRefResolvedType::Stack(idx) => Op::SetLocal(idx).emit(self),
                    VarRefResolvedType::Root => panic!("program root!")
                };
            }
            Expr::Logical(lhs, op, rhs) => {
                match op {
                    LogicalOp::AND => {
                        /*
                        [LHS]
                        JumpIfFalse :End
                        Pop
                        [RHS]
                        :End
                         */
                        self.expr(lhs)?;
                        let false_jump = Op::RelJumpIfFalse(1337);
                        let false_jump_write = false_jump.emit(self);
                        Op::Pop.emit(self);
                        self.expr(rhs)?;
                        false_jump.overwrite(self.chunk(), &false_jump_write);
                    }
                    LogicalOp::OR => {
                        /*
                        [LHS]
                        JumpIfTrue :End
                        Pop
                        [RHS]
                        :End
                         */
                        self.expr(lhs)?;
                        let true_jump = Op::RelJumpIfTrue(1337);
                        let true_jump_write = true_jump.emit(self);
                        Op::Pop.emit(self);
                        self.expr(rhs)?;
                        true_jump.overwrite(self.chunk(), &true_jump_write);
                    }
                }
            }
            Expr::This() => { todo!("") }
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

pub fn compile(func: ResolvedFunc<VarRefResolved, VarRefResolved>) -> Func {
    let mut c = Compiler { stack: vec![] };
    c.func(&func, true).unwrap()
}