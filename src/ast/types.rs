use std::fmt::{Display, Formatter};
use std::ptr::write;
use crate::scanner::{Token, TType};
use crate::{SourceRef, Symbol};
use crate::ast::parser_func::ParserFunc;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ParserError {
    pub msg: String,
    pub context: SourceRef,
}

impl ParserError {
    pub fn new(msg: String, context: SourceRef) -> ParserError { ParserError { msg, context } }
}

pub type Tokens = Vec<Token>;

pub type ExprTy<DeclT, RefT, FuncT> = Box<ExprInContext<DeclT, RefT, FuncT>>;

#[allow(unused)]
#[derive(Clone, Debug)]
pub struct ExprInContext<
    DeclT: PartialEq + Clone + Display,
    RefT: PartialEq + Clone + Display,
    FuncT: PartialEq + Clone + Display> {
    pub context: SourceRef,
    pub expr: Expr<DeclT, RefT, FuncT>,
    _marker: std::marker::PhantomData<(DeclT, RefT, FuncT)>,
}

impl<T: PartialEq + Clone + Display, S: Clone + PartialEq + Display, F: PartialEq + Clone + Display> Display for ExprInContext<T, S, F> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.expr))
    }
}

impl<T: PartialEq + Clone + Display, S: Clone + PartialEq + Display, F: Clone + PartialEq + Display> PartialEq for ExprInContext<T, S, F> {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl<T: Clone + PartialEq + Display,
    S: Clone + PartialEq + Display,
    F: PartialEq + Clone + Display
> ExprInContext<T, S, F> {
    pub fn new(expr: Expr<T, S, F>, context: SourceRef) -> ExprInContext<T, S, F> {
        ExprInContext { expr, context, _marker: std::marker::PhantomData }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt<
    VarDeclT: Clone + PartialEq + Display,
    VarRefT: Clone + PartialEq + Display,
    FuncT: PartialEq + Clone + Display> {
    Expr(ExprTy<VarDeclT, VarRefT, FuncT>),
    Block(Box<Vec<Stmt<VarDeclT, VarRefT, FuncT>>>, usize),
    Print(ExprTy<VarDeclT, VarRefT, FuncT>),
    Variable(VarDeclT, ExprTy<VarDeclT, VarRefT, FuncT>, SourceRef),
    If(ExprTy<VarDeclT, VarRefT, FuncT>, Box<Stmt<VarDeclT, VarRefT, FuncT>>, Option<Box<Stmt<VarDeclT, VarRefT, FuncT>>>),
    While(ExprTy<VarDeclT, VarRefT, FuncT>, Box<Stmt<VarDeclT, VarRefT, FuncT>>),
    Function(FuncT),
    Return(Option<ExprTy<VarDeclT, VarRefT, FuncT>>),
    // Class(Class),
}

impl<
    DeclT: Display + Clone + PartialEq,
    RefT: Display + Clone + PartialEq,
    FuncT: PartialEq + Clone + Display
> Display for Stmt<DeclT, RefT, FuncT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expr(expr) => write!(f, "{};", expr.expr)?,
            Stmt::Block(block, size) => {
                write!(f, "{{\n{}:", size)?;
                for stmt in block.iter() {
                    write!(f, "{}", stmt)?;
                }
                write!(f, "}}\n")?
            }
            Stmt::Print(expr) => write!(f, "print {};", expr.expr)?,
            Stmt::Variable(name, expr, _) => write!(f, "var {} = {};", name, expr.expr)?,
            Stmt::If(expr, block, else_block) => write!(f, "if ({}) {{\n {} \n}} else {{\n{}\n}}\n", expr, block, else_block.as_ref().map(|b| format!("else {}", b)).unwrap_or_else(|| "".to_string()))?,
            Stmt::While(expr, block) => write!(f, "while ({}) {{\n {} \n}}", expr.expr, block)?,
            Stmt::Function(func) => write!(f, "{}", func)?,
            Stmt::Return(expr) => write!(f, "return {};", if let Some(expr) = expr { format!("{}", expr.expr) } else { "".to_string() })?,
        };
        write!(f, "\n")
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOp {
    AND,
    OR,
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOp::AND => write!(f, "&&"),
            LogicalOp::OR => write!(f, "||"),
        }
    }
}


#[derive(Clone, Debug, PartialEq)]
pub enum Expr<
    DeclT: PartialEq + Clone + Display,
    RefT: PartialEq + Clone + Display,
    FuncT: PartialEq + Clone + Display> {
    Binary(ExprTy<DeclT, RefT, FuncT>, BinOp, ExprTy<DeclT, RefT, FuncT>),
    Call(ExprTy<DeclT, RefT, FuncT>, Vec<ExprTy<DeclT, RefT, FuncT>>),
    Grouping(ExprTy<DeclT, RefT, FuncT>),
    Get(ExprTy<DeclT, RefT, FuncT>, Symbol),
    Literal(crate::value::Value),
    Unary(UnaryOp, ExprTy<DeclT, RefT, FuncT>),
    Set(ExprTy<DeclT, RefT, FuncT>, Symbol, ExprTy<DeclT, RefT, FuncT>),
    Variable(RefT),
    Super(Symbol),
    Assign(RefT, ExprTy<DeclT, RefT, FuncT>),
    Logical(ExprTy<DeclT, RefT, FuncT>, LogicalOp, ExprTy<DeclT, RefT, FuncT>),
    This(),
}

impl<
    DeclT: Display + PartialEq + Clone,
    RefT: Display + PartialEq + Clone,
    FuncT: PartialEq + Clone + Display
> Display for Expr<DeclT, RefT, FuncT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Binary(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Expr::Call(callee, args) => {
                let mut str = format!("{}(", callee);
                for arg in args.iter() {
                    str.push_str(&format!("{}, ", arg));
                }
                str.push_str(")\n");
                f.write_str(&str)
            }
            Expr::Grouping(expr) => write!(f, "({})", expr),
            Expr::Get(object, name) => write!(f, "{}.{}", object, name),
            Expr::Literal(value) => write!(f, "{}", value),
            Expr::Unary(op, right) => write!(f, "{}{}", op, right),
            Expr::Set(object, name, value) => write!(f, "{}.{} = {}", object, name, value),
            Expr::Variable(name) => write!(f, "{}", name),
            Expr::Super(method) => write!(f, "super.{}", method),
            Expr::Assign(name, value) => write!(f, "{} = {}", name, value),
            Expr::Logical(left, op, right) => write!(f, "{} {} {}", left, op, right),
            Expr::This() => write!(f, "this"),
        }
    }
}


#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum BinOp {
    EQUAL_EQUAL,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    PLUS,
    SLASH,
    MULT,
    MINUS,
    GREATER,
    GREATER_EQUAL,
    AND,
    OR,
}

impl Display for BinOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BinOp::EQUAL_EQUAL => write!(f, "=="),
            BinOp::BANG_EQUAL => write!(f, "!="),
            BinOp::LESS => write!(f, "<"),
            BinOp::LESS_EQUAL => write!(f, "<="),
            BinOp::PLUS => write!(f, "+"),
            BinOp::SLASH => write!(f, "/"),
            BinOp::MULT => write!(f, "*"),
            BinOp::MINUS => write!(f, "-"),
            BinOp::GREATER => write!(f, ">"),
            BinOp::GREATER_EQUAL => write!(f, ">="),
            BinOp::AND => write!(f, "&&"),
            BinOp::OR => write!(f, "||"),
        }
    }
}

#[allow(non_camel_case_types)]
#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub enum UnaryOp {
    MINUS,
    BANG,
}

impl LogicalOp {
    pub fn new(tk: Token) -> LogicalOp {
        match tk.kind {
            TType::And => LogicalOp::AND,
            TType::Or => LogicalOp::OR,
            _ => panic!("{:?} is not a valid logical op", tk.kind)
        }
    }
}

impl BinOp {
    pub fn new(tk: Token) -> BinOp {
        match tk.kind {
            TType::Plus => BinOp::PLUS,
            TType::Star => BinOp::MULT,
            TType::EqEq => BinOp::EQUAL_EQUAL,
            TType::BangEq => BinOp::BANG_EQUAL,
            TType::Minus => BinOp::MINUS,
            TType::Slash => BinOp::SLASH,
            TType::Greater => BinOp::GREATER,
            TType::GreaterEq => BinOp::GREATER_EQUAL,
            TType::Less => BinOp::LESS,
            TType::LessEq => BinOp::LESS_EQUAL,
            TType::And => BinOp::AND,
            TType::Or => BinOp::OR,
            _ => panic!("is not a valid binary op")
        }
    }
}

impl UnaryOp {
    pub fn new(tk: Token) -> UnaryOp {
        match tk.kind {
            TType::Bang => UnaryOp::BANG,
            TType::Minus => UnaryOp::MINUS,
            _ => panic!("is not a valid unary op")
        }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::BANG => write!(f, "!"),
            UnaryOp::MINUS => write!(f, "-"),
        }
    }
}

pub type ExprResult<Declt, RefT, PF> = Result<ExprTy<Declt, RefT, PF>, ParserError>;