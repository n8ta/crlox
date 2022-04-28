use std::fmt::{Display, Formatter};
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

pub type ExprTy<DeclT, RefT> = Box<ExprInContext<DeclT, RefT>>;

#[allow(unused)]
#[derive(Clone, Debug)]
pub struct ExprInContext<DeclT: PartialEq + Clone + Display, RefT: PartialEq + Clone + Display> {
    pub context: SourceRef,
    pub expr: Expr<DeclT, RefT>,
    _marker: std::marker::PhantomData<(DeclT, RefT)>,
}

impl<T: PartialEq + Clone + Display, S: Clone + PartialEq + Display> Display for ExprInContext<T, S> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{}", self.expr))
    }
}

impl<T: PartialEq + Clone + Display, S: Clone + PartialEq + Display> PartialEq for ExprInContext<T, S> {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl<T: Clone + PartialEq + Display, S: Clone + PartialEq + Display> ExprInContext<T, S> {
    pub fn new(expr: Expr<T, S>, context: SourceRef) -> ExprInContext<T, S> {
        ExprInContext { expr, context, _marker: std::marker::PhantomData }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt<VarDeclT: Clone + PartialEq + Display, VarRefT: Clone + PartialEq + Display> {
    Expr(ExprTy<VarDeclT, VarRefT>),
    Block(Box<Vec<Stmt<VarDeclT, VarRefT>>>),
    Print(ExprTy<VarDeclT, VarRefT>),
    Variable(VarDeclT, ExprTy<VarDeclT, VarRefT>, SourceRef),
    If(ExprTy<VarDeclT, VarRefT>, Box<Stmt<VarDeclT, VarRefT>>, Option<Box<Stmt<VarDeclT, VarRefT>>>),
    While(ExprTy<VarDeclT, VarRefT>, Box<Stmt<VarDeclT, VarRefT>>),
    Function(ParserFunc<VarDeclT, VarRefT>),
    Return(Option<ExprTy<VarDeclT, VarRefT>>),
    // Class(Class),
}

impl<DeclT: Display + Clone + PartialEq, RefT: Display + Clone + PartialEq> Display for Stmt<DeclT, RefT> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Stmt::Expr(expr) => format!("{};", expr.expr),
            Stmt::Block(block) => {
                let mut str = "{\n".to_string();
                for stmt in block.iter() {
                    str.push_str(&format!("{}", stmt));
                }
                str.push_str("}\n");
                str
            }
            Stmt::Print(expr) => format!("print {};", expr.expr),
            Stmt::Variable(name, expr, _) => format!("var {} = {};", name, expr.expr),
            Stmt::If(expr, block, else_block) => format!("if ({}) {{\n {} \n}} else {{\n{}\n}}\n", expr, block, else_block.as_ref().map(|b| format!("else {}", b)).unwrap_or_else(|| "".to_string())),
            Stmt::While(expr, block) => format!("while ({}) {{\n {} \n}}", expr.expr, block),
            Stmt::Function(func) => format!("{}", func),
            Stmt::Return(expr) => format!("return {};", if let Some(expr) = expr { format!("{}", expr.expr) } else { "".to_string() }),
        };
        f.write_str(&s);
        f.write_str("\n")
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
pub enum Expr<DeclT: PartialEq + Clone + Display, RefT: PartialEq + Clone + Display> {
    Binary(ExprTy<DeclT, RefT>, BinOp, ExprTy<DeclT, RefT>),
    Call(ExprTy<DeclT, RefT>, Vec<ExprTy<DeclT, RefT>>),
    Grouping(ExprTy<DeclT, RefT>),
    Get(ExprTy<DeclT, RefT>, Symbol),
    Literal(crate::value::Value),
    Unary(UnaryOp, ExprTy<DeclT, RefT>),
    Set(ExprTy<DeclT, RefT>, Symbol, ExprTy<DeclT, RefT>),
    Variable(RefT),
    Super(Symbol),
    Assign(RefT, ExprTy<DeclT, RefT>),
    Logical(ExprTy<DeclT, RefT>, LogicalOp, ExprTy<DeclT, RefT>),
    This(),
}

impl<DeclT: Display + PartialEq + Clone, RefT: Display + PartialEq + Clone> Display for Expr<DeclT, RefT> {
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

pub type ExprResult<Declt, RefT> = Result<ExprTy<Declt, RefT>, ParserError>;