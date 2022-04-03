use crate::class::Class;
use crate::scanner::{Token, TType};
use crate::{SourceRef, Symbol};
use crate::ast::parser_func::ParserFunc;
use crate::resolver::Resolved;

#[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct ParserError {
    pub msg: String,
    pub context: SourceRef,
}

impl ParserError {
    pub fn new(msg: String, context: SourceRef) -> ParserError { ParserError { msg, context } }
}

pub type Tokens = Vec<Token>;

pub type ExprTy = Box<ExprInContext>;

#[derive(Clone, Debug)]
pub struct ExprInContext {
    pub context: SourceRef,
    pub expr: Expr,
}

impl PartialEq for ExprInContext {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

impl ExprInContext {
    pub fn new(expr: Expr, context: SourceRef) -> ExprInContext {
        ExprInContext { expr, context }
    }
}

#[derive(Clone, Debug)]
pub enum Stmt {
    Expr(ExprTy),
    Block(Box<Vec<Stmt>>),
    Print(ExprTy),
    Variable(Symbol, ExprTy, Option<Resolved>, SourceRef),
    If(ExprTy, Box<Stmt>, Option<Box<Stmt>>),
    While(ExprTy, Box<Stmt>),
    Function(ParserFunc),
    Return(Option<ExprTy>),
    Class(Class),
}


#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOp {
    AND,
    OR,
}

#[derive(Clone, PartialEq, Debug)]
pub enum Expr {
    Binary(ExprTy, BinOp, ExprTy),
    Call(ExprTy, Vec<ExprTy>),
    Grouping(ExprTy),
    Get(ExprTy, Symbol),
    Literal(crate::Value),
    Unary(UnaryOp, ExprTy),
    Set(ExprTy, Symbol, ExprTy),
    Variable(Variable),
    Super(Symbol, Option<Resolved>),
    Assign(Symbol, ExprTy, Option<Resolved>),
    Logical(ExprTy, LogicalOp, ExprTy),
    This(Option<Resolved>),
}


#[derive(Clone, PartialOrd, PartialEq, Debug)]
pub struct Variable {
    pub name: Symbol,
    pub resolved: Option<Resolved>,
}

impl Variable {
    pub fn new(name: Symbol) -> Variable { Variable { name, resolved: None } }
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

pub type ExprResult = Result<ExprTy, ParserError>;