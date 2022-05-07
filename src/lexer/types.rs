use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use crate::lexer::{Source, Symbol, SourceRef};

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TType,
    pub src: SourceRef,
}

impl Token {
    pub fn new_src(ttype: TType, offset: usize, len: usize, line: usize, src: Rc<Source>) -> Token {
        Token { kind: ttype, src: SourceRef::new(offset, len, line, src) }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Num {
    pub num: f64,
}

impl Eq for Num {}

impl Hash for Num {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.num as i64).hash(state)
    }
}

pub type TTypeId = u32;

#[derive(Debug, Clone, PartialOrd, PartialEq, Eq, Hash)]
#[repr(u32)]
pub enum TType {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens.
    Bang,
    BangEq,
    Eq,
    EqEq,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    // Literals.
    Identifier(Symbol),
    String(Symbol),
    Number(Num),
    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    Error(String),
    EOF,
    Stack,
}

pub const NUMBER_TTYPE_ID: TTypeId = 21;
pub const IDENTIFIER_TTYPE_ID: TTypeId = 19;
pub const STRING_TTYPE_ID: TTypeId = 20;
pub const ERROR_TTYPE_ID: TTypeId = 38;

impl Display for TType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TType::LeftParen => "LEFT_PAREN",
            TType::RightParen => "RIGHT_PAREN",
            TType::LeftBrace => "LEFT_BRACE",
            TType::RightBrace => "RIGHT_BRACE",
            TType::Comma => "COMMA",
            TType::Dot => "DOT",
            TType::Minus => "MINUS",
            TType::Plus => "PLUS",
            TType::Semicolon => "SEMICOLON",
            TType::Slash => "SLASH",
            TType::Star => "STAR",
            TType::Bang => "BANG",
            TType::BangEq => "BANG_EQUAL",
            TType::Eq => "EQUAL",
            TType::EqEq => "EQUAL_EQUAL",
            TType::Greater => "GREATER",
            TType::GreaterEq => "GREATER_EQUAL",
            TType::Less => "LESS",
            TType::LessEq => "LESS_EQUAL",
            TType::Identifier(_) => "IDENTIFIER",
            TType::String(_) => "STRING",
            TType::Number(_) => "NUMBER",
            TType::And => "AND",
            TType::Class => "CLASS",
            TType::Else => "ELSE",
            TType::False => "FALSE",
            TType::For => "FOR",
            TType::Fun => "FUN",
            TType::If => "IF",
            TType::Nil => "NIL",
            TType::Or => "OR",
            TType::Print => "PRINT",
            TType::Return => "RETURN",
            TType::Super => "SUPER",
            TType::This => "THIS",
            TType::True => "TRUE",
            TType::Var => "VAR",
            TType::While => "WHILE",
            TType::Error(_) => "ERROR",
            TType::EOF => "EOF",
            TType::Stack => "stack",
        };
        f.write_str(str)
    }
}

impl TType {
    pub fn tname(self) -> &'static str {
        match self {
            TType::LeftParen => "(",
            TType::RightParen => ")",
            TType::LeftBrace => "{",
            TType::RightBrace => "}",
            TType::Comma => ",",
            TType::Dot => ".",
            TType::Minus => "-",
            TType::Plus => "+",
            TType::Semicolon => ";",
            TType::Slash => "/",
            TType::Star => "*",
            TType::Bang => "!",
            TType::BangEq => "!=",
            TType::Eq => "=",
            TType::EqEq => "==",
            TType::Greater => ">",
            TType::GreaterEq => ">=",
            TType::Less => "<",
            TType::LessEq => "<=",
            TType::Identifier(_) => "identifier",
            TType::String(_) => "string",
            TType::Number(_) => "number",
            TType::And => "and",
            TType::Class => "class",
            TType::Else => "else",
            TType::False => "false",
            TType::For => "for",
            TType::Fun => "fun",
            TType::If => "if",
            TType::Nil => "nil",
            TType::Or => "or",
            TType::Print => "print",
            TType::Return => "return",
            TType::Super => "super",
            TType::This => "this",
            TType::True => "true",
            TType::Var => "var",
            TType::While => "while",
            TType::Error(_) => "error",
            TType::EOF => "EOF",
            TType::Stack => "stack",
        }
    }
    pub fn id(&self) -> TTypeId {
        match self {
            TType::LeftParen => 0,
            TType::RightParen => 1,
            TType::LeftBrace => 2,
            TType::RightBrace => 3,
            TType::Comma => 4,
            TType::Dot => 5,
            TType::Minus => 6,
            TType::Plus => 7,
            TType::Semicolon => 8,
            TType::Slash => 9,
            TType::Star => 10,
            TType::Bang => 11,
            TType::BangEq => 12,
            TType::Eq => 13,
            TType::EqEq => 14,
            TType::Greater => 15,
            TType::GreaterEq => 16,
            TType::Less => 17,
            TType::LessEq => 18,
            TType::Identifier(_) => IDENTIFIER_TTYPE_ID,
            TType::String(_) => STRING_TTYPE_ID,
            TType::Number(_) => NUMBER_TTYPE_ID,
            TType::And => 22,
            TType::Class => 23,
            TType::Else => 24,
            TType::False => 25,
            TType::For => 26,
            TType::Fun => 27,
            TType::If => 28,
            TType::Nil => 29,
            TType::Or => 30,
            TType::Print => 31,
            TType::Return => 32,
            TType::Super => 33,
            TType::This => 34,
            TType::True => 35,
            TType::Var => 36,
            TType::While => 37,
            TType::Error(_) => ERROR_TTYPE_ID,
            TType::EOF => 39,
            TType::Stack => 40,
        }
    }
}