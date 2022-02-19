use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::swap;
use std::rc::Rc;
use std::str::FromStr;
use crate::source_ref::Source;
use crate::{SourceRef, Symbolizer};
use crate::symbolizer::Symbol;
use crate::trie::Trie;

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TType,
    pub src: SourceRef,
}

impl Token {
    pub fn new(kind: TType, src: SourceRef) -> Token {
        Token { kind, src }
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
}

pub const IDENTIFIER_TTYPE_ID: TTypeId = 19;
pub const STRING_TTYPE_ID: TTypeId = 20;

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
            TType::Number(_) => 21,
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
            TType::Error(_) => 38,
            TType::EOF => 39,
        }
    }
}

#[derive(Clone)]
pub struct Scanner {
    inner: Rc<RefCell<ScannerInner>>,
}

struct ScannerInner {
    start: usize,
    current: usize,
    line: usize,
    src: Rc<Source>,
    chars: Vec<char>,
    keywords: Trie<u8, TType>,
    symbolizer: Symbolizer,
    curr: Token,
    previous: Token,
}


impl Scanner {
    pub fn scan_token(&mut self) -> Token {
        self.inner.borrow_mut().scan_token()
    }
    pub fn new(src: String, symbolize: Symbolizer) -> Scanner {
        Scanner {
            inner: Rc::new(RefCell::new(ScannerInner::new(src, symbolize))),
        }
    }
    pub fn current(&self) -> Token {
        self.inner.borrow().curr.clone()
    }
    pub fn previous(&self) -> Token {
        self.inner.borrow().previous.clone()
    }
}

impl ScannerInner {
    fn new(src: String, symbolizer: Symbolizer) -> ScannerInner {
        let mut keywords: Trie<u8, TType> = Trie::new();
        {
            keywords.insert("AND".as_bytes(), TType::And);
            keywords.insert("and".as_bytes(), TType::And);
            keywords.insert("CLASS".as_bytes(), TType::Class);
            keywords.insert("class".as_bytes(), TType::Class);
            keywords.insert("ELSE".as_bytes(), TType::Else);
            keywords.insert("else".as_bytes(), TType::Else);
            keywords.insert("IF".as_bytes(), TType::If);
            keywords.insert("if".as_bytes(), TType::If);
            keywords.insert("NIL".as_bytes(), TType::Nil);
            keywords.insert("nil".as_bytes(), TType::Nil);
            keywords.insert("OR".as_bytes(), TType::Or);
            keywords.insert("or".as_bytes(), TType::Or);
            keywords.insert("PRINT".as_bytes(), TType::Print);
            keywords.insert("print".as_bytes(), TType::Print);
            keywords.insert("RETURN".as_bytes(), TType::Return);
            keywords.insert("return".as_bytes(), TType::Return);
            keywords.insert("SUPER".as_bytes(), TType::Super);
            keywords.insert("super".as_bytes(), TType::Super);
            keywords.insert("VAR".as_bytes(), TType::Var);
            keywords.insert("var".as_bytes(), TType::Var);
            keywords.insert("WHILE".as_bytes(), TType::While);
            keywords.insert("while".as_bytes(), TType::While);
            keywords.insert("FALSE".as_bytes(), TType::False);
            keywords.insert("false".as_bytes(), TType::False);
            keywords.insert("TRUE".as_bytes(), TType::True);
            keywords.insert("true".as_bytes(), TType::True);
            keywords.insert("FUN".as_bytes(), TType::Fun);
            keywords.insert("fun".as_bytes(), TType::Fun);
            keywords.insert("RETURN".as_bytes(), TType::Return);
            keywords.insert("return".as_bytes(), TType::Return);
        }
        ScannerInner {
            keywords,
            start: 0,
            current: 0,
            line: 0,
            chars: src.chars().collect(),
            src: Rc::new(Source::new(src)),
            symbolizer,
            curr: Token::new(TType::And, SourceRef::simple()),
            previous: Token::new(TType::And, SourceRef::simple()),
        }
    }
    fn is_at_end(&self) -> bool {
        self.current >= self.chars.len()
    }
    fn make_src_ref(&self) -> SourceRef {
        SourceRef::new(
            self.start,
            self.current - self.start,
            self.line,
            self.src.clone())
    }
    fn make_token(&self, typ: TType) -> Token {
        Token::new(typ, self.make_src_ref())
    }
    fn make_err_token(&self, message: String) -> Token {
        Token::new(TType::Error(message), self.make_src_ref())
    }
    fn advance(&mut self) -> char {
        self.current += 1;
        *self.chars.get(self.current - 1).expect("Compiler error: Advanced called at end of input")
    }
    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; };
        if self.chars[self.current] != expected {
            return false;
        } else {
            self.current += 1;
            true
        }
    }
    fn peek(&self) -> char {
        match self.chars.get(self.current) {
            None => '\x00',
            Some(c) => *c,
        }
    }
    fn peek_next(&self) -> char {
        match self.chars.get(self.current) {
            None => '\x00',
            Some(c) => *c,
        }
    }
    fn strip_wspace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                _ => {
                    return;
                }
            }
        }
    }
    fn number(&mut self) -> Token {
        while self.peek().is_digit(10) { self.advance(); }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) { self.advance(); }
        }
        let str: String = self.chars[self.start..self.current].iter().collect();
        match f64::from_str(&str) {
            Ok(n) => self.make_token(TType::Number(Num { num: n })),
            Err(_) => self.make_err_token(format!("Tried to parse '{}' as a number but failed", str)),
        }
    }
    fn scan_token(&mut self) -> Token {
        swap(&mut self.previous, &mut self.curr);
        self.curr = self.scan_token_helper();
        self.curr.clone()
    }
    fn scan_token_helper(&mut self) -> Token {
        self.strip_wspace();
        self.start = self.current;
        if self.is_at_end() {
            return Token::new(TType::EOF, SourceRef::new(
                self.start.saturating_sub(1),
                1,
                self.src.lines,
                self.src.clone()));
        }
        let c = self.advance();
        if c.is_digit(10) {
            return self.number();
        }
        match c {
            '(' => self.make_token(TType::LeftParen),
            ')' => self.make_token(TType::RightParen),
            '{' => self.make_token(TType::LeftBrace),
            '}' => self.make_token(TType::RightBrace),
            ';' => self.make_token(TType::Semicolon),
            ',' => self.make_token(TType::Comma),
            '.' => self.make_token(TType::Dot),
            '-' => self.make_token(TType::Minus),
            '+' => self.make_token(TType::Plus),
            '*' => self.make_token(TType::Star),
            '!' => {
                let tk = if self.matches('=') { TType::BangEq } else { TType::Bang };
                self.make_token(tk)
            }
            '=' => {
                let tk = if self.matches('=') { TType::EqEq } else { TType::Eq };
                self.make_token(tk)
            }
            '<' => {
                let tk = if self.matches('=') { TType::LessEq } else { TType::Less };
                self.make_token(tk)
            }
            '>' => {
                let tk = if self.matches('=') { TType::GreaterEq } else { TType::Greater };
                self.make_token(tk)
            }
            '/' => {
                if self.peek_next() == '/' {
                    while !self.is_at_end() && self.peek() != '\n' { self.advance(); }
                }
                self.make_token(TType::Slash)
            }
            '"' => {
                while !self.is_at_end() && self.peek() != '"' {
                    if self.peek() == '\n' { self.line += 1 }
                    self.advance();
                }
                if self.is_at_end() {
                    return self.make_err_token("Unterminated string literal".to_string());
                }
                self.advance();
                let literal = self.chars[self.start + 1..self.current - 1].iter().collect();
                let sym = self.symbolizer.get_symbol(literal);
                self.make_token(TType::String(sym))
            }
            _ => {
                if c.is_alphabetic() {
                    while self.peek().is_alphanumeric() || self.peek() == '_' { self.advance(); }
                    let str = self.chars[self.start..self.current].iter().collect::<String>();
                    let sym = self.symbolizer.get_symbol(str.clone());
                    let keyword = self.keywords.find(sym.as_bytes());
                    if let Some(ttype) = keyword {
                        self.make_token((*ttype).clone())
                    } else {
                        self.make_token(TType::Identifier(sym))
                    }
                } else {
                    panic!("todo: error: {}", c);
                }
            }
        }
    }
}