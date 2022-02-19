use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem::swap;
use std::num::ParseFloatError;
use std::rc::Rc;
use std::str::FromStr;
use std::thread::current;
use crate::source_ref::Source;
use crate::{debug_println, SourceRef, Symbolizer};
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
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER(Symbol),
    STRING(Symbol),
    NUMBER(Num),
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    ERROR(String),
    EOF,
}

pub const IDENTIFIER_TTYPE_ID: TTypeId = 19;
pub const STRING_TTYPE_ID: TTypeId = 20;

impl Display for TType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            TType::LEFT_PAREN => "LEFT_PAREN",
            TType::RIGHT_PAREN => "RIGHT_PAREN",
            TType::LEFT_BRACE => "LEFT_BRACE",
            TType::RIGHT_BRACE => "RIGHT_BRACE",
            TType::COMMA => "COMMA",
            TType::DOT => "DOT",
            TType::MINUS => "MINUS",
            TType::PLUS => "PLUS",
            TType::SEMICOLON => "SEMICOLON",
            TType::SLASH => "SLASH",
            TType::STAR => "STAR",
            TType::BANG => "BANG",
            TType::BANG_EQUAL => "BANG_EQUAL",
            TType::EQUAL => "EQUAL",
            TType::EQUAL_EQUAL => "EQUAL_EQUAL",
            TType::GREATER => "GREATER",
            TType::GREATER_EQUAL => "GREATER_EQUAL",
            TType::LESS => "LESS",
            TType::LESS_EQUAL => "LESS_EQUAL",
            TType::IDENTIFIER(_) => "IDENTIFIER",
            TType::STRING(_) => "STRING",
            TType::NUMBER(_) => "NUMBER",
            TType::AND => "AND",
            TType::CLASS => "CLASS",
            TType::ELSE => "ELSE",
            TType::FALSE => "FALSE",
            TType::FOR => "FOR",
            TType::FUN => "FUN",
            TType::IF => "IF",
            TType::NIL => "NIL",
            TType::OR => "OR",
            TType::PRINT => "PRINT",
            TType::RETURN => "RETURN",
            TType::SUPER => "SUPER",
            TType::THIS => "THIS",
            TType::TRUE => "TRUE",
            TType::VAR => "VAR",
            TType::WHILE => "WHILE",
            TType::ERROR(_) => "ERROR",
            TType::EOF => "EOF",
        };
        f.write_str(str)
    }
}

impl TType {
    pub fn tname(self) -> &'static str {
        match self {
            TType::LEFT_PAREN => "(",
            TType::RIGHT_PAREN => ")",
            TType::LEFT_BRACE => "{",
            TType::RIGHT_BRACE => "}",
            TType::COMMA => ",",
            TType::DOT => ".",
            TType::MINUS => "-",
            TType::PLUS => "+",
            TType::SEMICOLON => ";",
            TType::SLASH => "/",
            TType::STAR => "*",
            TType::BANG => "!",
            TType::BANG_EQUAL => "!=",
            TType::EQUAL => "=",
            TType::EQUAL_EQUAL => "==",
            TType::GREATER => ">",
            TType::GREATER_EQUAL => ">=",
            TType::LESS => "<",
            TType::LESS_EQUAL => "<=",
            TType::IDENTIFIER(_) => "identifier",
            TType::STRING(_) => "string",
            TType::NUMBER(_) => "number",
            TType::AND => "and",
            TType::CLASS => "class",
            TType::ELSE => "else",
            TType::FALSE => "false",
            TType::FOR => "for",
            TType::FUN => "fun",
            TType::IF => "if",
            TType::NIL => "nil",
            TType::OR => "or",
            TType::PRINT => "print",
            TType::RETURN => "return",
            TType::SUPER => "super",
            TType::THIS => "this",
            TType::TRUE => "true",
            TType::VAR => "var",
            TType::WHILE => "while",
            TType::ERROR(_) => "error",
            TType::EOF => "EOF",
        }
    }
    pub fn id(&self) -> TTypeId {
        match self {
            TType::LEFT_PAREN => 0,
            TType::RIGHT_PAREN => 1,
            TType::LEFT_BRACE => 2,
            TType::RIGHT_BRACE => 3,
            TType::COMMA => 4,
            TType::DOT => 5,
            TType::MINUS => 6,
            TType::PLUS => 7,
            TType::SEMICOLON => 8,
            TType::SLASH => 9,
            TType::STAR => 10,
            TType::BANG => 11,
            TType::BANG_EQUAL => 12,
            TType::EQUAL => 13,
            TType::EQUAL_EQUAL => 14,
            TType::GREATER => 15,
            TType::GREATER_EQUAL => 16,
            TType::LESS => 17,
            TType::LESS_EQUAL => 18,
            TType::IDENTIFIER(_) => IDENTIFIER_TTYPE_ID,
            TType::STRING(_) => STRING_TTYPE_ID,
            TType::NUMBER(_) => 21,
            TType::AND => 22,
            TType::CLASS => 23,
            TType::ELSE => 24,
            TType::FALSE => 25,
            TType::FOR => 26,
            TType::FUN => 27,
            TType::IF => 28,
            TType::NIL => 29,
            TType::OR => 30,
            TType::PRINT => 31,
            TType::RETURN => 32,
            TType::SUPER => 33,
            TType::THIS => 34,
            TType::TRUE => 35,
            TType::VAR => 36,
            TType::WHILE => 37,
            TType::ERROR(_) => 38,
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
        debug_println!("Scanner-current: {:?}", self.inner.borrow().curr.kind);
        self.inner.borrow().curr.clone()
    }
    pub fn previous(&self) -> Token {
        debug_println!("Scanner-previous: {:?}", self.inner.borrow().previous.kind);
        self.inner.borrow().previous.clone()
    }
}

impl ScannerInner {
    fn new(src: String, symbolizer: Symbolizer) -> ScannerInner {
        let mut keywords: Trie<u8, TType> = Trie::new();
        {
            keywords.insert("AND".as_bytes(), TType::AND);
            keywords.insert("and".as_bytes(), TType::AND);
            keywords.insert("CLASS".as_bytes(), TType::CLASS);
            keywords.insert("class".as_bytes(), TType::CLASS);
            keywords.insert("ELSE".as_bytes(), TType::ELSE);
            keywords.insert("else".as_bytes(), TType::ELSE);
            keywords.insert("IF".as_bytes(), TType::IF);
            keywords.insert("if".as_bytes(), TType::IF);
            keywords.insert("NIL".as_bytes(), TType::NIL);
            keywords.insert("nil".as_bytes(), TType::NIL);
            keywords.insert("OR".as_bytes(), TType::OR);
            keywords.insert("or".as_bytes(), TType::OR);
            keywords.insert("PRINT".as_bytes(), TType::PRINT);
            keywords.insert("print".as_bytes(), TType::PRINT);
            keywords.insert("RETURN".as_bytes(), TType::RETURN);
            keywords.insert("return".as_bytes(), TType::RETURN);
            keywords.insert("SUPER".as_bytes(), TType::SUPER);
            keywords.insert("super".as_bytes(), TType::SUPER);
            keywords.insert("VAR".as_bytes(), TType::VAR);
            keywords.insert("var".as_bytes(), TType::VAR);
            keywords.insert("WHILE".as_bytes(), TType::WHILE);
            keywords.insert("while".as_bytes(), TType::WHILE);
            keywords.insert("FALSE".as_bytes(), TType::FALSE);
            keywords.insert("false".as_bytes(), TType::FALSE);
            keywords.insert("TRUE".as_bytes(), TType::TRUE);
            keywords.insert("true".as_bytes(), TType::TRUE);
            keywords.insert("FUN".as_bytes(), TType::FUN);
            keywords.insert("fun".as_bytes(), TType::FUN);
            keywords.insert("RETURN".as_bytes(), TType::RETURN);
            keywords.insert("return".as_bytes(), TType::RETURN);
        }
        ScannerInner {
            keywords,
            start: 0,
            current: 0,
            line: 0,
            chars: src.chars().collect(),
            src: Rc::new(Source::new(src)),
            symbolizer,
            curr: Token::new(TType::AND, SourceRef::simple()),
            previous: Token::new(TType::AND, SourceRef::simple()),
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
        Token::new(TType::ERROR(message), self.make_src_ref())
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
            Ok(n) => self.make_token(TType::NUMBER(Num { num: n })),
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
            '(' => self.make_token(TType::LEFT_PAREN),
            ')' => self.make_token(TType::RIGHT_PAREN),
            '{' => self.make_token(TType::LEFT_BRACE),
            '}' => self.make_token(TType::RIGHT_BRACE),
            ';' => self.make_token(TType::SEMICOLON),
            ',' => self.make_token(TType::COMMA),
            '.' => self.make_token(TType::DOT),
            '-' => self.make_token(TType::MINUS),
            '+' => self.make_token(TType::PLUS),
            '*' => self.make_token(TType::STAR),
            '!' => {
                let tk = if self.matches('=') { TType::BANG_EQUAL } else { TType::BANG };
                self.make_token(tk)
            }
            '=' => {
                let tk = if self.matches('=') { TType::EQUAL_EQUAL } else { TType::EQUAL };
                self.make_token(tk)
            }
            '<' => {
                let tk = if self.matches('=') { TType::LESS_EQUAL } else { TType::LESS };
                self.make_token(tk)
            }
            '>' => {
                let tk = if self.matches('=') { TType::GREATER_EQUAL } else { TType::GREATER };
                self.make_token(tk)
            }
            '/' => {
                if self.peek_next() == '/' {
                    while !self.is_at_end() && self.peek() != '\n' { self.advance(); }
                }
                self.make_token(TType::SLASH)
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
                self.make_token(TType::STRING(sym))
            }
            _ => {
                if c.is_alphabetic() {
                    while self.peek().is_alphanumeric() || self.peek() == '_' { self.advance(); }
                    let str = self.chars[self.start..self.current].iter().collect::<String>();
                    let sym = self.symbolizer.get_symbol(str.clone());
                    let keyword = self.keywords.find(str.as_bytes());
                    if let Some(ttype) = keyword {
                        self.make_token((*ttype).clone())
                    } else {
                        self.make_token(TType::IDENTIFIER(sym))
                    }
                } else {
                    panic!("todo: error: {}", c);
                }
            }
        }
    }
}