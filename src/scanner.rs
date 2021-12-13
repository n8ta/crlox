use std::rc::Rc;
use crate::source_ref::Source;
use crate::SourceRef;
use crate::trie::Trie;

pub struct Token {
    kind: TType,
    src: SourceRef,
}

impl Token {
    pub fn new(kind: TType, src: SourceRef) -> Token {
        Token { kind, src }
    }
}

#[derive(Debug, Clone)]
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
    IDENTIFIER(String),
    STRING(String),
    NUMBER,
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
}

pub struct Scanner {
    start: usize,
    current: usize,
    line: usize,
    src: Rc<Source>,
    chars: Vec<char>,
    keywords: Trie<u8, TType>,
}

impl Scanner {
    pub fn new(src: String) -> Scanner {
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
        }
        Scanner {
            keywords,
            start: 0,
            current: 0,
            line: 0,
            chars: src.chars().collect(),
            src: Rc::new(Source::new(src)),
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
        *self.chars.get(self.current).expect("Compiler error: bad peak")
    }
    fn peek_next(&self) -> char {
        *self.chars.get(self.current + 1).expect("Compiler error: bad peak_next")
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
        while (self.peek().is_digit(10)) { self.advance(); }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
            while self.peek().is_digit(10) { self.advance(); }
        }
        self.make_token(TType::NUMBER)
    }
    pub fn scan_token(&mut self) -> Option<Token> {
        self.strip_wspace();
        self.start = self.current;
        if self.is_at_end() { return None; }
        let c = self.advance();
        if c.is_digit(10) {
            return Some(self.number());
        }
        Some(match c {
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
                let tk = if self.matches('=') { TType::EQUAL_EQUAL } else { TType::EQUAL_EQUAL };
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
                    return Some(self.make_err_token("Unterminated string literal".to_string()));
                }
                self.advance();
                let literal = self.chars[self.start..self.current].iter().collect();
                self.make_token(TType::STRING(literal))
            }
            _ => {
                if c.is_alphabetic() {
                    while self.peek().is_alphanumeric() { self.advance(); }
                    let ident: String = self.chars[self.start..self.current].iter().collect::<String>();
                    if let Some(ttype) = self.keywords.find(ident.as_bytes()) {
                        self.make_token((*ttype).clone())
                    } else {
                        self.make_token(TType::IDENTIFIER(ident))
                    }

                } else {
                    panic!("todo: error");
                }
            }
        })
    }
}