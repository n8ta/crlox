use std::collections::HashMap;
use crate::source_ref::{SourceRef, Source};
use std::rc::Rc;
use crate::scanner::{Num, Token, TType};
use crate::{Symbolizer};

type ScannerResult = Result<Vec<Token>, (String, usize)>;

pub fn scanner(src: Rc<Source>, symbolizer: Symbolizer) -> ScannerResult {
    let mut scanner = Scanner::new(src, symbolizer);
    scanner.scan_tokens()
}

struct Scanner {
    source: Rc<Source>,
    src: String,
    start: usize,
    current: usize,
    line: usize,
    tokens: Vec<Token>,
    keywords: HashMap<String, TType>,
    symbolizer: Symbolizer,
}

impl Scanner {
    fn new(src: Rc<Source>, symbolizer: Symbolizer) -> Scanner {
        let mut map: HashMap<String, TType> = HashMap::default();
        map.insert(String::from("and"), TType::And);
        map.insert(String::from("else"), TType::Else);
        map.insert(String::from("false"), TType::False);
        map.insert(String::from("for"), TType::For);
        map.insert(String::from("fun"), TType::Fun);
        map.insert(String::from("if"), TType::If);
        map.insert(String::from("nil"), TType::Nil);
        map.insert(String::from("or"), TType::Or);
        map.insert(String::from("print"), TType::Print);
        map.insert(String::from("return"), TType::Return);
        map.insert(String::from("super"), TType::Super);
        map.insert(String::from("this"), TType::This);
        map.insert(String::from("var"), TType::Var);
        map.insert(String::from("while"), TType::While);
        map.insert(String::from("class"), TType::Class);
        map.insert(String::from("true"), TType::True);
        map.insert(String::from("false"), TType::False);

        Scanner {
            symbolizer,
            keywords: map,
            source: src.clone(),
            src: src.src.clone(),
            start: 0,
            current: 0,
            line: 0,
            tokens: vec![],
        }
    }
    fn is_at_end(&self) -> bool {
        self.current >= self.src.chars().count()
    }
    fn advance(&mut self) -> char {
        let x = self.src.chars().nth(self.current).unwrap();
        self.current += 1;
        x
    }
    fn add_token(&mut self, tt: TType) {
        self.tokens.push(
            Token::new_src(
                tt,
                self.start,
                self.current - self.start,
                self.line,
                self.source.clone(),
            )
        );
    }
    fn string(&mut self) -> Result<(), String> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' { self.line += 1; }
            self.advance();
        }
        if self.is_at_end() {
            let partial_str = self.src.chars().skip(self.start).take(self.src.len() - self.start).collect::<String>();
            return Err(format!("Unterminated String: {}", partial_str));
        }
        self.advance();
        let str = self.src.chars().skip(self.start + 1).take(self.current - self.start - 2).collect::<String>();
        let sym = self.symbolizer.get_symbol(str);
        self.add_token(TType::String(sym));
        return Ok(());
    }
    fn number(&mut self) -> Result<(), String> {
        while self.peek().is_digit(10) { self.advance(); }
        if self.peek() == '.' && self.peek_next().is_digit(10) {
            self.advance();
        }
        while self.peek().is_digit(10) { self.advance(); }

        let num = self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>();
        let float = match num.parse::<f64>() {
            Ok(float) => float,
            Err(_) => {
                return Err(String::from("Unable to parse f64 {}"));
            }
        };
        self.add_token(TType::Number(Num{num: float}));
        Ok(())
    }
    fn identifier(&mut self) -> Result<(), String> {
        while self.peek().is_alphanumeric() { self.advance(); }
        let ident = self.src.chars().skip(self.start).take(self.current - self.start).collect::<String>();
        let fetched: Option<TType> = self.keywords.get(&ident).and_then(|t| Some(t.clone()));
        match fetched {
            Some(k) => self.add_token(k.clone()),
            None => {
                let src = SourceRef::new(self.start, self.current - self.start, self.line, self.source.clone());
                let sym = self.symbolizer.get_symbol(src.source());
                self.add_token(TType::Identifier(sym))
            }
        };
        Ok(())
    }
    fn peek(&mut self) -> char {
        match self.src.chars().nth(self.current) {
            None => 0x0 as char,
            Some(c) => c,
        }
    }
    fn peek_next(&self) -> char {
        match self.src.chars().nth(self.current + 1) {
            None => 0x0 as char,
            Some(c) => c,
        }
    }
    fn scan_token(&mut self) -> Result<(), String> {
        let c = self.advance();
        match c {
            '(' => self.add_token(TType::LeftParen),
            ')' => self.add_token(TType::RightParen),
            '{' => self.add_token(TType::LeftBrace),
            '}' => self.add_token(TType::RightBrace),
            ',' => self.add_token(TType::Comma),
            '.' => self.add_token(TType::Dot),
            '-' => self.add_token(TType::Minus),
            '+' => self.add_token(TType::Plus),
            ';' => self.add_token(TType::Semicolon),
            '*' => self.add_token(TType::Star),
            '!' => {
                let tt = match self.matches('=') {
                    true => TType::BangEq,
                    false => TType::Bang,
                };
                self.add_token(tt);
            }
            '=' => {
                let tt = match self.matches('=') {
                    true => TType::EqEq,
                    false => TType::Eq
                };
                self.add_token(tt)
            }
            '<' => {
                let tt = match self.matches('=') {
                    true => TType::LessEq,
                    false => TType::Less
                };
                self.add_token(tt)
            }
            '>' => {
                let tt = match self.matches('=') {
                    true => TType::GreaterEq,
                    false => TType::Greater
                };
                self.add_token(tt)
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(TType::Slash);
                }
            }
            '"' => self.string()?,
            '\r' => (),
            '\t' => (),
            ' ' => (),
            '\n' => self.line += 1,
            _ => {
                if c.is_digit(10) {
                    self.number()?;
                } else if c.is_alphabetic() {
                    self.identifier()?;
                } else {
                    return Err(format!("Unexpected token `{}`", c));
                }
            }
        }
        Ok(())
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() { return false; }
        if self.src.chars().nth(self.current).unwrap() != expected { return false; }
        self.current += 1;
        true
    }

    fn scan_tokens(&mut self) -> ScannerResult {
        while !self.is_at_end() {
            if let Err(x) = self.scan_token() {
                return Err((x, self.line));
            }
            self.start = self.current;
        }
        self.tokens.push(Token::new_src(
            TType::EOF,
            self.current,
            self.current - self.start,
            self.line,
            self.source.clone(),
        ));
        Ok(self.tokens.clone())
    }
}