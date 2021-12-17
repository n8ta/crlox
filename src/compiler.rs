use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::{swap};
use crate::{Add, Chunk, Const, Div, Mult, Negate, OpTrait, Ret, SourceRef, Sub};
use crate::ops::{False, Nil, Not, True};
use crate::scanner::{Num, Scanner, Token, TType, TTypeId};
use crate::value::Value;

pub struct CompilerError {
    msg: String,
    src: SourceRef,
}

impl CompilerError {
    pub fn new(msg: String, src: SourceRef) -> CompilerError { CompilerError { msg, src } }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Error: {}\n{}", &self.msg, &self.src))
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1,
    // =
    OR = 2,
    // or
    AND = 3,
    // and
    EQUALITY = 4,
    // == !=
    COMPARISON = 5,
    // < > <= >=
    TERM = 6,
    // + -
    FACTOR = 7,
    // * /
    UNARY = 8,
    // ! -
    CALL = 9,
    // . ()
    PRIMARY = 10,
    MAX = 11,
}

impl From<u8> for Precedence {
    fn from(num: u8) -> Self {
        match num {
            0 => Precedence::NONE,
            1 => Precedence::ASSIGNMENT,
            2 => Precedence::OR,
            3 => Precedence::AND,
            4 => Precedence::EQUALITY,
            5 => Precedence::COMPARISON,
            6 => Precedence::TERM,
            7 => Precedence::FACTOR,
            8 => Precedence::UNARY,
            9 => Precedence::CALL,
            10 => Precedence::PRIMARY,
            _ => Precedence::MAX,
        }
    }
}

pub struct Compiler {
    src: String,
    parser: Parser,
    current_chunk: Chunk,
    rules: HashMap<TTypeId, Rule>,
    scanner: Scanner,
}

struct Parser {
    current: Token,
    previous: Token,
    panic_mode: bool,
    had_error: Option<CompilerError>,
}

type CompilerFnTy = fn(&mut Compiler) -> Result<(), CompilerError>;

struct Rule {
    prefix: Option<CompilerFnTy>,
    infix: Option<CompilerFnTy>,
    precedence: Precedence,
}

impl Rule {
    fn new(prefix: Option<CompilerFnTy>, infix: Option<CompilerFnTy>, precedence: Precedence) -> Rule { Rule { prefix, infix, precedence } }
}

fn parse_precedence(compiler: &mut Compiler, prec: Precedence) -> Result<(), CompilerError> {
    advance(compiler)?;
    let prefix_rule = get_rule(&compiler.rules, &compiler.parser.previous.kind); // todo: error?
    let prefix_rule = if let Some(func) = prefix_rule.prefix {
        func
    } else {
        let msg = format!("Unexpected token: {}", compiler.parser.previous.kind);
        return error_at_prev(compiler,
                                &msg);
    };

    prefix_rule(compiler)?;

    while (prec as u8) <= (get_rule(&compiler.rules, &compiler.parser.current.kind).precedence as u8) {
        advance(compiler)?;
        let infix = get_rule(&compiler.rules, &compiler.parser.previous.kind).infix.expect("No infix for this rule");
        infix(compiler)?;
    }
    Ok(())
}

fn number(compiler: &mut Compiler) -> Result<(), CompilerError> {
    if let TType::NUMBER(num) = &compiler.parser.previous.kind {
        emit_const(compiler, Value::Num(num.num), compiler.parser.previous.src.clone());
    } else {
        error_at_prev(
            compiler,
            &format!("Exected to find a number but found a {}", &compiler.parser.previous.kind))
            ?;
    }
    Ok(())
}

fn expression(compiler: &mut Compiler) -> Result<(), CompilerError> {
    parse_precedence(compiler, Precedence::ASSIGNMENT)?;
    Ok(())
}

fn unary(compiler: &mut Compiler) -> Result<(), CompilerError> {
    let typ = compiler.parser.previous.clone();
    parse_precedence(compiler, Precedence::UNARY)?;
    match typ.kind {
        TType::MINUS => Negate {}.write(&mut compiler.current_chunk, typ.src),
        TType::BANG => Not{}.write(&mut compiler.current_chunk, typ.src),
        _ => panic!("unreachable"),
    }
    Ok(())
}

fn emit_const(compiler: &mut Compiler, value: Value, src: SourceRef) -> Const {
    let con = compiler.current_chunk.add_const(value);
    con.write(&mut compiler.current_chunk, src.clone());
    con
}

fn grouping(compiler: &mut Compiler) -> Result<(), CompilerError> {
    expression(compiler)?;
    consume(compiler, TType::RIGHT_PAREN, "Expected a ')' at the end of a grouping")
}

fn binary(compiler: &mut Compiler) -> Result<(), CompilerError> {
    let typ = compiler.parser.previous.clone();
    let rule = get_rule(&compiler.rules, &typ.kind);
    let prec: u8 = rule.precedence as u8;
    parse_precedence(compiler, (prec + 1).into())?;
    match typ.kind {
        TType::PLUS => Add {}.write(&mut compiler.current_chunk, typ.src),
        TType::MINUS => Sub {}.write(&mut compiler.current_chunk, typ.src),
        TType::SLASH => Div {}.write(&mut compiler.current_chunk, typ.src),
        TType::STAR => Mult {}.write(&mut compiler.current_chunk, typ.src),
        _ => panic!("bad typ"),
    }
    Ok(())
}

fn literal(compiler: &mut Compiler) -> Result<(), CompilerError> {
    match compiler.parser.previous.kind {
        TType::TRUE => True{}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        TType::FALSE => False{}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        TType::NIL => Nil{}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        _ => panic!("not a literal!"),
    }
    Ok(())
}


fn consume(compiler: &mut Compiler, typ: TType, message: &str) -> Result<(), CompilerError> {
    if compiler.parser.current.kind == typ {
        advance(compiler)?;
        return Ok(());
    }
    error_at_current(compiler, message)
}

fn get_rule<'a>(map: &'a HashMap<u32, Rule>, typ: &TType) -> &'a Rule {
    map.get(&typ.id()).unwrap()
}

fn advance(compiler: &mut Compiler) -> Result<(), CompilerError> {
    swap(&mut compiler.parser.current, &mut compiler.parser.previous);
    loop {
        compiler.parser.current = compiler.scanner.scan_token();
        match &compiler.parser.current.kind {
            TType::ERROR(msg) => {
                error_at_current(compiler, &format!("Scanner error: {}", msg))?;
            }
            _ => {
                break;
            }
        }
    }
    Ok(())
}

fn error_at_current(compiler: &mut Compiler, message: &str) -> Result<(), CompilerError> {
    error_at(compiler, message, true)
}

fn error_at_prev(compiler: &mut Compiler, message: &str) -> Result<(), CompilerError> {
    error_at(compiler, message, false)
}

fn error_at(compiler: &mut Compiler, message: &str, current: bool) -> Result<(), CompilerError> {
    if compiler.parser.panic_mode {
        return Ok(());
    }
    compiler.parser.panic_mode = true;
    let src = if current { compiler.parser.current.src.clone() } else { compiler.parser.previous.src.clone() };
    Err(CompilerError::new(message.to_string(), src))
}


impl Compiler {
    pub fn compile(src: String) -> Result<Chunk, CompilerError> {
        let mut compiler = Compiler::new(src);
        compiler.run()?;
        Ret {}.write(&mut compiler.current_chunk, SourceRef::simple());
        let mut c = Chunk::new();
        swap(&mut c, &mut compiler.current_chunk);
        Ok(c)
    }
    fn new(src: String) -> Compiler {
        let scanner = Scanner::new(src.clone());
        let mut rules = HashMap::new();
        {
            rules.insert(TType::LEFT_PAREN.id(), Rule::new(Some(grouping), None, Precedence::NONE));
            rules.insert(TType::RIGHT_PAREN.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::LEFT_BRACE.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::RIGHT_BRACE.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::COMMA.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::DOT.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::MINUS.id(), Rule::new(Some(unary), Some(binary), Precedence::TERM));
            rules.insert(TType::PLUS.id(), Rule::new(None, Some(binary), Precedence::TERM));
            rules.insert(TType::SEMICOLON.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::SLASH.id(), Rule::new(None, Some(binary), Precedence::FACTOR));
            rules.insert(TType::STAR.id(), Rule::new(None, Some(binary), Precedence::FACTOR));
            rules.insert(TType::BANG.id(), Rule::new(Some(unary), None, Precedence::NONE));
            rules.insert(TType::BANG_EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EQUAL_EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::GREATER.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::GREATER_EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::LESS.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::LESS_EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::IDENTIFIER(format!("")).id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::STRING(format!("")).id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::NUMBER(Num { num: 0.0 }).id(), Rule::new(Some(number), None, Precedence::NONE));
            rules.insert(TType::AND.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::CLASS.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::ELSE.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::FALSE.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::FOR.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::FUN.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::IF.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::NIL.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::OR.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::PRINT.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::RETURN.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::SUPER.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::THIS.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::TRUE.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::VAR.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::WHILE.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::ERROR(format!("")).id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EOF.id(), Rule::new(None, None, Precedence::NONE));
        }
        Compiler {
            src,
            current_chunk: Chunk::new(),
            scanner,
            parser: Parser {
                panic_mode: false,
                had_error: None,
                current: Token::new(TType::AND, SourceRef::simple()),
                previous: Token::new(TType::AND, SourceRef::simple()),
            },
            rules,
        }
    }

    fn run(&mut self) -> Result<(), CompilerError> {
        advance(self)?;
        expression(self)?;
        consume(self, TType::EOF, "Expected end of file")?;
        if let Some(err) = &self.parser.had_error {
            return Err(CompilerError::new(err.msg.clone(), err.src.clone()));
        }
        Ok(())
    }
}