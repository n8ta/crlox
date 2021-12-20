use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::{swap};
use std::rc::Rc;
use crate::ops::{OpTrait, Add, Div, EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Mult, Nil, Not, NotEqual, Pop, Print, Sub, True, Negate, Const, Ret, DefGlobal, GetGlobal, SetGlobal, SetLocal, GetLocal};
use crate::scanner::{IDENTIFIER_TTYPE_ID, Num, Scanner, STRING_TTYPE_ID, Token, TType, TTypeId};
use crate::{Chunk, SourceRef, Symbol};
use crate::scanner::TType::PRINT;
use crate::symbolizer::Symbolizer;
use crate::value::{Value};

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
#[derive(Clone, Copy, PartialOrd, PartialEq)]
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

struct Local {
    name: Symbol,
    depth: usize,
    src: SourceRef,
}

pub struct Compiler {
    src: String,
    parser: Parser,
    current_chunk: Chunk,
    rules: HashMap<TTypeId, Rule>,
    scanner: Scanner,
    symbolizer: Symbolizer,
    local_count: usize,
    scope_depth: usize,
    locals: Vec<Local>,
}

impl Compiler {
    pub fn compile(src: String, symbolizer: Symbolizer) -> Result<Chunk, CompilerError> {
        let mut compiler = Compiler::new(src, symbolizer);
        compiler.run()?;
        Ret {}.write(&mut compiler.current_chunk, SourceRef::simple());
        let mut c = Chunk::new();
        swap(&mut c, &mut compiler.current_chunk);
        Ok(c)
    }
    fn new(src: String, symbolizer: Symbolizer) -> Compiler {
        let scanner = Scanner::new(src.clone(), symbolizer.clone());
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
            rules.insert(TType::BANG_EQUAL.id(), Rule::new(None, Some(binary), Precedence::EQUALITY));
            rules.insert(TType::EQUAL.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EQUAL_EQUAL.id(), Rule::new(None, Some(binary), Precedence::EQUALITY));
            rules.insert(TType::GREATER.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::GREATER_EQUAL.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::LESS.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::LESS_EQUAL.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(IDENTIFIER_TTYPE_ID, Rule::new(Some(variable), None, Precedence::NONE));
            rules.insert(STRING_TTYPE_ID, Rule::new(Some(string), None, Precedence::NONE));
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
            locals: vec![],
            src,
            current_chunk: Chunk::new(),
            scanner,
            parser: Parser {
                panic_mode: false,
                had_error: None,
                current: Token::new(TType::AND, SourceRef::simple()),
                previous: Token::new(TType::AND, SourceRef::simple()),
            },
            symbolizer,
            rules,
            local_count: 0,
            scope_depth: 0,
        }
    }

    fn run(&mut self) -> Result<(), CompilerError> {
        advance(self)?;

        while !matches(self, TType::EOF.id()) {
            declaration(self, true)?;
        }
        if let Some(err) = &self.parser.had_error {
            return Err(CompilerError::new(err.msg.clone(), err.src.clone()));
        }
        Ok(())
    }
}

struct Parser {
    current: Token,
    previous: Token,
    panic_mode: bool,
    had_error: Option<CompilerError>,
}

type CompilerFnTy = fn(&mut Compiler, bool) -> Result<(), CompilerError>;

struct Rule {
    prefix: Option<CompilerFnTy>,
    infix: Option<CompilerFnTy>,
    precedence: Precedence,
}

impl Rule {
    fn new(prefix: Option<CompilerFnTy>, infix: Option<CompilerFnTy>, precedence: Precedence) -> Rule { Rule { prefix, infix, precedence } }
}

fn synchronize(compiler: &mut Compiler) -> Result<(), CompilerError> {
    compiler.parser.panic_mode = false;
    loop {
        let token = &compiler.parser.current.kind;
        if let TType::EOF = token {
            break;
        }
        match token {
            TType::CLASS |
            TType::FUN | TType::VAR | TType::FOR |
            TType::IF | TType::WHILE | TType::PRINT |
            TType::RETURN => return Ok(()),
            _ => (),
        }
        advance(compiler)?;
    }
    Ok(())
}

fn matches(compiler: &mut Compiler, typ: TTypeId) -> bool {
    if compiler.parser.current.kind.id() != typ {
        return false;
    }
    advance(compiler);
    true
}

fn begin_scope(compiler: &mut Compiler) {
    compiler.scope_depth += 1;
    compiler.local_count = 0;
}

fn end_scope(compiler: &mut Compiler) {
    compiler.scope_depth -= 1;

    while compiler.local_count > 0 && compiler.locals[compiler.local_count].depth > compiler.scope_depth{
        let popped = compiler.locals.pop().unwrap();
        Pop{}.write(&mut compiler.current_chunk, popped.src );
        compiler.local_count -= 1;
    }
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

    let can_assign = prec <= Precedence::ASSIGNMENT;
    prefix_rule(compiler, can_assign)?;

    while (prec as u8) <= (get_rule(&compiler.rules, &compiler.parser.current.kind).precedence as u8) {
        advance(compiler)?;
        let infix = get_rule(&compiler.rules, &compiler.parser.previous.kind).infix.expect("No infix for this rule");
        infix(compiler, false)?;
    }

    if can_assign && matches(compiler, TType::EQUAL.id()) {
        Err(CompilerError::new(format!("Invalid assignment target"), compiler.parser.previous.src.clone()))
    } else {
        Ok(())
    }
}

fn identifier_constant(sym: Symbol, chunk: &mut Chunk) -> Result<Const, CompilerError> {
    Ok(chunk.add_const(Value::String(sym)))
}

fn add_local(compiler: &mut Compiler, name: Symbol, src: SourceRef) -> Result<(), CompilerError> {
    if compiler.locals.len() > 255 {
        return Err(CompilerError::new(format!("Hit maximum of 255 local variables in scope"), compiler.parser.previous.src.clone()));
    }
    compiler.locals.push(Local { name, depth: compiler.scope_depth, src});
    compiler.local_count += 1;
    Ok(())
}

fn define_variable(compiler: &mut Compiler, global: DefGlobal) -> Result<(), CompilerError> {
    if compiler.scope_depth > 0 {} else {
        global.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone())
    }
    Ok(())
}

fn declare_variable(compiler: &mut Compiler, name: Symbol, src: SourceRef) -> Result<(), CompilerError> {
    if compiler.scope_depth == 0 {
        Ok(())
    } else {
        for local in compiler.locals.iter() {
            if local.depth < compiler.scope_depth {
                break
            }
            if local.name == name {
                return Err(CompilerError::new(format!("Cannot define two variables with the name {}", name), compiler.parser.previous.src.clone()))
            }
        }
        add_local(compiler, name, src);
        Ok(())
    }
}

fn parse_variable(compiler: &mut Compiler, message: &str) -> Result<Const, CompilerError> {
    consume(compiler, IDENTIFIER_TTYPE_ID, message)?;
    if let TType::IDENTIFIER(str) = &compiler.parser.previous.kind.clone() {
        declare_variable(compiler, str.clone(), compiler.parser.previous.src.clone()) ?;
        if compiler.scope_depth > 0 {
            Ok(compiler.current_chunk.add_const(Value::String(str.clone())))
        } else {
            identifier_constant(str.clone(), &mut compiler.current_chunk)
        }
    } else {
        let typ = compiler.parser.previous.kind.clone().tname();
        Err(CompilerError::new(
            format!("Expected to find an identifier but found a {}",
                    typ),
            compiler.parser.previous.src.clone()))
    }
}

fn check(compiler: &mut Compiler, typ: TTypeId) -> bool {
    compiler.parser.current.kind.id() == typ
}

fn block(compiler: &mut Compiler) -> Result<(), CompilerError> {
    while !check(compiler, TType::RIGHT_BRACE.id()) && !check(compiler, TType::EOF.id()) {
        declaration(compiler, false)?;
    }
    consume(compiler, TType::RIGHT_BRACE.id(), "Expected '}' after block")
}

fn var_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let const_ref = parse_variable(compiler, "Expected to find an identifier after a var")?;
    if matches(compiler, TType::EQUAL.id()) {
        expression(compiler, can_assign)?;
    } else {
        Nil {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone());
    }
    consume(compiler, TType::SEMICOLON.id(), "Expect ';' after variable declaration")?;
    define_variable(compiler,DefGlobal { idx: const_ref.idx });
    // let def_global =
    // def_global.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone());
    Ok(())
}

fn declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::VAR.id()) {
        var_declaration(compiler, can_assign)?;
    } else {
        statement(compiler, can_assign)?;
    }
    if compiler.parser.panic_mode {
        synchronize(compiler)?;
    }
    Ok(())
}

fn statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::PRINT.id()) {
        print_statement(compiler, can_assign)
    } else if matches(compiler, TType::LEFT_BRACE.id()) {
        begin_scope(compiler);
        block(compiler)?;
        end_scope(compiler);
        Ok(())
    } else {
        expression_statement(compiler, can_assign)
    }
}

fn expression_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::SEMICOLON.id(), "Expected ';' after expression.")?;
    Pop {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone());
    Ok(())
}

fn print_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::SEMICOLON.id(), "Expect ';' after value.")?;
    Print {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone());
    Ok(())
}

fn number(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    if let TType::NUMBER(num) = &compiler.parser.previous.kind {
        emit_const(compiler, Value::Num(num.num), compiler.parser.previous.src.clone());
    } else {
        error_at_prev(
            compiler,
            &format!("Expected to find a number but found a {}", &compiler.parser.previous.kind))
            ?;
    }
    Ok(())
}

fn expression(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    parse_precedence(compiler, Precedence::ASSIGNMENT)?;
    Ok(())
}

fn unary(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    let typ = compiler.parser.previous.clone();
    parse_precedence(compiler, Precedence::UNARY)?;
    match typ.kind {
        TType::MINUS => Negate {}.write(&mut compiler.current_chunk, typ.src),
        TType::BANG => Not {}.write(&mut compiler.current_chunk, typ.src),
        _ => panic!("unreachable"),
    }
    Ok(())
}

fn emit_const(compiler: &mut Compiler, value: Value, src: SourceRef) -> Const {
    let con = compiler.current_chunk.add_const(value);
    con.write(&mut compiler.current_chunk, src.clone());
    con
}

fn grouping(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected a ')' at the end of a grouping")
}

fn binary(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    let typ = compiler.parser.previous.clone();
    let rule = get_rule(&compiler.rules, &typ.kind);
    let prec: u8 = rule.precedence as u8;
    parse_precedence(compiler, (prec + 1).into())?;
    match typ.kind {
        TType::PLUS => Add {}.write(&mut compiler.current_chunk, typ.src),
        TType::MINUS => Sub {}.write(&mut compiler.current_chunk, typ.src),
        TType::SLASH => Div {}.write(&mut compiler.current_chunk, typ.src),
        TType::STAR => Mult {}.write(&mut compiler.current_chunk, typ.src),
        TType::BANG_EQUAL => NotEqual {}.write(&mut compiler.current_chunk, typ.src),
        TType::EQUAL_EQUAL => EqualEqual {}.write(&mut compiler.current_chunk, typ.src),
        TType::LESS => Less {}.write(&mut compiler.current_chunk, typ.src),
        TType::LESS_EQUAL => LessOrEq {}.write(&mut compiler.current_chunk, typ.src),
        TType::GREATER => Greater {}.write(&mut compiler.current_chunk, typ.src),
        TType::GREATER_EQUAL => GreaterOrEq {}.write(&mut compiler.current_chunk, typ.src),
        _ => panic!("bad typ"),
    }
    Ok(())
}

fn string(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    if let TType::STRING(str) = &compiler.parser.previous.kind {
        emit_const(compiler,
                   Value::String(str.clone()),
                   compiler.parser.previous.src.clone());
        Ok(())
    } else {
        Err(CompilerError::new(format!("Expected to find a string but found a {}", &compiler.parser.previous.kind),
                               compiler.parser.previous.src.clone()))
    }
}

fn literal(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    match compiler.parser.previous.kind {
        TType::TRUE => True {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        TType::FALSE => False {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        TType::NIL => Nil {}.write(&mut compiler.current_chunk, compiler.parser.previous.src.clone()),
        _ => panic!("not a literal!"),
    }
    Ok(())
}

fn variable(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    named_variable(compiler, can_assign)
}

enum Resolution {
    Local(u8),
    Global,
}
fn resolve_local(compiler: &mut Compiler, sym: &Symbol) -> Resolution {
    for (i, local) in compiler.locals.iter().rev().enumerate() {
        if local.name == *sym {
            return Resolution::Local(i as u8)
        }
    }
    Resolution::Global

}

fn named_variable(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if let TType::IDENTIFIER(str) = &compiler.parser.previous.kind.clone() {

        let prev = compiler.parser.previous.src.clone();
        match resolve_local(compiler, str) {
            Resolution::Local(local_idx) => {
                if can_assign && matches(compiler, TType::EQUAL.id()) {
                    expression(compiler, can_assign)?;
                    SetLocal { idx: local_idx }.write(&mut compiler.current_chunk, prev);
                } else {
                    GetLocal { idx: local_idx }.write(&mut compiler.current_chunk, prev);
                }
            }
            Resolution::Global => {
                let const_ref = identifier_constant(str.clone(), &mut compiler.current_chunk)?;
                if can_assign && matches(compiler, TType::EQUAL.id()) {
                    expression(compiler, can_assign)?;
                    SetGlobal { idx: const_ref.idx }.write(&mut compiler.current_chunk, prev);
                } else {
                    GetGlobal { idx: const_ref.idx }.write(&mut compiler.current_chunk, prev);
                }

            }
        }


        Ok(())
    } else {
        panic!("compiler error");
    }
}

fn consume(compiler: &mut Compiler, typ: TTypeId, message: &str) -> Result<(), CompilerError> {
    if compiler.parser.current.kind.id() == typ {
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