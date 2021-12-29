use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::{swap};
use crate::ops::{OpTrait, Add, Div, EqualEqual, False, Greater, GreaterOrEq, Less, LessOrEq, Mult, Nil, Not, NotEqual, Pop, Print, Sub, True, Negate, Const, Ret, DefGlobal, GetGlobal, SetGlobal, SetLocal, GetLocal, RelJump, RelJumpIfFalse, OpJumpTrait, Call};
use crate::scanner::{IDENTIFIER_TTYPE_ID, Num, Scanner, STRING_TTYPE_ID, Token, TType, TTypeId};
use crate::{Chunk, SourceRef, Symbol};
use crate::chunk::Write;
use crate::func::{Func, FuncType};
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

#[derive(Debug)]
struct Local {
    name: Symbol,
    depth: usize,
    src: SourceRef,
    initialized: bool,
}

struct Scope {
    local_count: usize,
}

pub struct Compiler {
    pub current_chunk: Chunk,
    src: String,
    parser: Parser,
    rules: HashMap<TTypeId, Rule>,
    scanner: Scanner,
    symbolizer: Symbolizer,
    scopes: Vec<Scope>,
    locals: Vec<Local>,
}

impl Compiler {
    pub fn prev_source(&self) -> &SourceRef {
        &self.parser.previous.src
    }
    pub fn compile(src: String, symbolizer: Symbolizer) -> Result<Func, CompilerError> {
        let mut compiler = Compiler::new(src, symbolizer);
        compiler.run()?;
        Ret {}.write(&mut compiler.current_chunk, SourceRef::simple());
        let mut c = Chunk::new();
        swap(&mut c, &mut compiler.current_chunk);
        Ok(Func::global(c))
    }
    fn new(src: String, symbolizer: Symbolizer) -> Compiler {
        let scanner = Scanner::new(src.clone(), symbolizer.clone());
        let mut rules = HashMap::new();
        {
            rules.insert(TType::LEFT_PAREN.id(), Rule::new(Some(grouping), Some(call), Precedence::CALL));
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
            rules.insert(TType::CLASS.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::ELSE.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::FALSE.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::FOR.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::FUN.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::IF.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::NIL.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::OR.id(), Rule::new(None, Some(or_), Precedence::NONE));
            rules.insert(TType::AND.id(), Rule::new(None, Some(and_), Precedence::NONE));
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
            current_chunk: Chunk::new(),
            locals: vec![],
            src,
            scanner,
            parser: Parser {
                panic_mode: false,
                had_error: None,
                current: Token::new(TType::AND, SourceRef::simple()),
                previous: Token::new(TType::AND, SourceRef::simple()),
            },
            symbolizer,
            rules,
            scopes: vec![],
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
    compiler.scopes.push(Scope { local_count: 0 });
}

fn end_scope(compiler: &mut Compiler) {
    let scope_depth = compiler.scopes.len() - 1;
    let scope = compiler.scopes.last_mut().unwrap();
    let mut pops = 0;
    while scope.local_count > 0 && compiler.locals.last().unwrap().depth > (scope_depth) {
        let popped = compiler.locals.pop().unwrap();
        pops += 1;
        scope.local_count -= 1;
    }
    for _ in (0..pops) {
        Pop {}.emit(compiler);
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
    compiler.locals.push(Local { name, depth: compiler.scopes.len() - 1, src, initialized: false });
    compiler.scopes.last_mut().unwrap().local_count += 1;
    Ok(())
}

fn mark_initialized(compiler: &mut Compiler) -> Result<(), CompilerError> {
    let depth = compiler.scopes.len();
    if depth == 0 { return Ok(()); }
    let last = compiler.locals.last_mut().unwrap();
    last.depth = depth;
    last.initialized = true;
    Ok(())
}

fn define_variable(compiler: &mut Compiler, global: DefGlobal) -> Result<(), CompilerError> {
    if compiler.scopes.len() > 0 {
        mark_initialized(compiler)?;
        return Ok(());
    }
    global.emit(compiler);
    Ok(())
}

fn declare_variable(compiler: &mut Compiler, name: Symbol, src: SourceRef) -> Result<(), CompilerError> {
    if compiler.scopes.len() == 0 {
        Ok(())
    } else {
        for local in compiler.locals.iter() {
            if local.depth < compiler.scopes.len()  {
                break;
            }
            if local.name == name {
                return Err(CompilerError::new(format!("Cannot define two variables with the name {}", name), compiler.parser.previous.src.clone()));
            }
        }
        add_local(compiler, name, src)?;
        Ok(())
    }
}

fn parse_variable(compiler: &mut Compiler, message: &str) -> Result<(Symbol, Const), CompilerError> {
    consume(compiler, IDENTIFIER_TTYPE_ID, message)?;
    if let TType::IDENTIFIER(str) = &compiler.parser.previous.kind.clone() {
        declare_variable(compiler, str.clone(), compiler.parser.previous.src.clone())?;
        if compiler.scopes.len() > 0 {
            Ok((str.clone(), compiler.current_chunk.add_const(Value::String(str.clone()))))
        } else {
            Ok((str.clone(), identifier_constant(str.clone(), &mut compiler.current_chunk)?))
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
    let (_name, const_ref) = parse_variable(compiler, "Expected to find an identifier after a var")?;
    if matches(compiler, TType::EQUAL.id()) {
        expression(compiler, can_assign)?;
    } else {
        Nil {}.emit(compiler);
    }
    consume(compiler, TType::SEMICOLON.id(), "Expect ';' after variable declaration")?;
    define_variable(compiler, DefGlobal { idx: const_ref.idx })?;
    Ok(())
}


fn function(compiler: &mut Compiler, name: Symbol, _can_assign: bool) -> Result<Const, CompilerError> {
    consume(compiler, TType::LEFT_PAREN.id(), "Expected '(' after function name")?;
    let mut new_chunk = Chunk::new();
    let mut locals = vec![];
    swap(&mut compiler.current_chunk, &mut new_chunk);
    swap(&mut compiler.locals, &mut locals);
    begin_scope(compiler);
    declare_variable(compiler, name.clone(), compiler.parser.previous.src.clone())?;
    mark_initialized(compiler)?;
    let mut arity: usize = 0;
    if !(check(compiler, TType::RIGHT_PAREN.id())) {
        let mut first = true;
        while matches(compiler, TType::COMMA.id()) || first {
            first = false;
            arity += 1;
            let (_symbol, name_const) = parse_variable(compiler, "Expected a parameter name here")?;
            define_variable(compiler, DefGlobal { idx: name_const.idx })?;
        }
    }
    let arity: u8 = if arity >= (u8::MAX as usize) {
        return Err(CompilerError::new(format!("Cannot have more than {} parameters", u8::MAX), compiler.parser.previous.src.clone()));
    } else {
        arity as u8
    };
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after parameters")?;
    consume(compiler, TType::LEFT_BRACE.id(), "Expected '{' before function body")?;
    block(compiler)?;

    end_scope(compiler);
    let _ = compiler.scopes.pop();

    swap(&mut compiler.current_chunk, &mut new_chunk);
    swap(&mut compiler.locals, &mut locals);

    let func_const = compiler.current_chunk.add_const(Value::Func(
        Func::new(name.clone(),
                  arity,
                  FuncType::Function,
                  new_chunk,
        )));


    Ok(func_const)
}

fn fun_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let (sym, name_const) = parse_variable(compiler, "Expected to find an identifier after a function")?;
    mark_initialized(compiler)?;
    let func_const = function(compiler, sym, can_assign)?;
    func_const.emit(compiler);
    define_variable(compiler, DefGlobal { idx: name_const.idx })?;
    Ok(())
}

fn declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::FUN.id()) {
        fun_declaration(compiler, can_assign)?;
    } else if matches(compiler, TType::VAR.id()) {
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
    } else if matches(compiler, TType::RETURN.id()) {
        return_statement(compiler, can_assign)
    } else if matches(compiler, TType::IF.id()) {
        if_statement(compiler, can_assign)
    } else if matches(compiler, TType::WHILE.id()) {
        while_statement(compiler, can_assign)
    } else if matches(compiler, TType::LEFT_BRACE.id()) {
        begin_scope(compiler);
        block(compiler)?;
        end_scope(compiler);
        Ok(())
    } else {
        expression_statement(compiler, can_assign)
    }
}

fn and_(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let jf = RelJumpIfFalse { idx: -1 };
    Pop {}.emit(compiler);
    Ok(())
}

fn or_(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let else_jump = RelJumpIfFalse { idx: -1 };
    let end_jump = RelJump { idx: -1 };
    let else_write = else_jump.emit(compiler);
    let end_write = end_jump.emit(compiler);
    else_jump.overwrite(&mut compiler.current_chunk, &else_write);
    Pop {}.emit(compiler);
    parse_precedence(compiler, Precedence::OR)?;
    end_jump.overwrite(&mut compiler.current_chunk, &end_write);
    Ok(())
}


fn expression_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::SEMICOLON.id(), "Expected ';' after expression.")?;
    Pop {}.emit(compiler);
    Ok(())
}

fn return_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::SEMICOLON.id()) {
        Nil {}.emit(compiler);
        Ret {}.emit(compiler);
    } else {
        expression(compiler, can_assign)?;
        consume(compiler, TType::SEMICOLON.id(), "Expected ';' after return value.")?;
        Ret {}.emit(compiler);
    }
    Ok(())
}

fn print_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::SEMICOLON.id(), "Expect ';' after value.")?;
    Print {}.emit(compiler);
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
    };
    Ok(())
}

fn emit_const(compiler: &mut Compiler, value: Value, src: SourceRef) -> Const {
    let con = compiler.current_chunk.add_const(value);
    con.write(&mut compiler.current_chunk, src.clone());
    con
}

fn argument_list(compiler: &mut Compiler) -> Result<u8, CompilerError> {
    let mut count = 0;
    if !matches(compiler, TType::RIGHT_PAREN.id()) {
        let mut first = true;
        while check(compiler, TType::COMMA.id()) || first {
            if !first {
                advance(compiler)?;
            }
            first = false;
            expression(compiler, false)?;
            count += 1;
        }
    }
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after arguments.")?;
    if count > u8::MAX {
        Err(CompilerError::new(format!("Cannot have more than {} arguments.", u8::MAX), compiler.parser.previous.src.clone()))
    } else {
        Ok(count as u8)
    }
}

fn call(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    let arity = argument_list(compiler)?;
    Call { arity }.emit(compiler);
    Ok(())
}

fn grouping(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected a ')' at the end of a grouping")
}


// Expression
// FalseJump :IfNotJump
// Stmt: if-body
// :IfNotJump
// rest

// Expression
// FalseJump :IfNotJump
// Stmt: if-body
// Jump :IfDoneJump
// :IfNotJump
// Stmt: else-body
// :IfDoneJump
// ...

fn if_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    consume(compiler, TType::LEFT_PAREN.id(), "Expected '(' after if.")?;
    expression(compiler, can_assign)?;
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after 'if (expression'")?;
    let mut if_jump = RelJumpIfFalse { idx: -1 };

    let if_jump_write: Write = if_jump.emit(compiler);

    statement(compiler, can_assign)?;

    if matches(compiler, TType::ELSE.id()) {
        let mut if_done_jump = RelJump { idx: -1 };
        let if_done_jump_write = if_done_jump.emit(compiler);
        if_jump.overwrite(&mut compiler.current_chunk, &if_jump_write);
        statement(compiler, can_assign)?;
        if_done_jump.overwrite(&mut compiler.current_chunk, &if_done_jump_write);
    } else {
        if_jump.overwrite(&mut compiler.current_chunk, &if_jump_write);
    }
    Ok(())
}


// :start
// expression
// jump if false :done
// ...body...
// jump :start
// :done
fn while_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let loop_start = compiler.current_chunk.len() as i16;
    consume(compiler, TType::LEFT_PAREN.id(), "Expected '(' after while.")?;
    expression(compiler, can_assign)?;
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after while expression.")?;
    let mut exit_jump = RelJumpIfFalse { idx: -1 }; // jump to :done if expression is false
    let exit_jump_write = exit_jump.emit(compiler);
    Pop {}.emit(compiler); // pop while loop expression
    statement(compiler, can_assign)?; // body
    let jump_to_start = RelJump { idx: -((compiler.current_chunk.len() as i16) - loop_start) };
    jump_to_start.emit(compiler); // jump to :start
    exit_jump.overwrite(&mut compiler.current_chunk, &exit_jump_write); // :done
    Pop {}.emit(compiler); // pop while loop expression
    Ok(())
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
    };
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
        TType::TRUE => True {}.emit(compiler),
        TType::FALSE => False {}.emit(compiler),
        TType::NIL => Nil {}.emit(compiler),
        _ => panic!("not a literal!"),
    };
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

    for (i, local) in compiler.locals.iter().enumerate().rev() {
        if local.name == *sym {
            return Resolution::Local(i as u8);
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