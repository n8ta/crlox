use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::mem::{swap};
use std::sync::atomic::compiler_fence;
use crate::ops::{OpTrait, Add, Div, EqualEqual, False, Less, LessOrEq, Mult, Nil, Not, NotEqual, Pop, Print, Sub, True, Negate, Const, Ret, DefGlobal, GetGlobal, SetGlobal, SetLocal, GetLocal, RelJump, RelJumpIfFalse, OpJumpTrait, Call, SmallConst, Closure, SetUpValue, GetUpValue, OpU8};
use crate::scanner::{IDENTIFIER_TTYPE_ID, Num, Scanner, STRING_TTYPE_ID, Token, TType, TTypeId};
use crate::{Chunk, debug_println, SourceRef, Symbol};
use crate::chunk::Write;
use crate::closure::RtClosure;
use crate::func::{Func, FuncType};
use crate::source_ref::Source;
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

#[derive(Debug, Clone)]
pub struct Local {
    pub name: Symbol,
    depth: usize,
    src: SourceRef,
    initialized: bool,
}

impl Local {
    pub fn new(name: Symbol, depth: usize, src: SourceRef) -> Self {
        Local { name, depth, src, initialized: false }
    }
}

pub struct Scope {
    pub local_count: usize,
    pub locals: Vec<Local>,
}

impl Scope {
    pub fn new(local_count: usize, locals: Vec<Local>) -> Self {
        Scope { local_count, locals }
    }
}


struct Stack {
    scope: Vec<Scope>,
    ups: Vec<UpValue>,
}

pub struct Compiler {
    src: String,
    pub chunk: Chunk,
    rules: HashMap<TTypeId, Rule>,
    scanner: Scanner,
    symbolizer: Symbolizer,
    scopes: Vec<Scope>,
    ups: Vec<UpValue>,
    enclosing: Option<Box<Compiler>>,
    panic_mode: bool,
    had_error: Option<CompilerError>,
}

#[derive(PartialEq, PartialOrd, Clone)]
pub struct UpValue {
    pub index: u8,
    pub is_local: bool,
}

impl UpValue {
    fn emit(&self, compiler: &mut Compiler) {
        let prev = compiler.prev_source().clone();
        compiler.current_chunk().add_byte(self.index, prev.clone());
        compiler.current_chunk().add_byte(if self.is_local { 1 } else { 0 }, prev);
    }
}

impl Compiler {
    pub fn prev_source(&self) -> SourceRef {
        self.scanner.previous().src.clone()
    }
    pub fn compile(src: String, symbolizer: Symbolizer) -> Result<Func, CompilerError> {
        let scanner = Scanner::new(src.clone(), symbolizer.clone());
        let mut compiler = Compiler::new(src, symbolizer, scanner);
        compiler.run()?;
        Ret {}.write(&mut compiler.current_chunk(), SourceRef::simple());
        let mut c = Chunk::new();
        swap(&mut c, &mut compiler.chunk);
        Ok(Func::global(c))
    }
    pub fn current_chunk(&mut self) -> &mut Chunk {
        &mut self.chunk
    }
    fn new(src: String, symbolizer: Symbolizer, scanner: Scanner) -> Compiler {
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
            src,
            chunk: Chunk::new(),
            scanner,
            panic_mode: false,
            had_error: None,
            symbolizer,
            scopes: vec![],
            ups: vec![],
            rules,
            enclosing: None,
        }
    }

    fn run(&mut self) -> Result<(), CompilerError> {
        advance(self)?;

        while !matches(self, TType::EOF.id())? {
            declaration(self, true)?;
        }
        if let Some(err) = &self.had_error {
            return Err(CompilerError::new(err.msg.clone(), err.src.clone()));
        }
        Ok(())
    }
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
    compiler.panic_mode = false;
    loop {
        let token = &compiler.scanner.current().kind;
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

fn matches(compiler: &mut Compiler, typ: TTypeId) -> Result<bool, CompilerError> {
    if compiler.scanner.current().kind.id() != typ {
        return Ok(false);
    }
    advance(compiler)?;
    Ok(true)
}

fn begin_scope(compiler: &mut Compiler, is_func: bool) {
    compiler.scopes.push(Scope { local_count: 0, locals: vec![] })
}

fn end_scope(compiler: &mut Compiler) {
    let scope = compiler.scopes.pop().unwrap();
    for _ in 0..scope.locals.len() {
        Pop {}.emit(compiler);
    }
}

fn parse_precedence(compiler: &mut Compiler, prec: Precedence) -> Result<(), CompilerError> {
    advance(compiler)?;
    let prefix_rule = get_rule(&compiler.rules, &compiler.scanner.previous().kind); // todo: error?
    let prefix_rule = if let Some(func) = prefix_rule.prefix {
        func
    } else {
        let msg = format!("Unexpected token: {}", compiler.scanner.previous().kind);
        return error_at_prev(compiler,
                             &msg);
    };

    let can_assign = prec <= Precedence::ASSIGNMENT;
    prefix_rule(compiler, can_assign)?;

    while (prec as u8) <= (get_rule(&compiler.rules, &compiler.scanner.current().kind).precedence as u8) {
        advance(compiler)?;
        let infix = get_rule(&compiler.rules, &compiler.scanner.previous().kind).infix.expect("No infix for this rule");
        infix(compiler, false)?;
    }

    if can_assign && matches(compiler, TType::EQUAL.id())? {
        Err(CompilerError::new(format!("Invalid assignment target"), compiler.scanner.previous().src.clone()))
    } else {
        Ok(())
    }
}

fn identifier_constant(sym: Symbol, chunk: &mut Chunk) -> Result<Const, CompilerError> {
    Ok(chunk.add_const(Value::String(sym)))
}

fn add_local(compiler: &mut Compiler, name: Symbol, src: SourceRef) -> Result<(), CompilerError> {
    if compiler.scopes.last().expect("to be in a scope when adding a local").locals.len() > 255 {
        return Err(CompilerError::new(format!("Hit maximum of 255 local variables in scope"), compiler.scanner.previous().src.clone()));
    }
    let depth = compiler.scopes.len();
    compiler.scopes.last_mut().unwrap().locals.push(Local { name, depth: depth - 1, src, initialized: false });
    Ok(())
}

fn mark_initialized(compiler: &mut Compiler) -> Result<(), CompilerError> {
    let depth = compiler.scopes.len();
    if depth == 0 { return Ok(()); }
    let last = compiler.scopes.last_mut().expect("to be in scope").locals.last_mut().expect("locals to not be empty");
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
    if let Some(scope) = compiler.scopes.last() {
        for local in scope.locals.iter() {
            if local.depth < compiler.scopes.len() {
                break;
            }
            if local.name == name {
                return Err(CompilerError::new(format!("Cannot define two variables with the name {}", name), compiler.scanner.previous().src.clone()));
            }
        }
        add_local(compiler, name, src)?;
    }
    Ok(())
}

fn parse_variable(compiler: &mut Compiler, message: &str) -> Result<(Symbol, Const), CompilerError> {
    consume(compiler, IDENTIFIER_TTYPE_ID, message)?;
    if let TType::IDENTIFIER(str) = &compiler.scanner.previous().kind.clone() {
        declare_variable(compiler, str.clone(), compiler.scanner.previous().src.clone())?;
        if compiler.scopes.len() > 0 {
            Ok((str.clone(), compiler.current_chunk().add_const(Value::String(str.clone()))))
        } else {
            Ok((str.clone(), identifier_constant(str.clone(), &mut compiler.current_chunk())?))
        }
    } else {
        let typ = compiler.scanner.previous().kind.clone().tname();
        Err(CompilerError::new(
            format!("Expected to find an identifier but found a {}",
                    typ),
            compiler.scanner.previous().src.clone()))
    }
}

fn check(compiler: &mut Compiler, typ: TTypeId) -> bool {
    compiler.scanner.current().kind.id() == typ
}

fn block(compiler: &mut Compiler) -> Result<(), CompilerError> {
    while !check(compiler, TType::RIGHT_BRACE.id()) && !check(compiler, TType::EOF.id()) {
        declaration(compiler, false)?;
    }
    consume(compiler, TType::RIGHT_BRACE.id(), "Expected '}' after block")
}

fn var_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let (_name, const_ref) = parse_variable(compiler, "Expected to find an identifier after a var")?;
    if matches(compiler, TType::EQUAL.id())? {
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

    let mut new_compiler = Compiler::new(compiler.src.clone(), compiler.symbolizer.clone(), compiler.scanner.clone());

    let src = new_compiler.scanner.previous().src.clone();
    declare_variable(&mut new_compiler, name.clone(), src)?;
    mark_initialized(&mut new_compiler)?;
    let mut arity: usize = 0;
    if !(check(&mut new_compiler, TType::RIGHT_PAREN.id())) {
        let mut first = true;
        while matches(&mut new_compiler, TType::COMMA.id())? || first {
            first = false;
            arity += 1;
            let (_symbol, name_const) = parse_variable(&mut new_compiler, "Expected a parameter name here")?;
            define_variable(&mut new_compiler, DefGlobal { idx: name_const.idx })?;
        }
    }
    let arity: u8 = if arity >= (u8::MAX as usize) {
        return Err(CompilerError::new(format!("Cannot have more than {} parameters", u8::MAX), compiler.scanner.previous().src.clone()));
    } else {
        arity as u8
    };
    consume(&mut new_compiler, TType::RIGHT_PAREN.id(), "Expected ')' after parameters")?;
    consume(&mut new_compiler, TType::LEFT_BRACE.id(), "Expected '{' before function body")?;
    block(&mut new_compiler)?;

    Nil {}.emit(&mut new_compiler);
    Ret {}.emit(&mut new_compiler);

    end_scope(&mut new_compiler);
    let completed_chunk = new_compiler.chunk.clone();

    let func_const = compiler.current_chunk().add_const(Value::Func(
        Func::new(name.clone(),
                  arity,
                  FuncType::Function,
                  completed_chunk,
                  0,
        )));

    Closure { idx: func_const.idx }.emit(compiler);
    // for up in up_values.iter() {
    //     up.emit(compiler);
    // }
    Ok(func_const)
}

fn fun_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let (sym, name_const) = parse_variable(compiler, "Expected to find an identifier after a function")?;
    mark_initialized(compiler)?;
    let func_const = function(compiler, sym, can_assign)?;
    // func_const.emit(compiler);
    define_variable(compiler, DefGlobal { idx: name_const.idx })?;
    Ok(())
}

fn declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::FUN.id())? {
        fun_declaration(compiler, can_assign)?;
    } else if matches(compiler, TType::VAR.id())? {
        var_declaration(compiler, can_assign)?;
    } else {
        statement(compiler, can_assign)?;
    }
    if compiler.panic_mode {
        synchronize(compiler)?;
    }
    Ok(())
}

fn statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::PRINT.id())? {
        print_statement(compiler, can_assign)
    } else if matches(compiler, TType::RETURN.id())? {
        return_statement(compiler, can_assign)
    } else if matches(compiler, TType::IF.id())? {
        if_statement(compiler, can_assign)
    } else if matches(compiler, TType::WHILE.id())? {
        while_statement(compiler, can_assign)
    } else if matches(compiler, TType::LEFT_BRACE.id())? {
        begin_scope(compiler, false);
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
    else_jump.overwrite(&mut compiler.current_chunk(), &else_write);
    Pop {}.emit(compiler);
    parse_precedence(compiler, Precedence::OR)?;
    end_jump.overwrite(&mut compiler.current_chunk(), &end_write);
    Ok(())
}


fn expression_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::SEMICOLON.id(), "Expected ';' after expression.")?;
    Pop {}.emit(compiler);
    Ok(())
}

fn return_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::SEMICOLON.id())? {
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
    if let TType::NUMBER(num) = &compiler.scanner.previous().kind {
        emit_const(compiler, Value::Num(num.num), compiler.scanner.previous().src.clone());
    } else {
        error_at_prev(
            compiler,
            &format!("Expected to find a number but found a {}", &compiler.scanner.previous().kind))
            ?;
    }
    Ok(())
}

fn expression(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    parse_precedence(compiler, Precedence::ASSIGNMENT)?;
    Ok(())
}

fn unary(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    let typ = compiler.scanner.previous().clone();
    parse_precedence(compiler, Precedence::UNARY)?;
    match typ.kind {
        TType::MINUS => Negate {}.write(&mut compiler.current_chunk(), typ.src),
        TType::BANG => Not {}.write(&mut compiler.current_chunk(), typ.src),
        _ => panic!("unreachable"),
    };
    Ok(())
}

fn emit_const(compiler: &mut Compiler, value: Value, src: SourceRef) {
    if let Value::Num(num) = value {
        if (0.0..255.0).contains(&num) {
            SmallConst { val: num as u8 }.write(&mut compiler.current_chunk(), src);
            return;
        }
    }
    let con = compiler.current_chunk().add_const(value);
    con.write(&mut compiler.current_chunk(), src.clone());
}

fn argument_list(compiler: &mut Compiler) -> Result<u8, CompilerError> {
    let mut count = 0;
    if !matches(compiler, TType::RIGHT_PAREN.id())? {
        let mut first = true;
        while check(compiler, TType::COMMA.id()) || first {
            if !first {
                advance(compiler)?;
            }
            first = false;
            expression(compiler, false)?;
            count += 1;
        }
        consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after arguments.")?;
    }
    if count > u8::MAX {
        Err(CompilerError::new(format!("Cannot have more than {} arguments.", u8::MAX), compiler.scanner.previous().src.clone()))
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

    if matches(compiler, TType::ELSE.id())? {
        let mut if_done_jump = RelJump { idx: -1 };
        let if_done_jump_write = if_done_jump.emit(compiler);
        if_jump.overwrite(&mut compiler.current_chunk(), &if_jump_write);
        statement(compiler, can_assign)?;
        if_done_jump.overwrite(&mut compiler.current_chunk(), &if_done_jump_write);
    } else {
        if_jump.overwrite(&mut compiler.current_chunk(), &if_jump_write);
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
    let loop_start = compiler.current_chunk().len() as i16;
    consume(compiler, TType::LEFT_PAREN.id(), "Expected '(' after while.")?;
    expression(compiler, can_assign)?;
    consume(compiler, TType::RIGHT_PAREN.id(), "Expected ')' after while expression.")?;
    let mut exit_jump = RelJumpIfFalse { idx: -1 }; // jump to :done if expression is false
    let exit_jump_write = exit_jump.emit(compiler);
    Pop {}.emit(compiler); // pop while loop expression
    statement(compiler, can_assign)?; // body
    let jump_to_start = RelJump { idx: -((compiler.current_chunk().len() as i16) - loop_start) };
    jump_to_start.emit(compiler); // jump to :start
    exit_jump.overwrite(&mut compiler.current_chunk(), &exit_jump_write); // :done
    Pop {}.emit(compiler); // pop while loop expression
    Ok(())
}

fn binary(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    let typ = compiler.scanner.previous().clone();
    let rule = get_rule(&compiler.rules, &typ.kind);
    let prec: u8 = rule.precedence as u8;
    parse_precedence(compiler, (prec + 1).into())?;
    match typ.kind {
        TType::PLUS => Add {}.write(&mut compiler.current_chunk(), typ.src),
        TType::MINUS => Sub {}.write(&mut compiler.current_chunk(), typ.src),
        TType::SLASH => Div {}.write(&mut compiler.current_chunk(), typ.src),
        TType::STAR => Mult {}.write(&mut compiler.current_chunk(), typ.src),
        TType::BANG_EQUAL => NotEqual {}.write(&mut compiler.current_chunk(), typ.src),
        TType::EQUAL_EQUAL => EqualEqual {}.write(&mut compiler.current_chunk(), typ.src),
        TType::LESS => Less {}.write(&mut compiler.current_chunk(), typ.src),
        TType::LESS_EQUAL => LessOrEq {}.write(&mut compiler.current_chunk(), typ.src),
        TType::GREATER => {
            LessOrEq {}.write(&mut compiler.current_chunk(), typ.src.clone());
            Not {}.write(&mut compiler.current_chunk(), typ.src)
        }
        TType::GREATER_EQUAL => {
            Less {}.write(&mut compiler.current_chunk(), typ.src.clone());
            Not {}.write(&mut compiler.current_chunk(), typ.src)
        }
        _ => panic!("bad typ"),
    };
    Ok(())
}

fn string(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    if let TType::STRING(str) = &compiler.scanner.previous().kind {
        emit_const(compiler,
                   Value::String(str.clone()),
                   compiler.scanner.previous().src.clone());
        Ok(())
    } else {
        Err(CompilerError::new(format!("Expected to find a string but found a {}", &compiler.scanner.previous().kind),
                               compiler.scanner.previous().src.clone()))
    }
}

fn literal(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    match compiler.scanner.previous().kind {
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

#[derive(PartialEq, Debug)]
pub enum Resolution {
    Local(u8),
    Closed(u8),
    Global,
}

fn resolve_local(compiler: &mut Compiler, name: &Symbol) -> Resolution {
    let mut flat: Vec<Local> = vec![];
    for scope in compiler.scopes.iter() {
        for local in scope.locals.iter() {
            flat.push(local.clone())
        }
    }
    for (idx, local) in flat.iter().enumerate().rev() {
        if &local.name == name {
            return Resolution::Local(idx as u8);
        }
    }
    Resolution::Global
}

fn get_or_set<Get: OpU8, Set: OpU8>(compiler: &mut Compiler, can_assign: bool, name: &Symbol, idx: u8) -> Result<(), CompilerError> {
    if can_assign && matches(compiler, TType::EQUAL.id())? {
        expression(compiler, can_assign)?;
        Set::emit_u8(compiler, idx);
    } else {
        Get::emit_u8(compiler, idx);
    }
    Ok(())
}

fn named_variable(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let name = if let TType::IDENTIFIER(str) = &compiler.scanner.previous().kind.clone() { str.clone() } else { panic!("Compiler error"); };
    let prev = compiler.scanner.previous().src.clone();
    match resolve_local(compiler, &name) {
        Resolution::Local(local_idx) => {
            get_or_set::<GetLocal, SetLocal>(compiler, can_assign, &name, local_idx)?;
        }
        Resolution::Closed(up_val_idx) => {
            // add_up_value(compiler, up_val_idx, true);
            // get_or_set::<GetUpValue, SetUpValue>(compiler, can_assign, &name, up_val_idx)?;
        }
        Resolution::Global => {
            let const_ref = identifier_constant(name.clone(), &mut compiler.current_chunk())?;
            get_or_set::<GetGlobal, SetGlobal>(compiler, can_assign, &name, const_ref.idx)?;
        }
    }
    Ok(())
}

fn consume(compiler: &mut Compiler, typ: TTypeId, message: &str) -> Result<(), CompilerError> {
    if compiler.scanner.current().kind.id() == typ {
        advance(compiler)?;
        return Ok(());
    }
    error_at_current(compiler, message)
}

fn get_rule<'a>(map: &'a HashMap<u32, Rule>, typ: &TType) -> &'a Rule {
    map.get(&typ.id()).unwrap()
}

fn advance(compiler: &mut Compiler) -> Result<(), CompilerError> {
    swap(&mut compiler.scanner.current(), &mut compiler.scanner.previous());
    loop {
        compiler.scanner.scan_token();
        match &compiler.scanner.current().kind.clone() {
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
    if compiler.panic_mode {
        return Ok(());
    }
    compiler.panic_mode = true;
    let src = if current { compiler.scanner.current().src.clone() } else { compiler.scanner.previous().src.clone() };
    Err(CompilerError::new(message.to_string(), src))
}