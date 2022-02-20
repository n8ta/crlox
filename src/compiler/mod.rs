mod rules;

use std::fmt::{Display, Formatter};
use std::mem::{swap};
use std::process::exit;
use crate::ops::{OpTrait, Add, Div, EqualEqual, False, Less, LessOrEq, Mult, Nil, Not, NotEqual, Pop, Print, Sub, True, Negate, Const, Ret, SetLocal, GetLocal, RelJump, RelJumpIfFalse, OpJumpTrait, Call, SmallConst, Closure, OpU8, Stack};
use crate::scanner::{IDENTIFIER_TTYPE_ID, Scanner, TType, TTypeId};
use crate::{Chunk, SourceRef, Symbol};
use crate::chunk::Write;
use crate::compiler::rules::{Precedence, Rules};
use crate::func::{Func, FuncType};
use crate::resolver::{Resolution, Resolver};
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


pub struct Compiler {
    src: String,
    pub chunk: Chunk,
    resolver: Resolver,
    rules: Rules,
    scanner: Scanner,
    symbolizer: Symbolizer,
    // enclosing: Option<Box<Compiler>>,
    panic_mode: bool,
    had_error: Option<CompilerError>,
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
        Compiler {
            src,
            chunk: Chunk::new(),
            scanner,
            panic_mode: false,
            had_error: None,
            symbolizer,
            rules: Rules::new(),
            // enclosing: None,
            resolver: Resolver::new(),
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

fn synchronize(compiler: &mut Compiler) -> Result<(), CompilerError> {
    compiler.panic_mode = false;
    loop {
        let token = &compiler.scanner.current().kind;
        if let TType::EOF = token {
            break;
        }
        match token {
            TType::Class |
            TType::Fun | TType::Var | TType::For |
            TType::If | TType::While | TType::Print |
            TType::Return => return Ok(()),
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

fn begin_scope(compiler: &mut Compiler) {
    compiler.resolver.begin_scope();
}

fn end_scope(compiler: &mut Compiler) {
    for _ in compiler.resolver.end_scope() {
        Pop {}.emit(compiler);
    }
}

fn parse_precedence(compiler: &mut Compiler, prec: Precedence) -> Result<(), CompilerError> {
    advance(compiler)?;
    let prefix_rule = compiler.rules.get_rule(&compiler.scanner.previous().kind); // todo: error?
    let prefix_rule = if let Some(func) = prefix_rule.prefix {
        func
    } else {
        let msg = format!("Unexpected token: {}", compiler.scanner.previous().kind);
        return error_at_prev(compiler, &msg);
    };

    let can_assign = prec <= Precedence::ASSIGNMENT;
    prefix_rule(compiler, can_assign)?;

    while (prec as u8) <= (compiler.rules.get_rule(&compiler.scanner.current().kind).precedence as u8) {
        advance(compiler)?;
        let infix = compiler.rules.get_rule(&compiler.scanner.previous().kind).infix.expect("No infix for this rule");
        infix(compiler, false)?;
    }

    if can_assign && matches(compiler, TType::Eq.id())? {
        Err(CompilerError::new(format!("Invalid assignment target"), compiler.scanner.previous().src.clone()))
    } else {
        Ok(())
    }
}

// fn identifier_constant(sym: Symbol, chunk: &mut Chunk) -> Result<Const, CompilerError> {
//     Ok(chunk.add_const(Value::String(sym)))
// }

fn parse_variable(compiler: &mut Compiler, message: &str) -> Result<(Symbol, Const), CompilerError> {
    consume(compiler, IDENTIFIER_TTYPE_ID, message)?;
    if let TType::Identifier(str) = &compiler.scanner.previous().kind.clone() {
        compiler.resolver.declare_variable( str.clone(), compiler.scanner.previous().src.clone())?;
        Ok((str.clone(), compiler.current_chunk().add_const(Value::String(str.clone()))))
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
    while !check(compiler, TType::RightBrace.id()) && !check(compiler, TType::EOF.id()) {
        declaration(compiler, false)?;
    }
    consume(compiler, TType::RightBrace.id(), "Expected '}' after block")
}

fn var_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let (name, _const_ref) = parse_variable(compiler, "Expected to find an identifier after a var")?;
    if matches(compiler, TType::Eq.id())? {
        expression(compiler, can_assign)?;
    } else {
        Nil {}.emit(compiler);
    }
    consume(compiler, TType::Semicolon.id(), "Expect ';' after variable declaration")?;
    compiler.resolver.mark_initialized( name, compiler.scanner.previous().src.clone())?;
    Ok(())
}


fn function(compiler: &mut Compiler, name: Symbol, _can_assign: bool) -> Result<(), CompilerError> {
    consume(compiler, TType::LeftParen.id(), "Expected '(' after function name")?;

    let mut new_compiler = Compiler::new(compiler.src.clone(), compiler.symbolizer.clone(), compiler.scanner.clone());

    let src = new_compiler.scanner.previous().src.clone();
    new_compiler.resolver.declare_variable( name.clone(), src.clone())?;
    new_compiler.resolver.mark_initialized(name.clone(), src.clone())?;
    let mut arity: usize = 0;
    if !(check(&mut new_compiler, TType::RightParen.id())) {
        let mut first = true;
        while matches(&mut new_compiler, TType::Comma.id())? || first {
            first = false;
            arity += 1;
            let (_symbol, _name_const) = parse_variable(&mut new_compiler, "Expected a parameter name here")?;
            new_compiler.resolver.mark_initialized(name.clone(), src.clone())?;
        }
    }
    let arity: u8 = if arity >= (u8::MAX as usize) {
        return Err(CompilerError::new(format!("Cannot have more than {} parameters", u8::MAX), compiler.scanner.previous().src.clone()));
    } else {
        arity as u8
    };
    consume(&mut new_compiler, TType::RightParen.id(), "Expected ')' after parameters")?;
    consume(&mut new_compiler, TType::LeftBrace.id(), "Expected '{' before function body")?;
    block(&mut new_compiler)?;

    end_scope(&mut new_compiler);
    Nil {}.emit(&mut new_compiler);
    Ret {}.emit(&mut new_compiler);

    let completed_chunk = new_compiler.chunk.clone();

    let func_const = compiler.current_chunk().add_const(Value::Func(
        Func::new(name.clone(),
                  arity,
                  FuncType::Function,
                  completed_chunk,
                  0,
        )));

    Closure { idx: func_const.idx }.emit(compiler);
    Ok(())
}

fn fun_declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let (sym, _name_const) = parse_variable(compiler, "Expected to find an identifier after a function")?;
    compiler.resolver.mark_initialized(sym.clone(), compiler.scanner.previous().src.clone())?;
    function(compiler, sym, can_assign)?;
    // func_const.emit(compiler);
    // define_variable(compiler, DefGlobal { idx: name_const.idx })?;
    Ok(())
}

fn declaration(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::Fun.id())? {
        fun_declaration(compiler, can_assign)?;
    } else if matches(compiler, TType::Var.id())? {
        var_declaration(compiler, can_assign)?;
    } else if matches(compiler, TType::Stack.id())? {
        consume(compiler, TType::Semicolon.id(), "Expected a ';' after stack");
        Stack{}.emit(compiler);
    } else {
        statement(compiler, can_assign)?;
    }
    if compiler.panic_mode {
        synchronize(compiler)?;
    }
    Ok(())
}

fn statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::Print.id())? {
        print_statement(compiler, can_assign)
    } else if matches(compiler, TType::Return.id())? {
        return_statement(compiler, can_assign)
    } else if matches(compiler, TType::If.id())? {
        if_statement(compiler, can_assign)
    } else if matches(compiler, TType::While.id())? {
        while_statement(compiler, can_assign)
    } else if matches(compiler, TType::For.id())? {
        for_statement(compiler, can_assign)
    } else if matches(compiler, TType::LeftBrace.id())? {
        begin_scope(compiler);
        block(compiler)?;
        end_scope(compiler);
        Ok(())
    } else {
        expression_statement(compiler, can_assign)
    }
}

fn for_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError>  {
    begin_scope(compiler);
    consume(compiler, TType::LeftParen.id(), "Expected a '(' after a 'for'")?;

    // init
    consume(compiler, TType::Var.id(), "Expected a variable declaration 'var' at start of for loop")?;
    var_declaration(compiler, can_assign);

    let test_target = compiler.current_chunk().len();

    // test
    expression(compiler, false);
    consume(compiler, TType::Semicolon.id(), "Expected a ';' after for loop test condition")?;

    let test_jump_to_end = RelJumpIfFalse{idx:-1};
    let test_jump_to_end_write = test_jump_to_end.emit(compiler);

    Pop{}.emit(compiler);
    let from_test_to_body = RelJump{idx:-1};
    let from_test_to_body_write = from_test_to_body.emit(compiler);


    // increment
    let incr_target = compiler.current_chunk().len() as i16;
    expression(compiler, true)?;
    Pop{}.emit(compiler);

    // Jump from incr to test
    RelJump{idx: ((test_target as i16) - compiler.current_chunk().len() as i16)}.emit(compiler);

    consume(compiler, TType::RightParen.id(), "Expected a ')' after a 'for' init, condition, increment")?;

    // body
    // test_to_body should jump right here before the body
    from_test_to_body.overwrite(compiler.current_chunk(), &from_test_to_body_write);
    consume(compiler, TType::LeftBrace.id(), "Expected a '}' after a 'for ()'")?;
    block(compiler);

    // jump to incr after body
    RelJump{idx: incr_target - (compiler.current_chunk().len() as i16) }.emit(compiler);
    Pop{}.emit(compiler);

    // test jump_to end jumps here, the end
    test_jump_to_end.overwrite(compiler.current_chunk(), &test_jump_to_end_write);


    end_scope(compiler);
    Ok(())

}

fn and_(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    // FIRST
    let jf = RelJumpIfFalse { idx: -1 };
    let jf_write = jf.emit(compiler);
    Pop {}.emit(compiler);
    parse_precedence(compiler, Precedence::AND);
    jf.overwrite(&mut compiler.current_chunk(), &jf_write);
    Ok(())
}

fn or_(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
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
    consume(compiler, TType::Semicolon.id(), "Expected ';' after expression.")?;
    Pop {}.emit(compiler);
    Ok(())
}

fn return_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    if matches(compiler, TType::Semicolon.id())? {
        Nil {}.emit(compiler);
        Ret {}.emit(compiler);
    } else {
        expression(compiler, can_assign)?;
        consume(compiler, TType::Semicolon.id(), "Expected ';' after return value.")?;
        Ret {}.emit(compiler);
    }
    Ok(())
}

fn print_statement(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    expression(compiler, can_assign)?;
    consume(compiler, TType::Semicolon.id(), "Expect ';' after value.")?;
    Print {}.emit(compiler);
    Ok(())
}

fn number(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    if let TType::Number(num) = &compiler.scanner.previous().kind {
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
        TType::Minus => Negate {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Bang => Not {}.write(&mut compiler.current_chunk(), typ.src),
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
    if !matches(compiler, TType::RightParen.id())? {
        let mut first = true;
        while check(compiler, TType::Comma.id()) || first {
            if !first {
                advance(compiler)?;
            }
            first = false;
            expression(compiler, false)?;
            count += 1;
        }
        consume(compiler, TType::RightParen.id(), "Expected ')' after arguments.")?;
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
    consume(compiler, TType::RightParen.id(), "Expected a ')' at the end of a grouping")
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
    consume(compiler, TType::LeftParen.id(), "Expected '(' after if.")?;
    expression(compiler, can_assign)?;
    consume(compiler, TType::RightParen.id(), "Expected ')' after 'if (expression'")?;
    let if_jump = RelJumpIfFalse { idx: -1 };

    let if_jump_write: Write = if_jump.emit(compiler);

    statement(compiler, can_assign)?;

    if matches(compiler, TType::Else.id())? {
        let if_done_jump = RelJump { idx: -1 };
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
    consume(compiler, TType::LeftParen.id(), "Expected '(' after while.")?;
    expression(compiler, can_assign)?;
    consume(compiler, TType::RightParen.id(), "Expected ')' after while expression.")?;
    let exit_jump = RelJumpIfFalse { idx: -1 }; // jump to :done if expression is false
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
    let rule = compiler.rules.get_rule(&typ.kind);
    let prec: u8 = rule.precedence as u8;
    parse_precedence(compiler, (prec + 1).into())?;
    match typ.kind {
        TType::Plus => Add {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Minus => Sub {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Slash => Div {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Star => Mult {}.write(&mut compiler.current_chunk(), typ.src),
        TType::BangEq => NotEqual {}.write(&mut compiler.current_chunk(), typ.src),
        TType::EqEq => EqualEqual {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Less => Less {}.write(&mut compiler.current_chunk(), typ.src),
        TType::LessEq => LessOrEq {}.write(&mut compiler.current_chunk(), typ.src),
        TType::Greater => {
            LessOrEq {}.write(&mut compiler.current_chunk(), typ.src.clone());
            Not {}.write(&mut compiler.current_chunk(), typ.src)
        }
        TType::GreaterEq => {
            Less {}.write(&mut compiler.current_chunk(), typ.src.clone());
            Not {}.write(&mut compiler.current_chunk(), typ.src)
        }
        _ => panic!("bad typ"),
    };
    Ok(())
}

fn string(compiler: &mut Compiler, _can_assign: bool) -> Result<(), CompilerError> {
    if let TType::String(str) = &compiler.scanner.previous().kind {
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
        TType::True => True {}.emit(compiler),
        TType::False => False {}.emit(compiler),
        TType::Nil => Nil {}.emit(compiler),
        _ => panic!("not a literal!"),
    };
    Ok(())
}

fn variable(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    named_variable(compiler, can_assign)
}

fn get_or_set<Get: OpU8, Set: OpU8>(compiler: &mut Compiler, can_assign: bool, _name: &Symbol, idx: u8) -> Result<(), CompilerError> {
    if can_assign && matches(compiler, TType::Eq.id())? {
        expression(compiler, can_assign)?;
        Set::emit_u8(compiler, idx);
    } else {
        Get::emit_u8(compiler, idx);
    }
    Ok(())
}

fn named_variable(compiler: &mut Compiler, can_assign: bool) -> Result<(), CompilerError> {
    let name = if let TType::Identifier(str) = &compiler.scanner.previous().kind.clone() { str.clone() } else { panic!("Compiler error"); };
    let prev = compiler.scanner.previous().src.clone();
    match compiler.resolver.resolve_local( &name, &prev)? {
        Resolution::Local(local_idx) => {
            get_or_set::<GetLocal, SetLocal>(compiler, can_assign, &name, local_idx)?;
        }
        Resolution::Closed(_up_val_idx) => {
            // add_up_value(compiler, up_val_idx, true);
            // get_or_set::<GetUpValue, SetUpValue>(compiler, can_assign, &name, up_val_idx)?;
            todo!("closures");
        }
    }
    Ok(())
}

fn consume(compiler: &mut Compiler, typ: TTypeId, message: &str) -> Result<(), CompilerError> {
    if compiler.scanner.current().kind.id() == typ {
        advance(compiler)?;
        return Ok(());
    }
    error_at_current(compiler, &format!("{}, found a {}", message, compiler.scanner.current().kind))
}


fn advance(compiler: &mut Compiler) -> Result<(), CompilerError> {
    swap(&mut compiler.scanner.current(), &mut compiler.scanner.previous());
    loop {
        compiler.scanner.scan_token();
        match &compiler.scanner.current().kind.clone() {
            TType::Error(msg) => {
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