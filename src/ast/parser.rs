use std::rc::Rc;
use crate::scanner::{IDENTIFIER_TTYPE_ID, NUMBER_TTYPE_ID, STRING_TTYPE_ID, Token, TType, TTypeId};
use crate::source_ref::Source;
use crate::value::Value;
use crate::ast::parser_func::ParserFunc;
use crate::ast::types::{BinOp, Expr, ExprInContext, ExprResult, ExprTy, LogicalOp, ParserError, Stmt, Tokens, UnaryOp};
use crate::{SourceRef, Symbol};

pub fn parse(tokens: Tokens, source: Rc<Source>) -> Result<PStmt, ParserError> {
    let mut parser: Parser = Parser::new(tokens, source);
    let v = parser.parse()?;
    Ok(v)
}

type PFunc = ParserFunc<Symbol, Symbol>;
type PStmt = Stmt<Symbol, Symbol, PFunc>;
type PExpr = Expr<Symbol, Symbol, PFunc>;
type PExprTy = ExprTy<Symbol, Symbol, PFunc>;
type PExprResult = ExprResult<Symbol, Symbol, PFunc>;

fn mk_expr(expr: PExpr, context: SourceRef) -> PExprTy {
    Box::new(ExprInContext::new(expr, context))
}


#[allow(dead_code)]
pub(crate) struct Parser {
    tokens: Tokens,
    current: usize,
    source: Rc<Source>,
}

impl Parser {
    pub(crate) fn new(tokens: Tokens, source: Rc<Source>) -> Parser {
        Parser { tokens, current: 0, source }
    }

    fn parse(&mut self) -> Result<PStmt, ParserError> {
        let mut stmts: Vec<PStmt> = vec![];
        while !self.is_at_end() {
            stmts.push(self.declaration()?)
        }
        Ok(Stmt::Block(Box::new(stmts,), 0))
    }

    fn check(&mut self, typ: TTypeId) -> bool {
        if self.is_at_end() {
            false
        } else {
            typ == self.peek().kind.id()
        }
    }

    fn consume(&mut self, typ: TTypeId, message: &str) -> Result<Token, ParserError> {
        if self.check(typ.clone()) { return Ok(self.advance()); }
        Err(ParserError::new(
            format!("{} - didn't find a {} as expected. Found a {}",
                    message,
                    typ,
                    &self.peek().kind.tname()),
            self.tokens[self.current].src.clone()))
    }

    fn synchronize(&mut self) {
        self.advance();
        while !self.is_at_end() {
            if self.previous().is_some() && self.previous().unwrap().kind.id() == TType::Semicolon.id() {
                return;
            }

            // Token::CLASS | Token::FUN | Token::VAR | Token::FOR | Token::IF | Token::WHILE | Token::PRINT | Token::RETURN
            match self.peek().kind {
                TType::Class | TType::Fun | TType::Var | TType::For | TType::If | TType::While | TType::Print | TType::Return => {
                    return;
                }
                _ => {}
            }
            self.advance();
        }
    }

    fn matches(&mut self, tokens: Vec<TTypeId>) -> bool {
        let tkn = match self.tokens.get(self.current) {
            None => return false,
            Some(t) => &t.kind,
        };
        for expected in tokens.iter() {
            if *expected == tkn.id() {
                self.advance();
                return true;
            }
        }
        false
    }

    fn previous(&self) -> Option<Token> {
        if self.current == 0 {
            return None;
        }
        Some(self.tokens[self.current - 1].clone())
    }

    fn peek(&self) -> Token {
        return self.tokens[self.current].clone();
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.current].kind.id() == TType::EOF.id()
    }

    fn advance(&mut self) -> Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous().unwrap()
    }

    fn err(&self, msg: String) -> ParserError {
        ParserError::new(msg, self.tokens[self.current].src.clone())
    }

    fn declaration(&mut self) -> Result<PStmt, ParserError> {
        if self.matches(vec![TType::Var.id()]) {
            match self.variable_declaration() {
                Ok(stmt) => Ok(stmt),
                Err(err) => {
                    self.synchronize();
                    Err(err)
                }
            }
        } else if self.matches(vec![TType::Class.id()]) {
            self.class()
        } else if self.matches(vec![TType::Fun.id()]) {
            self.function().and_then(|f| Ok(Stmt::Function(f)))
        } else {
            match self.statement() {
                Ok(stmt) => Ok(stmt),
                Err(msg) => {
                    self.synchronize();
                    Err(msg)
                }
            }
        }
    }

    fn class(&mut self) -> Result<PStmt, ParserError> {
        // let name_in_context = self.consume(IDENTIFIER_TTYPE_ID, "Class should be followed by name");
        // let name = if let TType::Identifier(sym) = name_in_context.unwrap().kind {
        //     sym
        // } else {
        //     panic!("Compiler bug");
        // };
        //
        // self.consume(TType::LeftBrace.id(), "Expected a '{' before class body")?;
        // let mut methods = vec![];
        // while !self.check(TType::RightBrace.id()) && !self.is_at_end() {
        //     methods.push(self.function()?);
        // }
        // self.consume(TType::LeftBrace.id(), "Expected a '}' after class body")?;
        //
        todo!("class")
        // Ok(Stmt::Class(Class::new(name)))
    }

    fn function(&mut self) -> Result<ParserFunc<Symbol, Symbol>, ParserError> {
        let name_in_context = self.consume(IDENTIFIER_TTYPE_ID, "Expected a function name")?;
        let name = if let TType::Identifier(str) = name_in_context.kind { str } else {
            return Err(self.err(format!("Expected a function name")));
        };
        self.consume(TType::LeftParen.id(), "Expected `(` after function name")?;
        let mut params_untyped: Vec<Token> = vec![];
        let mut first = true;
        while !self.check(TType::RightParen.id()) {
            if !first {
                self.consume(TType::Comma.id(), "Expected a `,` between function arguments")?;
            }
            first = false;
            if params_untyped.len() >= 255 {
                return Err(self.err(format!("Cannot have more than 255 parameters.")));
            };
            params_untyped.push(self.consume(IDENTIFIER_TTYPE_ID, "Expected parameter name of function arg")?);
        }

        // TODO: Make less shit
        let mut params: Vec<Symbol> = vec![];
        for param in params_untyped {
            if let TType::Identifier(str) = param.kind {
                params.push(str)
            } else {
                panic!("This shouldn't happen");
            }
        }

        self.consume(TType::RightParen.id(), "Expected a ')' after function parameters")?;
        let lbrace = self.consume(TType::LeftBrace.id(), "Expected a '{' after a function declaration")?;

        let mut context = name_in_context.src.merge(&lbrace.src);

        let body =
            if let (Stmt::Block(blk, _), rbrace) = self.block()? {
                context = context.merge(&rbrace);
                *blk
            } else {
                panic!("block() didn't return a block");
            };
        Ok(ParserFunc::new(name,
                           params,
                           Stmt::Block(Box::new(body), 0),
                           name_in_context.src.clone(),
                           context,
        ))
    }

    fn variable_declaration(&mut self) -> Result<PStmt, ParserError> {
        let name = self.consume(IDENTIFIER_TTYPE_ID, "Expect variable name.").unwrap();
        let init = if self.matches(vec![TType::Eq.id()]) {
            let exp = self.expression()?;
            mk_expr(exp.expr, exp.context)
        } else {
            return Err(ParserError::new(format!("Variable must be initiailized"), name.src.clone()));
        };
        self.consume(TType::Semicolon.id(), "Expected ';' after variable declaration")?;
        if let TType::Identifier(str) = name.kind {
            return Ok(Stmt::Variable(str.clone(), init,  name.src));
        }
        Err(self.err(format!("FAILED didnt find a IDENT where expected")))
    }

    fn or(&mut self) -> PExprResult {
        let mut expr = self.and()?;
        while self.matches(vec![TType::Or.id()]) {
            let op = LogicalOp::new(self.previous().unwrap());
            let right = self.and()?;
            let cont = expr.context.merge(&right.context);
            expr = Box::new(ExprInContext::new(Expr::Logical(expr, op, right), cont));
        }
        Ok(expr)
    }

    fn and(&mut self) -> PExprResult {
        let mut expr = self.equality()?;
        while self.matches(vec![TType::And.id()]) {
            let op = LogicalOp::new(self.previous().unwrap());
            let right = self.equality()?;
            let context = expr.context.merge(&right.context);
            expr = Box::new(ExprInContext::new(Expr::Logical(expr, op, right), context));
        }
        Ok(expr)
    }

    fn assignment(&mut self) -> PExprResult {
        let expr = self.or()?;

        if self.matches(vec![TType::Eq.id()]) {
            let _eq = self.previous().unwrap();
            let value: PExprTy = self.assignment()?;

            if let Expr::Variable(var) = expr.expr {
                return Ok(mk_expr(Expr::Assign(var, value.clone()),
                                  expr.context.merge(&value.context)));
            } else if let Expr::Get(get_expr, field) = expr.expr {
                return Ok(mk_expr(Expr::Set(get_expr, field, value),
                                  expr.context));
            } else {
                return Err(self.err(format!("Invalid assignment target:\n{}", expr.context)));
            }
        }
        Ok(expr)
    }


    pub fn expression(&mut self) -> PExprResult {
        self.assignment()
    }

    fn statement(&mut self) -> Result<PStmt, ParserError> {
        if self.matches(vec![TType::Print.id()]) {
            self.print_statement()
        } else if self.matches(vec![TType::While.id()]) {
            self.while_statement()
        } else if self.matches(vec![TType::LeftBrace.id()]) {
            self.block().and_then(|s| Ok(s.0))
        } else if self.matches(vec![TType::If.id()]) {
            self.if_statement()
        } else if self.matches(vec![TType::For.id()]) {
            self.for_statement()
        } else if self.matches(vec![TType::Return.id()]) {
            self.return_statement()
        } else {
            self.expression_statement()
        }
    }

    fn return_statement(&mut self) -> Result<PStmt, ParserError> {
        let mut value = None;
        if !self.check(TType::Semicolon.id()) {
            value = Some(self.expression()?);
        }
        self.consume(TType::Semicolon.id(), "Expected a ';' after a return statement")?;
        Ok(Stmt::Return(value))
    }

    fn while_statement(&mut self) -> Result<PStmt, ParserError> {
        self.consume(TType::LeftParen.id(), "Expected '(' after 'while'")?;
        let expr = self.expression()?;
        self.consume(TType::RightParen.id(), "Expected ')' after while condition")?;
        let body = self.statement()?;
        Ok(Stmt::While(expr, Box::new(body)))
    }

    fn if_statement(&mut self) -> Result<PStmt, ParserError> {
        self.consume(TType::LeftParen.id(), "Expected '(' after 'if'")?;
        let test = self.expression()?;
        self.consume(TType::RightParen.id(), "Expected ')' after `if (... ")?;
        let if_branch = self.statement()?;
        let mut else_branch: Option<Box<PStmt>> = None;
        if self.matches(vec![TType::Else.id()]) {
            else_branch = Some(Box::new(self.statement()?));
        }
        Ok(Stmt::If(test, Box::new(if_branch), else_branch))
    }

    fn for_statement(&mut self) -> Result<PStmt, ParserError> {
        self.consume(TType::LeftParen.id(), "Expected a '(' after a for loop")?;
        let init: Option<PStmt> = if self.matches(vec![TType::Semicolon.id()]) {
            None
        } else if self.matches(vec![TType::Var.id()]) {
            Some(self.variable_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        let mut condition: Option<PExprTy> = None;
        if !self.check(TType::Semicolon.id()) {
            condition = Some(self.expression()?);
        }
        self.consume(TType::Semicolon.id(), "Expect ';' after for loop condition.")?;
        let mut increment: Option<PExprTy> = None;
        if !self.check(TType::RightParen.id()) {
            increment = Some(self.expression()?);
        }
        self.consume(TType::RightParen.id(), "Expected ')' after for loop")?;
        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::Block(Box::new(vec![body, Stmt::Expr(increment)]), 0);
        }
        let src = self.tokens[self.current].src.clone();
        if condition.is_none() {
            condition = Some(Box::new(ExprInContext::new(Expr::Literal(Value::Bool(true)), src)));
        }
        body = Stmt::While(condition.unwrap(), Box::new(body));
        if let Some(init) = init {
            body = Stmt::Block(Box::new(vec![init, body]), 0)
        }
        Ok(body)
    }

    fn block(&mut self) -> Result<(PStmt, SourceRef), ParserError> {
        let mut stmts: Vec<PStmt> = vec![];
        while !self.check(TType::RightBrace.id()) && !self.is_at_end() {
            stmts.push(self.declaration()?)
        }
        let rbrace = self.consume(TType::RightBrace.id(), "Expected block to end with an '}'.")?;
        Ok((Stmt::Block(Box::new(stmts), 0), rbrace.src))
    }

    fn print_statement(&mut self) -> Result<PStmt, ParserError> {
        let value: PExprTy = self.expression()?;
        self.consume(TType::Semicolon.id(), "Expected ';' after print value.")?;
        Ok(Stmt::Print(value))
    }

    fn expression_statement(&mut self) -> Result<PStmt, ParserError> {
        let value: PExprTy = self.expression()?;
        self.consume(TType::Semicolon.id(), "Expected ';' after expression.")?;
        Ok(Stmt::Expr(value))
    }

    fn equality(&mut self) -> PExprResult {
        let mut expr = self.comparison()?;
        while self.matches(vec![TType::BangEq.id(), TType::EqEq.id()]) {
            let operator = BinOp::new(self.previous().unwrap());
            let right = self.comparison()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn comparison(&mut self) -> PExprResult {
        let mut expr = self.term()?;
        while self.matches(vec![TType::Greater.id(), TType::GreaterEq.id(), TType::Less.id(), TType::LessEq.id()]) {
            let operator = BinOp::new(self.previous().unwrap());
            let right = self.term()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right),
                           context)
        }
        Ok(expr)
    }

    fn term(&mut self) -> PExprResult {
        let mut expr: PExprTy = self.factor()?;
        while self.matches(vec![TType::Minus.id(), TType::Plus.id()]) {
            let operator = BinOp::new(self.previous().unwrap());
            let right = self.factor()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context)
        }
        Ok(expr)
    }

    fn factor(&mut self) -> PExprResult {
        let mut expr = self.unary()?;
        while self.matches(vec![TType::Slash.id(), TType::Star.id()]) {
            let operator = BinOp::new(self.previous().unwrap());
            let right = self.unary()?;
            let context = expr.context.clone().merge(&right.context);
            expr = mk_expr(Expr::Binary(expr, operator, right), context);
        }
        Ok(expr)
    }

    fn unary(&mut self) -> PExprResult {
        if self.matches(vec![TType::Bang.id(), TType::Minus.id()]) {
            let operator = UnaryOp::new(self.previous().unwrap());
            let right = self.unary()?;
            let context = self.previous().unwrap().src.merge(&right.context);
            return Ok(mk_expr(Expr::Unary(operator, right), context));
        }
        // Ok(self.primary()?)
        Ok(self.call()?)
    }

    fn call(&mut self) -> PExprResult {
        let mut expr = self.primary()?;
        loop {
            if self.matches(vec![TType::LeftParen.id()]) {
                expr = self.finish_call(expr)?;
            } else if self.matches(vec![TType::Dot.id()]) {
                let context = expr.context.clone();
                let ident = self.consume(IDENTIFIER_TTYPE_ID, "Expected an identifier after a '.'")?;
                if let TType::Identifier(name) = &ident.kind {
                    let merged_context = ident.src.merge(&context);
                    expr = Box::new(ExprInContext::new(Expr::Get(expr, name.clone()), merged_context));
                } else {
                    panic!("Compiler error");
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: PExprTy) -> PExprResult {
        let mut args: Vec<PExprTy> = vec![];
        let mut context = callee.context.clone();
        let mut first = true;
        if !self.check(TType::RightParen.id()) {
            while first || self.matches(vec![TType::Comma.id()]) {
                first = false;
                if args.len() >= 255 {
                    return Err(self.err("Can't have more than 255 args".to_string()));
                }
                let expr = self.expression()?;
                context = context.merge(&expr.context);
                args.push(expr);
            }
        }
        self.consume(TType::RightParen.id(), "Expected a ')' after function arguments=")?;
        Ok(Box::new(ExprInContext::new(Expr::Call(callee, args), context)))
    }

    fn primary(&mut self) -> PExprResult {
        if self.matches(vec![TType::False.id(), TType::True.id()]) {
            let last = self.previous().unwrap().kind == TType::True;
            return Ok(mk_expr(Expr::Literal(Value::Bool(last)), self.previous().unwrap().src));
        }
        if self.matches(vec![TType::Nil.id()]) {
            return Ok(mk_expr(Expr::Literal(Value::Nil),
                              self.previous().unwrap().src));
        }
        if self.matches(vec![NUMBER_TTYPE_ID]) {
            // These ifs should always be true (based on the match above). This is an awkward intersection of javas
            // (language used in the book) Object base class and rust's type system.
            // I couldn't find a good way to write a generic match that return a specific variant
            // of the Token enum to avoid the second type check. (Enum variants aren't types, very sad)
            if let TType::Number(num) = self.previous().unwrap().kind {
                return Ok(mk_expr(Expr::Literal(Value::Num(num.num)), self.previous().unwrap().src));
            }
            panic!("This path shouldn't happen!");
        }
        if self.matches(vec![TType::LeftParen.id()]) {
            let expr = self.expression()?;
            self.consume(TType::RightParen.id(), "Expected ')' after expression.")?;
            return Ok(mk_expr(Expr::Grouping(expr), self.previous().unwrap().src));
        }
        if self.matches(vec![STRING_TTYPE_ID]) {
            let str = self.previous().expect("Compiler error");
            if let TType::String(sym) = str.kind {
                return Ok(mk_expr(Expr::Literal(Value::String(sym)), self.previous().unwrap().src));
            }
            panic!("Also shouldn't happen. something wrong with matches function");
        }
        // TODO: THIS / SUPER
        // if self.matches(vec![TType::This.id()]) {
        //     let prev = self.previous().unwrap();
        //     if let (TType::This, src) = (prev.kind, prev.src) {
        //         return Ok(mk_expr(Expr::This, src));
        //     } else {
        //         panic!("compiler error");
        //     }
        // }
        // if self.matches(vec![TType::Super.id()]) {
        //     let prev = self.previous().unwrap();
        //     if let (TType::Super, src) = (prev.kind, prev.src) {
        //         self.consume(TType::Dot.id(), "Expected a '.' after the super keyword")?;
        //         let method = self.consume(IDENTIFIER_TTYPE_ID, "Expected to see a method name after super")?;
        //         if let TType::Identifier(method) = method.kind {
        //             return Ok(mk_expr(Expr::Super(method), src));
        //         } else {
        //             panic!("Compiler error")
        //         }
        //     } else {
        //         panic!("compiler error");
        //     }
        // }


        if self.matches(vec![IDENTIFIER_TTYPE_ID]) {
            if let TType::Identifier(id) = self.previous().unwrap().kind {
                return Ok(mk_expr(Expr::Variable(id),  self.previous().unwrap().src));
            }
            panic!("Here be dragons");
        }
        Err(self.err(format!("Failed to match any expression for `{}`", self.tokens[self.current].src.source())))
    }
}