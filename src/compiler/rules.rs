use std::collections::HashMap;
use crate::Compiler;
use crate::compiler::{and_, binary, call, CompilerError, grouping, literal, number, or_, string, unary, variable};
use crate::scanner::{ERROR_TTYPE_ID, IDENTIFIER_TTYPE_ID, Num, STRING_TTYPE_ID, TType, TTypeId};

#[repr(u8)]
#[derive(Clone, Copy, PartialOrd, PartialEq)]
pub enum Precedence {
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


type CompilerFnTy = fn(&mut Compiler, bool) -> Result<(), CompilerError>;

pub struct Rules {
    rules: HashMap<TTypeId, Rule>
}
impl Rules {
    pub fn new() -> Rules {
        let mut rules = HashMap::new();
        {
            rules.insert(TType::LeftParen.id(), Rule::new(Some(grouping), Some(call), Precedence::CALL));
            rules.insert(TType::RightParen.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::LeftBrace.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::RightBrace.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Comma.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Dot.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Minus.id(), Rule::new(Some(unary), Some(binary), Precedence::TERM));
            rules.insert(TType::Plus.id(), Rule::new(None, Some(binary), Precedence::TERM));
            rules.insert(TType::Semicolon.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Slash.id(), Rule::new(None, Some(binary), Precedence::FACTOR));
            rules.insert(TType::Star.id(), Rule::new(None, Some(binary), Precedence::FACTOR));
            rules.insert(TType::Bang.id(), Rule::new(Some(unary), None, Precedence::NONE));
            rules.insert(TType::BangEq.id(), Rule::new(None, Some(binary), Precedence::EQUALITY));
            rules.insert(TType::Eq.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EqEq.id(), Rule::new(None, Some(binary), Precedence::EQUALITY));
            rules.insert(TType::Greater.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::GreaterEq.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::Less.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(TType::LessEq.id(), Rule::new(None, Some(binary), Precedence::COMPARISON));
            rules.insert(IDENTIFIER_TTYPE_ID, Rule::new(Some(variable), None, Precedence::NONE));
            rules.insert(STRING_TTYPE_ID, Rule::new(Some(string), None, Precedence::NONE));
            rules.insert(TType::Number(Num { num: 0.0 }).id(), Rule::new(Some(number), None, Precedence::NONE));
            rules.insert(TType::Class.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Else.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::False.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::For.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Fun.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::If.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Nil.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::Or.id(), Rule::new(None, Some(or_), Precedence::OR));
            rules.insert(TType::And.id(), Rule::new(None, Some(and_), Precedence::AND));
            rules.insert(TType::Print.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Return.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::Super.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::This.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::True.id(), Rule::new(Some(literal), None, Precedence::NONE));
            rules.insert(TType::Var.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::While.id(), Rule::new(None, None, Precedence::NONE));
            rules.insert(ERROR_TTYPE_ID, Rule::new(None, None, Precedence::NONE));
            rules.insert(TType::EOF.id(), Rule::new(None, None, Precedence::NONE));
        }
        Rules { rules }
    }
    pub fn get_rule(&self, typ: &TType) -> &Rule {
        self.rules.get(&typ.id()).unwrap()
    }

}

#[derive(Clone)]
pub struct Rule {
    pub prefix: Option<CompilerFnTy>,
    pub infix: Option<CompilerFnTy>,
    pub precedence: Precedence,
}

impl Rule {
    fn new(prefix: Option<CompilerFnTy>, infix: Option<CompilerFnTy>, precedence: Precedence) -> Rule { Rule { prefix, infix, precedence } }
}