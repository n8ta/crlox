mod types;
mod stack_placement_pass;
mod upvalue_identifier_pass;

pub use types::{VarRef, VarDecl, VarDeclType, UniqSymbol, UniqSymbolizer, ResolvedFunc, FuncScope, UpvalueType, Upvalue};
pub use stack_placement_pass::{VarRefResolved, VarRefResolvedType};

use std::fmt::{Debug, Display, Formatter};
use crate::ast::{Expr, ExprInContext, ExprTy, Stmt, ParserFunc};
use crate::lexer::{SourceRef, Symbol, Symbolizer};
use crate::printable_error::PrintableError;
use crate::resolver::stack_placement_pass::pass;
use crate::resolver::upvalue_identifier_pass::PartialResolver;
use crate::Source;

pub fn resolve(ast: upvalue_identifier_pass::StmtIn, symbolizer: Symbolizer) -> Result<ResolvedFunc<VarRefResolved, VarRefResolved>, PrintableError> {
    let symbolizer = symbolizer.clone();
    let mut uniq_symbolizer = UniqSymbolizer::new(symbolizer);

    let upval_identified_ast = upvalue_identifier_pass::pass(ast, &mut uniq_symbolizer)?;
    let variables_placed_in_stack_ast = stack_placement_pass::pass(upval_identified_ast);
    variables_placed_in_stack_ast
}

#[test]
fn test_plain_text() {
    use std::rc::Rc;
    use crate::lexer::Source;

    fn remove_spaces_eq(expected: &str, actual: &str) {
        let exp: String = expected.chars().filter(|c| *c != ' ' && *c != '\n').collect();
        let act: String = actual.chars().filter(|c| *c != ' ' && *c != '\n').collect();
        if exp != act {
            eprintln!("Expected: \n{}\n\nActual: \n{}", expected, actual);
        }
        assert_eq!(act, exp);
    }

    fn pass(input: &str, exp: &str) {
        println!("{}", input);
        let symbolizer = Symbolizer::new();
        let source = Rc::new(Source::new(input.to_string()));
        let tokens = crate::lexer::scanner(source.clone(), symbolizer.clone()).unwrap();
        let ast = crate::ast::parse(tokens, source).unwrap();
        let uniq_ast = crate::resolver::resolve(ast, symbolizer).unwrap();
        let uniq_ast_str = format!("{}", uniq_ast);

        remove_spaces_eq(exp, &uniq_ast_str);
    }

    pass("var x = 0; print x;",
         "fun root[]() {1: var l0 = 0; print l0; }");
    pass("var x = 0; {{{print x; }}}", "fun root[]() {1:  var l0 = 0; {0:{0:{0:print l0;}}}     }");
    pass("var x = 0; { var x = 1; {{ print x; }}}",
         "fun root[] () {1:   var l0 = 0; {1: var l1 = 1;  {0:{0: print l1; }} } }");
    pass("var x = 0; { var x = 1; } print x;",
         "fun root[]() {1:   var l0 = 0; {1: var l1 = 1; } print l0;   }");

    pass("var x = 0; { var x = 1; print x; } { var x = 2; print x; } print x;",
         "fun root[]() {1: varl0 = 0;  {1: var l1 = 1; print l1; } {1: varl1 = 2; print l1; } print l0;   }");
    pass("               var x = 0; { var x = 1; var z = 3; } print x;",
         "fun root[]() {1: var l0 = 0; {2: var l1 = 1; var l2 = 3; } print l0;  }");
    pass("                 var x = 0; { var x = 1; var z = 3; print x; print z; print x; } print x;",
         "fun root[](){1: var l0 = 0; {2: var l1 = 1; var l2 = 3; print l1; print l2; print l1; } print l0;    }");
    pass("                   var x = 0; var y = x + x;",
         "fun root[]() {2:   var l0 = 0; var l1 = l0 + l0; } ");
    pass("var x = 0; var y = x + x; print y;",
         "fun root[]() {2:  var l0 = 0; var l1 = l0 + l0; print l1;  }");
    pass("                      { var x = 123; var y = x; var z = y + x; }",
         "fun root[]() {0:     {3: var l0 = 123;\n var l1 = l0;\n var l2 = l1 + l0; }     }");
    pass("{                     var x = 123; var y = x; var z = ((y)) + (x+2); }",
         "fun root[](){0:    {3:  var l0 = 123; var l1 = l0; var l2 = ((l1)) + (l0+2); }    }");
    pass("fun test() { print 1; }",
         "fun root[]() {1:    fun l0[]() {0: print 1; }   }");
    pass("fun test() { print test; } ",
         "fun root[]() {1:  fun l0[]() {0:     print l0;  }}");
    pass("fun test() { var test = 0; print test; }",
         "fun root[]() {1:  funl0[]() {1:     var l1 = 0; print l1;    }}");
    pass("fun test() { var x = 123; fun testinner() { return x; } }",
         "fun root[]() {1:\
                        fun l0[2r]() {1: var u0 = 123; fun l1[2c0]() {0: return u0;  }     }}");
    pass("var aa = 11; fun test() { var x = 123; fun testinner() { return x + aa; } }",
         "funroot[1r](){1:varu0=11;funl0[3r,1c0](){1:varu0=123;funl1[3c0,1c1](){0:returnu0+u1;}}}");
    // pass("fun ii(a, b, c) { \
    //     print c + b + a + ii;\
    //  }",
    //      "funl0[]() {1: fun l1[](l2, l3, l4) {0: \
    //      print l3 + l2 + l1 + l0;\
    // }}");
    // pass("\
    // fun ii(a, b, c) { \
    //     {{{ print c + b + a; }}}\
    //     }",
    //      "funl0[]() {1: \
    //      \
    //      fun l1[](l2, l3, l4) {0:\
    //      {0:{0:{0: print l3 + l2 + l1; }}}\
    //       }}");
    // pass("fun ii(a, b, c) { \
    // var a = 0; \
    // print c + b + a; \
    // }",
    //      "funl0[]() {1: fun l1[](l2, l3, l4) {1: \
    //      var l5 = 0; \
    //      print l3 + l2 + l4; \
    //      }}");
    // pass("fun makeClosure() { \
    //  var local = \"local\";\
    //  fun closure() { print local; } \
    //  return closure; \
    //  } \
    //  var closure = makeClosure(); \
    //  closure();",
    //      "funl0[](){2:\
    //      funl1[2r](){1:\
    //      varu2=local;\
    //         funl3[2c0](){0:printu0;}\
    //       returnl1;}\
    //       varl4=l0();l1();}");
}