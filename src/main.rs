extern crate core;

use std::io::Read;
use std::process::exit;
use std::rc::Rc;
use crate::source_ref::{Source, SourceRef};
use crate::symbolizer::{Symbol, Symbolizer};

mod value;
// mod func;
// mod ops;
mod source_ref;
// mod chunk;
// mod vm;
mod scanner;
mod trie;
mod symbolizer;
// mod e2e_tests;
mod native_func;
mod debug;
// mod closure;
mod ast;
// mod compiler_ast;
mod uniq_pass;
mod printable_error;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = if let Some(path) = args.get(1) {
        path
    } else {
        println!("Usage: ./rclox [source_file.lox]");
        exit(-1);
    };
    let _dump_bytecode = if let Some(flag) = args.get(2) {
        flag == "--dump"
    } else { false };

    let mut file = match std::fs::File::open(path) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("Unable to open file @ '{}'", path);
            eprintln!("Error: {}", err);
            exit(-1)
        }
    };
    let mut contents: String = String::new();
    if let Err(err) = file.read_to_string(&mut contents) {
        eprintln!("Unable to read file @ '{}'", path);
        eprintln!("Error: {}", err);
        exit(-1);
    }

    let symbolizer = Symbolizer::new();
    let source = Rc::new(Source::new(contents));

    let tokens = crate::ast::scanner::scanner(source.clone(), symbolizer.clone()).unwrap();
    let ast = crate::ast::parser::parse(tokens, source).unwrap();

    let uniq_ast = match crate::uniq_pass::uniq(ast, symbolizer)
    {
        Ok(uniq_ast) => uniq_ast,
        Err(err) => {
            eprintln!("{}", err);
            panic!("Uniq error");
        }
    };



    // println!("uniq: {}", uniq_ast);
    // let rast = match crate::resolver::resolve(ast, &mut symbolizer) {
    //     Ok(r) => r,
    //     Err(e) => {
    //         eprintln!("resolver error");
    //         panic!("resolver")
    //     }
    // };

    // let func = crate::compiler_ast::compile(&ast, symbolizer.clone());
    // let func = match Compiler::compile(contents, symbolizer.clone()) {
    //     Ok(c) => c,
    //     Err(err) => {
    //         eprintln!("{}", err);
    //         exit(-1);
    //     },
    // };
    //
    // if dump_bytecode {
    //     println!("Op size is : {}B\n", core::mem::size_of::<crate::ops::Op>());
    //     println!("{:?}", func);
    // }
    //
    //
    //
    // let res = VM::interpret(func, symbolizer.clone());
    // match res {
    //     Err(r) => eprintln!("{}", r),
    //     _ => {},
    // }
}