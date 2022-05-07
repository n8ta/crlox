#[allow(dead_code)]

use std::io::Read;
use std::process::exit;
use std::rc::Rc;
use crate::source_ref::{Source, SourceRef};
use crate::symbolizer::{Symbol, Symbolizer};
use crate::vm::VM;

mod value;
mod source_ref;
mod scanner;
// mod trie;
mod symbolizer;
mod e2e_tests;
mod native_func;
mod debug;
mod closure;
mod ast;
mod compiler_ast;
mod resolver;
mod printable_error;
mod func;
mod ops;
mod vm;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = if let Some(path) = args.get(1) {
        path
    } else {
        println!("Usage: ./rclox [source_file.lox]");
        exit(-1);
    };

    let flags: Vec<&String> = args.iter().skip(2).collect();
    let dump_bytecode = flags.iter().find(|flag| **flag == "--dump").is_some();
    let dump_resolved = flags.iter().find(|flag| **flag == "--resolved").is_some();


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

    let ast = match crate::resolver::resolve(ast, symbolizer.clone())
    {
        Ok(uniq_ast) => uniq_ast,
        Err(err) => {
            eprintln!("{}", err);
            panic!("Uniq error");
        }
    };

    if dump_resolved {
        println!("{}", ast);
    }

    let bytecode = crate::compiler_ast::compile(ast);

    if dump_bytecode {
        println!("Op size is : {}B\n", core::mem::size_of::<crate::ops::Op>());
        println!("{:?}", bytecode);
    }



    let res = VM::interpret(bytecode, symbolizer.clone());
    match res {
        Err(r) => eprintln!("{}", r),
        _ => {},
    }
}