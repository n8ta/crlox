extern crate core;

use std::io::Read;
use std::process::exit;
use crate::chunk::Chunk;
use crate::source_ref::SourceRef;
use crate::vm::{VM};
use crate::compiler::Compiler;
use crate::symbolizer::{Symbol, Symbolizer};
use crate::value::Value;

mod value;
mod func;
mod ops;
mod source_ref;
mod chunk;
mod vm;
mod scanner;
mod compiler;
mod trie;
mod symbolizer;
mod e2e_tests;
mod native_func;
mod debug;
mod closure;
mod class;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let path = if let Some(path) = args.get(1) {
        path
    } else {
        println!("Usage: ./rclox [source_file.lox]");
        exit(-1);
    };
    let dump_bytecode = if let Some(flag) = args.get(2) {
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
    if let Err(err) =  file.read_to_string(&mut contents) {
        eprintln!("Unable to read file @ '{}'", path);
        eprintln!("Error: {}", err);
        exit(-1);
    }

    let symbolizer = Symbolizer::new();

    let func = match Compiler::compile(contents, symbolizer.clone()) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("{}", err);
            exit(-1);
        },
    };

    if dump_bytecode {
        println!("Op size is : {}B\n", core::mem::size_of::<crate::ops::Op>());
        println!("{:?}", func);
        // return;
    }



    let res = VM::interpret(func, symbolizer.clone());
    match res {
        Err(r) => eprintln!("{}", r),
        _ => {},
    }
}