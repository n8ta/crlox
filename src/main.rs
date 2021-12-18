use std::io::Read;
use std::process::exit;
use std::thread::current;
use crate::chunk::Chunk;
use crate::ops::{Add, Const, Div, Mult, Negate, OpTrait, Ret, Sub};
use crate::source_ref::SourceRef;
use crate::vm::{InterpError, VM};
use crate::compiler::Compiler;
use crate::value::Value;

mod value;
mod ops;
mod source_ref;
mod chunk;
mod vm;
mod scanner;
mod compiler;
mod trie;
mod intern;

#[repr(u8)]
enum Test {
    A,
    B(String),
}

fn main() {

    let args: Vec<String> = std::env::args().collect();
    let path = if let Some(path) = args.get(1) {
        path
    } else {
        println!("Usage: ./rclox [source_file.lox]");
        exit(-1);
    };

    println!("Opening {}", path);
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
    let chunk = match Compiler::compile(contents) {
        Ok(c) => c,
        Err(err) => {
            eprintln!("{}", err);
            exit(-1);
        },
    };

    chunk.disassemble();

    println!("chunk: {:?}", &chunk);
    let res = VM::interpret(&chunk);
    match res {
        Ok(r) => println!("success: {}", r),
        Err(r) => eprintln!("{}", r),
    }
}