use std::io::Read;
use std::process::exit;
use std::thread::current;
use crate::chunk::Chunk;
use crate::ops::{Add, Const, Div, Mult, Negate, OpTrait, Ret, Sub};
use crate::source_ref::SourceRef;
use crate::vm::VM;

mod value;
mod ops;
mod source_ref;
mod chunk;
mod vm;
mod scanner;
mod compiler;
mod trie;

fn main() {
    let mut chunk = Chunk::new();
    let a = chunk.add_const(1.2);
    let b = chunk.add_const(3.4);
    let c = chunk.add_const(5.6);
    a.write(&mut chunk);
    b.write(&mut chunk);
    Add {}.write(&mut chunk);
    c.write(&mut chunk);
    Div {}.write(&mut chunk);
    Negate {}.write(&mut chunk);
    Ret {}.write(&mut chunk);
    VM::interpret(&chunk);

    let args: Vec<String> = std::env::args().collect();
    if let Some(path) = args.get(1) {
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
    } else {
        println!("Usage: ./rclox [source_file.lox]")
    }
}