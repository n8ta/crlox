# crlox

Crlox is a rust implementation of the lox language from the book [craftinginterpreters](craftinginterpreters.com).

## Language Features
- Dynamic (python like field assignments)
- Closures, first class functions, & lexical scope
- Bound methods
- Dense bytecode format for fast execution and lower memory overhead
- Robust source code tracking that can print source code of any runtime exception.

## Building

Only cargo required.
```asm
cargo build
cargo run path_to_program.lox
```

## Testing
```
cargo test
```
Tests are in `./tests` and run `textXYZ.lox` and compare its output to
`outXYZ.lox` in teh same directory. As features are complete new tests can be 
moved out of `./tests/not_ready`