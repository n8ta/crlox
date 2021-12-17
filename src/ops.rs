use std::ops::Neg;
use crate::{Chunk, SourceRef};
use crate::source_ref::Source;

pub trait OpTrait {
    const CODE: u8;
    const SIZE: usize;
    fn write(&self, code: &mut Chunk, src: SourceRef);
    fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self);
}

#[derive(Debug)]
pub struct Ret {}

impl OpTrait for Ret {
    const CODE: u8 = 0;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Ret {}) }
}

#[derive(Debug)]
pub struct Const {
    pub idx: u8,
}

impl OpTrait for Const {
    const CODE: u8 = 1;
    const SIZE: usize = 2;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src.clone());
        code.add_byte(self.idx, src);
    }
    fn decode(code: &Vec<u8>, idx: usize) -> (usize, Self) {
        (1, Const { idx: *code.get(idx).expect("Bad bytecode") })
    }
}

#[derive(Debug)]
pub struct Negate {}

impl OpTrait for Negate {
    const CODE: u8 = 2;
    const SIZE: usize = 1;

    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }

    fn decode(_code: &Vec<u8>, idx: usize) -> (usize, Self) { (0, Negate {}) }
}

#[derive(Debug)]
pub struct Add {}

impl OpTrait for Add {
    const CODE: u8 = 3;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Add{})}
}
#[derive(Debug)]
pub struct Sub {}

impl OpTrait for Sub {
    const CODE: u8 = 4;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Sub{})}
}
#[derive(Debug)]
pub struct Mult {}

impl OpTrait for Mult {
    const CODE: u8 = 5;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Mult{})}
}
#[derive(Debug)]
pub struct Div {}

impl OpTrait for Div {
    const CODE: u8 = 6;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Div{})}
}

#[derive(Debug)]
pub struct True {}

impl OpTrait for True {
    const CODE: u8 = 7;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, True{})}
}

#[derive(Debug)]
pub struct False {}

impl OpTrait for False {
    const CODE: u8 = 8;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, False{})}
}

#[derive(Debug)]
pub struct Nil {}

impl OpTrait for Nil {
    const CODE: u8 = 9;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Nil{})}
}

#[derive(Debug)]
pub struct Not {}

impl OpTrait for Not {
    const CODE: u8 = 10;
    const SIZE: usize = 1;
    fn write(&self, code: &mut Chunk, src: SourceRef) {
        code.add_byte(Self::CODE, src);
    }
    fn decode(_code: &Vec<u8>, _idx: usize) -> (usize, Self) { (0, Not{})}
}





























