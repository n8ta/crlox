// use std::fmt::{Debug, Display, Formatter};
// use std::ops::Deref;
// use crate::{Chunk, Value};
// use crate::ops::print_ops;
//
// #[derive(Clone, PartialEq)]
// pub struct Func<T: Debug + Display + Clone + PartialEq> {
//     inner: FuncInner<T>,
// }
//
// impl<T: Debug + Display + Clone + PartialEq> Deref for Func<T> {
//     type Target = FuncInner<T>;
//
//     fn deref(&self) -> &Self::Target {
//         &self.inner
//     }
// }
//
//
// impl<T: Debug + Display + Clone + PartialEq> Debug for Func<T> {
//     /// Print this function and any function in it's constants
//     /// Hope there's no loops!
//     fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
//         f.write_str(&format!("func<{}>\n", self.inner.name))?;
//         f.write_str(&format!("{}", print_ops(&self.inner.chunk.code)))?;
//
//         f.write_str("\n")?;
//         for constant in self.inner.chunk.constants.iter() {
//             if let Value::Func(const_func) = constant {
//                 f.write_str(&format!("{:?}", const_func))?;
//             }
//         }
//         Ok(())
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub struct FuncInner<T: PartialEq + Clone> {
//     pub name: T,
//     pub chunk: Chunk,
//     pub arity: u8,
//     pub ftype: FuncType,
//
// }
//
// impl<T: Clone + PartialEq + Debug + Display> Func<T> {
//     pub fn name(&self) -> T {
//         self.inner.name.clone()
//     }
//     pub fn arity(&self) -> u8 {
//         self.inner.arity
//     }
//     pub fn chunk(&self) -> &Chunk {
//         &self.inner.chunk
//     }
//     // pub fn global(chunk: Chunk, uniq_symbolizer: UniqSymbolizer) -> Func<UniqSymbol> {
//     //     let inner: FuncInner<T> = FuncInner {
//     //         name: uniq_symbolizer.root(),
//     //         chunk,
//     //         arity: 0,
//     //         ftype: FuncType::Function,
//     //     };
//     //     Func {
//     //         inner,
//     //     }
//     // }
//     pub fn new(name: T, arity: u8, ftype: FuncType, chunk: Chunk) -> Func<T> {
//         let inner = FuncInner {
//             name,
//             chunk,
//             arity,
//             ftype,
//         };
//
//         Func {
//             inner,
//         }
//     }
// }
//
// #[derive(Debug, PartialEq, Clone)]
// pub enum FuncType {
//     Method,
//     Function,
// }