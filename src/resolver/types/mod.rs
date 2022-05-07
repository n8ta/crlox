mod func_scope;
mod resolved_func;
mod uniq_symbol;
mod var_ref;
mod var_decl;
mod upvalue;

pub use func_scope::FuncScope;
pub use resolved_func::ResolvedFunc;
pub use uniq_symbol::{UniqSymbol, UniqSymbolizer};
pub use var_decl::{VarDecl, VarDeclType};
pub use var_ref::VarRef;
pub use upvalue::{Upvalue, UpvalueType};