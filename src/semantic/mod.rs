mod expr;
mod opcode_map;

pub type ExprRef<'a> = &'a Expr<'a>;
pub type ExprList<'a> = oxc_allocator::Vec<'a, ExprRef<'a>>;
pub type LetBindings<'a> = oxc_allocator::Vec<'a, (&'a str, ExprRef<'a>)>;
pub type Params<'a> = oxc_allocator::Vec<'a, &'a str>;

pub use expr::{BinOp, Expr, UnaryOp, Value};
pub use opcode_map::{InitialSlot, Opcode, OpcodeMap, Pre, SymbolTable};
