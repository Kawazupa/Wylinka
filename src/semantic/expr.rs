use super::{ExprList, ExprRef, LetBindings, Params};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value<'a> {
    Undefined,
    Null,
    Bool(bool),
    Number(f64),
    String(&'a str),
    Window,
    Fn(&'a str),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Exp,
    Shl,
    Shr,
    ShrU,
    BitOr,
    BitXor,
    BitAnd,
    Eq,
    Neq,
    StrictEq,
    StrictNeq,
    Lt,
    Le,
    Gt,
    Ge,
    In,
    InstanceOf,
    LogOr,
    LogAnd,
    Coalesce,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Neg,
    Plus,
    Not,
    BitNot,
    TypeOf,
    Void,
    Delete,
}

#[derive(Debug)]
pub enum Expr<'a> {
    HashRoutine,
    Const(Value<'a>),
    Local(&'a str),
    Arg(&'a str),
    Slot(u32),
    Global(&'a str),
    This,
    IterVar,
    CatchVar,
    InvokeArgs,
    Unknown(&'static str),
    Force(ExprRef<'a>),
    Thunk(ExprRef<'a>),
    Seq(ExprList<'a>),
    Lambda {
        params: Params<'a>,
        body: ExprRef<'a>,
    },
    Reg {
        slot: u16,
        version: u32,
    },
    PartialApp {
        opcode: u32,
        args: ExprList<'a>,
        locals: LetBindings<'a>,
    },
    Index(ExprRef<'a>, ExprRef<'a>),
    Apply(&'a str, ExprList<'a>),
    Call(ExprRef<'a>, ExprList<'a>),
    New(ExprRef<'a>, ExprList<'a>),
    BinOp(BinOp, ExprRef<'a>, ExprRef<'a>),
    UnaryOp(UnaryOp, ExprRef<'a>),
    Cond {
        test: ExprRef<'a>,
        then: ExprRef<'a>,
        alt: ExprRef<'a>,
    },
    Assign(ExprRef<'a>, ExprRef<'a>),
    Let {
        bindings: LetBindings<'a>,
    },
    TryCatch {
        try_body: ExprRef<'a>,
        catch_body: ExprRef<'a>,
    },
    ForIn {
        iter: ExprRef<'a>,
        body: ExprRef<'a>,
    },
    While {
        test: ExprRef<'a>,
        body: ExprRef<'a>,
    },
}
