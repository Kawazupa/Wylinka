use std::collections::HashMap;

use oxc_allocator::Vec as AVec;

use super::{Expr, ExprRef};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InitialSlot {
    Null,
    EmptyArray,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable<'a> {
    names: Vec<&'a str>,
}

impl<'a> SymbolTable<'a> {
    pub fn new() -> Self {
        SymbolTable { names: Vec::new() }
    }

    pub fn push(&mut self, name: &'a str) {
        self.names.push(name);
    }

    pub fn len(&self) -> u8 {
        self.names.len() as u8
    }

    pub fn is_empty(&self) -> bool {
        self.names.is_empty()
    }

    pub fn lookup(&self, name: &str) -> Option<u8> {
        self.names.iter().position(|n| *n == name).map(|i| i as u8)
    }

    pub fn names(&self) -> &[&'a str] {
        &self.names
    }
}

#[derive(Debug)]
pub struct Pre<'a> {
    pub name: &'a str,
    pub depth: u8,
    pub value: ExprRef<'a>,
}

#[derive(Debug)]
pub struct Opcode<'a> {
    pub id: u32,
    pub depth: u8,
    pub symbols: SymbolTable<'a>,
    pub pre: Vec<Pre<'a>>,
    pub body: ExprRef<'a>,
    pub source_levels: Vec<&'a str>,
}

impl<'a> Opcode<'a> {
    pub fn cycles_needed(&self) -> usize {
        let pre: usize = self.pre.iter().map(|p| node_count(p.value)).sum();
        pre + node_count(self.body)
    }
}

fn node_count(e: ExprRef) -> usize {
    match e {
        Expr::HashRoutine
        | Expr::Const(_)
        | Expr::Local(_)
        | Expr::Arg(_)
        | Expr::Slot(_)
        | Expr::Global(_)
        | Expr::This
        | Expr::IterVar
        | Expr::CatchVar
        | Expr::InvokeArgs
        | Expr::Unknown(_)
        | Expr::Reg { .. } => 1,
        Expr::PartialApp { args, .. } | Expr::Seq(args) => {
            1 + args.iter().map(|x| node_count(x)).sum::<usize>()
        }
        Expr::Force(a) | Expr::Thunk(a) | Expr::UnaryOp(_, a) => 1 + node_count(a),
        Expr::Lambda { body, .. } => 1 + node_count(body),
        Expr::Index(a, b) | Expr::BinOp(_, a, b) | Expr::Assign(a, b) => {
            1 + node_count(a) + node_count(b)
        }
        Expr::Apply(_, args) => 1 + args.iter().map(|x| node_count(x)).sum::<usize>(),
        Expr::Call(callee, args) | Expr::New(callee, args) => {
            1 + node_count(callee) + args.iter().map(|x| node_count(x)).sum::<usize>()
        }
        Expr::Cond { test, then, alt } => {
            1 + node_count(test) + node_count(then) + node_count(alt)
        }
        Expr::Let { bindings } => {
            1 + bindings.iter().map(|(_, v)| node_count(v)).sum::<usize>()
        }
        Expr::TryCatch {
            try_body,
            catch_body,
        } => 1 + node_count(try_body) + node_count(catch_body),
        Expr::ForIn { iter, body } => 1 + node_count(iter) + node_count(body),
        Expr::While { test, body } => 1 + node_count(test) + node_count(body),
    }
}

#[derive(Debug)]
pub struct OpcodeMap<'a> {
    pub initial: AVec<'a, InitialSlot>,
    pub ops: HashMap<u32, Opcode<'a>>,
    pub state_var: &'a str,
}
