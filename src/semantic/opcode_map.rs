use std::collections::HashMap;

use oxc_allocator::Vec as AVec;

use super::ExprRef;

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

#[derive(Debug)]
pub struct OpcodeMap<'a> {
    pub initial: AVec<'a, InitialSlot>,
    pub ops: HashMap<u32, Opcode<'a>>,
    pub state_var: &'a str,
}
