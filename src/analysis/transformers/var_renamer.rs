use std::collections::HashMap;

use oxc_allocator::Allocator;
use oxc_ast::ast::{BindingIdentifier, IdentifierReference, Program};
use oxc_ast_visit::VisitMut;
use oxc_semantic::{ReferenceId, SemanticBuilder, SymbolId};
use oxc_str::Ident;

use super::Transformer;

pub struct VarRenamer;

impl VarRenamer {
    pub fn new() -> Self {
        Self
    }
}

impl Default for VarRenamer {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for VarRenamer {
    fn name(&self) -> &'static str {
        "var_renamer"
    }

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>) {
        let mut symbol_names: HashMap<SymbolId, &'a str> = HashMap::new();
        let mut ref_names: HashMap<ReferenceId, &'a str> = HashMap::new();

        {
            let semantic = SemanticBuilder::new().build(program).semantic;
            let scoping = semantic.scoping();

            for (i, symbol_id) in scoping.symbol_ids().enumerate() {
                let name: &'a str = allocator.alloc_str(&format!("v{i}"));
                symbol_names.insert(symbol_id, name);
                for reference_id in scoping.get_resolved_reference_ids(symbol_id) {
                    ref_names.insert(*reference_id, name);
                }
            }
        }

        let mut visitor = RenameVisitor {
            symbol_names,
            ref_names,
        };
        visitor.visit_program(program);
    }
}

struct RenameVisitor<'a> {
    symbol_names: HashMap<SymbolId, &'a str>,
    ref_names: HashMap<ReferenceId, &'a str>,
}

impl<'a> VisitMut<'a> for RenameVisitor<'a> {
    fn visit_binding_identifier(&mut self, it: &mut BindingIdentifier<'a>) {
        if let Some(symbol_id) = it.symbol_id.get() {
            if let Some(name) = self.symbol_names.get(&symbol_id) {
                it.name = Ident::from(*name);
            }
        }
    }

    fn visit_identifier_reference(&mut self, it: &mut IdentifierReference<'a>) {
        if let Some(reference_id) = it.reference_id.get() {
            if let Some(name) = self.ref_names.get(&reference_id) {
                it.name = Ident::from(*name);
            }
        }
    }
}
