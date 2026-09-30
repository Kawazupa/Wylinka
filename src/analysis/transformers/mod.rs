pub mod const_folder;
pub mod iife;
pub mod local_simplifier;
pub mod thunk_inliner;
pub mod var_renamer;

use oxc_allocator::Allocator;
use oxc_ast::ast::Program;

pub trait Transformer<'a> {
    fn name(&self) -> &'static str;

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>);
}
