use std::collections::{HashMap, HashSet};

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{
    BindingPattern, BlockStatement, Expression, FunctionBody, IdentifierReference, Program,
    Statement, VariableDeclarator,
};
use oxc_ast_visit::{Visit, VisitMut, walk, walk_mut};
use oxc_syntax::operator::UnaryOperator;

use super::Transformer;

pub struct ThunkInliner;

impl ThunkInliner {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ThunkInliner {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for ThunkInliner {
    fn name(&self) -> &'static str {
        "thunk_inliner"
    }

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>) {
        let mut collector = ThunkCollector {
            allocator,
            thunks: HashMap::new(),
        };
        collector.visit_program(program);

        let thunks = collector.thunks;
        if thunks.is_empty() {
            return;
        }

        let thunk_names: HashSet<&'a str> = thunks.keys().copied().collect();
        let mut counter = UseCounter {
            thunk_names: &thunk_names,
            non_call_uses: HashSet::new(),
        };
        counter.visit_program(program);

        let mut replacer = ThunkReplacer {
            allocator,
            thunks: &thunks,
        };
        replacer.visit_program(program);

        let removable: HashSet<&'a str> = thunks
            .keys()
            .copied()
            .filter(|name| !counter.non_call_uses.contains(name))
            .collect();

        if removable.is_empty() {
            return;
        }

        let mut remover = ThunkDeclRemover { removable };
        remover.visit_program(program);
    }
}

struct ThunkCollector<'a> {
    allocator: &'a Allocator,
    thunks: HashMap<&'a str, Expression<'a>>,
}

impl<'a> Visit<'a> for ThunkCollector<'a> {
    fn visit_variable_declarator(&mut self, it: &VariableDeclarator<'a>) {
        walk::walk_variable_declarator(self, it);
        let BindingPattern::BindingIdentifier(name_box) = &it.id else {
            return;
        };
        let Some(init) = it.init.as_ref() else {
            return;
        };
        let Some(ret_expr) = extract_thunk_return(init) else {
            return;
        };
        let cloned = ret_expr.clone_in(self.allocator);
        self.thunks.insert(name_box.name.as_str(), cloned);
    }
}

fn extract_thunk_return<'a>(expr: &'a Expression<'a>) -> Option<&'a Expression<'a>> {
    let Expression::FunctionExpression(f) = expr else {
        return None;
    };
    if f.r#async || f.generator || f.id.is_some() {
        return None;
    }
    let body = f.body.as_ref()?;
    if !body.directives.is_empty() || body.statements.len() != 1 {
        return None;
    }
    let Statement::ReturnStatement(r) = &body.statements[0] else {
        return None;
    };
    let arg = r.argument.as_ref()?;
    if !is_inlineable_literal(arg) {
        return None;
    }
    Some(arg)
}

fn is_inlineable_literal(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::NullLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_) => true,
        Expression::Identifier(id) => matches!(id.name.as_str(), "window" | "undefined"),
        Expression::UnaryExpression(u) => {
            !matches!(u.operator, UnaryOperator::Delete) && is_inlineable_literal(&u.argument)
        }
        Expression::BinaryExpression(b) => {
            is_inlineable_literal(&b.left) && is_inlineable_literal(&b.right)
        }
        Expression::LogicalExpression(l) => {
            is_inlineable_literal(&l.left) && is_inlineable_literal(&l.right)
        }
        Expression::ConditionalExpression(c) => {
            is_inlineable_literal(&c.test)
                && is_inlineable_literal(&c.consequent)
                && is_inlineable_literal(&c.alternate)
        }
        Expression::ParenthesizedExpression(p) => is_inlineable_literal(&p.expression),
        _ => false,
    }
}

struct UseCounter<'a, 'n> {
    thunk_names: &'n HashSet<&'a str>,
    non_call_uses: HashSet<&'a str>,
}

impl<'a, 'n> Visit<'a> for UseCounter<'a, 'n> {
    fn visit_variable_declarator(&mut self, it: &VariableDeclarator<'a>) {
        if let BindingPattern::BindingIdentifier(name_box) = &it.id {
            if self.thunk_names.contains(name_box.name.as_str()) {
                return;
            }
        }
        walk::walk_variable_declarator(self, it);
    }

    fn visit_expression(&mut self, it: &Expression<'a>) {
        if let Expression::CallExpression(call) = it {
            if call.arguments.is_empty() {
                if let Expression::Identifier(ident) = call.callee.get_inner_expression() {
                    if self.thunk_names.contains(ident.name.as_str()) {
                        return;
                    }
                }
            }
        }
        walk::walk_expression(self, it);
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        if self.thunk_names.contains(it.name.as_str()) {
            self.non_call_uses.insert(it.name.as_str());
        }
    }
}

struct ThunkReplacer<'a, 'm> {
    allocator: &'a Allocator,
    thunks: &'m HashMap<&'a str, Expression<'a>>,
}

impl<'a, 'm> VisitMut<'a> for ThunkReplacer<'a, 'm> {
    fn visit_variable_declarator(&mut self, it: &mut VariableDeclarator<'a>) {
        if let BindingPattern::BindingIdentifier(name_box) = &it.id {
            if self.thunks.contains_key(name_box.name.as_str()) {
                return;
            }
        }
        walk_mut::walk_variable_declarator(self, it);
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        walk_mut::walk_expression(self, it);
        let Expression::CallExpression(call) = it else {
            return;
        };
        if !call.arguments.is_empty() {
            return;
        }
        let name = match call.callee.get_inner_expression() {
            Expression::Identifier(ident) => ident.name.as_str(),
            _ => return,
        };
        let Some(replacement) = self.thunks.get(name) else {
            return;
        };
        *it = replacement.clone_in(self.allocator);
    }
}

struct ThunkDeclRemover<'a> {
    removable: HashSet<&'a str>,
}

impl<'a> VisitMut<'a> for ThunkDeclRemover<'a> {
    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        walk_mut::walk_function_body(self, it);
        self.prune(&mut it.statements);
    }

    fn visit_block_statement(&mut self, it: &mut BlockStatement<'a>) {
        walk_mut::walk_block_statement(self, it);
        self.prune(&mut it.body);
    }
}

impl<'a> ThunkDeclRemover<'a> {
    fn prune(&self, stmts: &mut oxc_allocator::Vec<'a, Statement<'a>>) {
        for stmt in stmts.iter_mut() {
            if let Statement::VariableDeclaration(decl) = stmt {
                decl.declarations.retain(|d| {
                    if let BindingPattern::BindingIdentifier(name_box) = &d.id {
                        !self.removable.contains(name_box.name.as_str())
                    } else {
                        true
                    }
                });
            }
        }
        stmts.retain(|stmt| {
            if let Statement::VariableDeclaration(decl) = stmt {
                !decl.declarations.is_empty()
            } else {
                true
            }
        });
    }
}
