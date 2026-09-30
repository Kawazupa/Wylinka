use std::collections::{HashMap, HashSet};

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::AstBuilder;
use oxc_ast::ast::{
    BindingPattern, CallExpression, Expression, FunctionBody, IdentifierReference, Program,
    Statement,
};
use oxc_ast_visit::{Visit, VisitMut, walk, walk_mut};
use oxc_span::SPAN;
use oxc_syntax::operator::UnaryOperator;

use super::Transformer;

pub struct LocalSimplifier;

impl LocalSimplifier {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LocalSimplifier {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for LocalSimplifier {
    fn name(&self) -> &'static str {
        "local_simplifier"
    }

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>) {
        let mut v = SimplifyVisitor { allocator };
        v.visit_program(program);
    }
}

struct SimplifyVisitor<'a> {
    allocator: &'a Allocator,
}

impl<'a> VisitMut<'a> for SimplifyVisitor<'a> {
    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        walk_mut::walk_function_body(self, it);
        self.simplify(it);
    }
}

impl<'a> SimplifyVisitor<'a> {
    fn simplify(&mut self, body: &mut FunctionBody<'a>) {
        let mut all: HashSet<&'a str> = HashSet::new();
        let mut fn_bindings: HashSet<&'a str> = HashSet::new();
        for stmt in body.statements.iter() {
            let Statement::VariableDeclaration(decl) = stmt else {
                continue;
            };
            for d in decl.declarations.iter() {
                let BindingPattern::BindingIdentifier(id) = &d.id else {
                    continue;
                };
                let name = id.name.as_str();
                all.insert(name);
                if d.init.as_ref().is_some_and(is_fn_expr) {
                    fn_bindings.insert(name);
                }
            }
        }
        if all.is_empty() {
            return;
        }

        let mut counter = UseCounter {
            names: &all,
            total: HashMap::new(),
            call: HashMap::new(),
        };
        for stmt in body.statements.iter() {
            counter.visit_statement(stmt);
        }
        let total = counter.total;
        let call = counter.call;
        let used = |name: &str| total.get(name).copied().unwrap_or(0);

        let mut inline: HashMap<&'a str, Expression<'a>> = HashMap::new();
        for name in fn_bindings.iter() {
            if used(name) == 1 && call.get(name).copied().unwrap_or(0) == 1 {
                if let Some(fnexpr) = find_init_clone(body, name, self.allocator) {
                    inline.insert(name, fnexpr);
                }
            }
        }
        let inlined: HashSet<&'a str> = inline.keys().copied().collect();

        if !inline.is_empty() {
            let mut repl = CalleeReplacer { repl: &mut inline };
            for stmt in body.statements.iter_mut() {
                repl.visit_statement(stmt);
            }
        }

        if inlined.is_empty() && all.iter().all(|n| used(n) != 0) {
            return;
        }

        let builder = AstBuilder::new(self.allocator);
        let old = std::mem::replace(&mut body.statements, builder.vec());
        let mut out = builder.vec();
        for stmt in old {
            let Statement::VariableDeclaration(decl_box) = stmt else {
                out.push(stmt);
                continue;
            };
            let decl = decl_box.unbox();
            let kind = decl.kind;
            for d in decl.declarations {
                let name = match &d.id {
                    BindingPattern::BindingIdentifier(id) => Some(id.name.as_str()),
                    _ => None,
                };
                match name {
                    Some(n) if inlined.contains(n) => {}
                    Some(n) if used(n) == 0 => {
                        if let Some(init) = d.init {
                            if !is_pure_expr(&init) {
                                out.push(builder.statement_expression(SPAN, init));
                            }
                        }
                    }
                    _ => {
                        let decls = builder.vec1(d);
                        let vd =
                            builder.alloc_variable_declaration(SPAN, kind, decls, false);
                        out.push(Statement::VariableDeclaration(vd));
                    }
                }
            }
        }
        body.statements = out;
    }
}

struct CalleeReplacer<'a, 'm> {
    repl: &'m mut HashMap<&'a str, Expression<'a>>,
}

impl<'a, 'm> VisitMut<'a> for CalleeReplacer<'a, 'm> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        walk_mut::walk_expression(self, it);
        let Expression::CallExpression(call) = it else {
            return;
        };
        let Expression::Identifier(id) = call.callee.get_inner_expression() else {
            return;
        };
        let name = id.name.as_str();
        if let Some(fnexpr) = self.repl.remove(name) {
            call.callee = fnexpr;
        }
    }
}

struct UseCounter<'a, 'n> {
    names: &'n HashSet<&'a str>,
    total: HashMap<&'a str, u32>,
    call: HashMap<&'a str, u32>,
}

impl<'a, 'n> Visit<'a> for UseCounter<'a, 'n> {
    fn visit_call_expression(&mut self, it: &CallExpression<'a>) {
        if let Expression::Identifier(id) = it.callee.get_inner_expression() {
            let name = id.name.as_str();
            if self.names.contains(name) {
                *self.call.entry(name).or_insert(0) += 1;
            }
        }
        walk::walk_call_expression(self, it);
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        let name = it.name.as_str();
        if self.names.contains(name) {
            *self.total.entry(name).or_insert(0) += 1;
        }
    }
}

fn find_init_clone<'a>(
    body: &FunctionBody<'a>,
    name: &str,
    alloc: &'a Allocator,
) -> Option<Expression<'a>> {
    for stmt in body.statements.iter() {
        let Statement::VariableDeclaration(decl) = stmt else {
            continue;
        };
        for d in decl.declarations.iter() {
            if let BindingPattern::BindingIdentifier(id) = &d.id {
                if id.name.as_str() == name {
                    return d.init.as_ref().map(|e| e.clone_in(alloc));
                }
            }
        }
    }
    None
}

fn is_fn_expr(e: &Expression<'_>) -> bool {
    matches!(
        e.get_inner_expression(),
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
    )
}

fn is_pure_expr(expr: &Expression<'_>) -> bool {
    match expr {
        Expression::NullLiteral(_)
        | Expression::BooleanLiteral(_)
        | Expression::NumericLiteral(_)
        | Expression::StringLiteral(_)
        | Expression::BigIntLiteral(_)
        | Expression::RegExpLiteral(_)
        | Expression::Identifier(_)
        | Expression::ThisExpression(_)
        | Expression::FunctionExpression(_)
        | Expression::ArrowFunctionExpression(_)
        | Expression::ClassExpression(_) => true,
        Expression::UnaryExpression(u) => {
            !matches!(u.operator, UnaryOperator::Delete) && is_pure_expr(&u.argument)
        }
        Expression::BinaryExpression(b) => is_pure_expr(&b.left) && is_pure_expr(&b.right),
        Expression::LogicalExpression(l) => is_pure_expr(&l.left) && is_pure_expr(&l.right),
        Expression::ConditionalExpression(c) => {
            is_pure_expr(&c.test) && is_pure_expr(&c.consequent) && is_pure_expr(&c.alternate)
        }
        Expression::ParenthesizedExpression(p) => is_pure_expr(&p.expression),
        _ => false,
    }
}
