use std::collections::{HashMap, HashSet};

use oxc_allocator::{Allocator, CloneIn, TakeIn};
use oxc_ast::AstBuilder;
use oxc_ast::NONE;
use oxc_ast::ast::{
    Argument, ArrowFunctionExpression, BindingPattern, Expression, Function, FunctionBody,
    IdentifierReference, Program, Statement, VariableDeclarationKind,
};
use oxc_ast_visit::{Visit, VisitMut, walk, walk_mut};
use oxc_span::SPAN;
use oxc_syntax::operator::UnaryOperator;
use oxc_syntax::scope::ScopeFlags;

use super::Transformer;

pub struct IifeIsolator;

impl IifeIsolator {
    pub fn new() -> Self {
        Self
    }
}

impl Default for IifeIsolator {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for IifeIsolator {
    fn name(&self) -> &'static str {
        "iife_isolator"
    }

    fn transform(&mut self, _allocator: &'a Allocator, program: &mut Program<'a>) {
        let Some(idx) = program.body.iter().position(is_iife_statement) else {
            return;
        };
        if idx != 0 {
            program.body.drain(0..idx);
        }
        program.body.truncate(1);
    }
}

pub struct IifeFolder;

impl IifeFolder {
    pub fn new() -> Self {
        Self
    }
}

impl Default for IifeFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for IifeFolder {
    fn name(&self) -> &'static str {
        "iife_folder"
    }

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>) {
        let Some(Statement::ExpressionStatement(stmt)) = program.body.first_mut() else {
            return;
        };
        let Expression::CallExpression(call) = &mut stmt.expression else {
            return;
        };
        let Expression::FunctionExpression(func) = call.callee.get_inner_expression_mut() else {
            return;
        };
        let Some(body) = func.body.as_mut() else {
            return;
        };
        let mut folder = FoldVisitor {
            allocator,
            buckets: Vec::new(),
        };
        folder.visit_function_body(body);
    }
}

struct FoldVisitor<'a> {
    allocator: &'a Allocator,
    buckets: Vec<Vec<Statement<'a>>>,
}

impl<'a> VisitMut<'a> for FoldVisitor<'a> {
    fn visit_function_body(&mut self, it: &mut FunctionBody<'a>) {
        self.buckets.push(Vec::new());
        walk_mut::walk_function_body(self, it);
        let bucket = self.buckets.pop().expect("bucket pushed at entry");
        if bucket.is_empty() {
            return;
        }
        let builder = AstBuilder::new(self.allocator);
        let original = std::mem::replace(&mut it.statements, builder.vec());
        let mut new_stmts = builder.vec();
        for s in bucket {
            new_stmts.push(s);
        }
        new_stmts.extend(original);
        it.statements = new_stmts;
    }

    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        walk_mut::walk_expression(self, it);
        if !is_iife_with_returnish_body(it) {
            return;
        }
        if !iife_captures_param(it) && args_are_pure(it) {
            self.fold(it);
        } else {
            self.lift(it);
        }
        self.visit_expression(it);
    }
}

impl<'a> FoldVisitor<'a> {
    fn fold(&mut self, it: &mut Expression<'a>) {
        let Expression::CallExpression(call) = it.take_in(self.allocator) else {
            unreachable!()
        };
        let call = call.unbox();
        let arguments = call.arguments;
        let Expression::FunctionExpression(func) = unwrap_parens(call.callee) else {
            unreachable!()
        };
        let func = func.unbox();
        let Function { params, body, .. } = func;

        let builder = AstBuilder::new(self.allocator);

        let mut args: Vec<Expression<'a>> = arguments
            .into_iter()
            .map(|arg| match arg {
                Argument::SpreadElement(_) => unreachable!(),
                other => other.into_expression(),
            })
            .collect();

        let mut map: HashMap<&'a str, Expression<'a>> = HashMap::new();
        for (i, param) in params.unbox().items.into_iter().enumerate() {
            let BindingPattern::BindingIdentifier(name) = param.pattern else {
                unreachable!()
            };
            let value = if i < args.len() {
                std::mem::replace(
                    &mut args[i],
                    builder.expression_identifier(SPAN, "undefined"),
                )
            } else {
                builder.expression_identifier(SPAN, "undefined")
            };
            map.insert(name.unbox().name.as_str(), value);
        }

        let body = body.unwrap().unbox();
        let mut body_stmts: Vec<Statement<'a>> = body.statements.into_iter().collect();
        let return_stmt = body_stmts
            .pop()
            .expect("returnish body guarantees a return");

        let mut subst = SubstVisitor {
            allocator: self.allocator,
            map,
        };
        for stmt in body_stmts.iter_mut() {
            if let Statement::VariableDeclaration(decl) = stmt {
                for declarator in decl.declarations.iter_mut() {
                    if let Some(init) = declarator.init.as_mut() {
                        subst.visit_expression(init);
                    }
                }
            }
        }

        let Statement::ReturnStatement(r) = return_stmt else {
            unreachable!()
        };
        let mut result = r
            .unbox()
            .argument
            .unwrap_or_else(|| builder.expression_identifier(SPAN, "undefined"));
        subst.visit_expression(&mut result);

        let bucket = self
            .buckets
            .last_mut()
            .expect("fold called outside any function body");
        for s in body_stmts {
            bucket.push(s);
        }

        *it = result;
    }

    fn lift(&mut self, it: &mut Expression<'a>) {
        let builder = AstBuilder::new(self.allocator);
        let Expression::CallExpression(call) = it.take_in(self.allocator) else {
            unreachable!()
        };
        let call = call.unbox();
        let arguments = call.arguments;
        let Expression::FunctionExpression(func) = unwrap_parens(call.callee) else {
            unreachable!()
        };
        let func = func.unbox();
        let Function {
            params,
            body: iife_body,
            ..
        } = func;

        let mut args: Vec<Expression<'a>> = arguments
            .into_iter()
            .map(|arg| match arg {
                Argument::SpreadElement(_) => unreachable!(),
                other => other.into_expression(),
            })
            .collect();

        let mut new_decls: Vec<Statement<'a>> = Vec::new();
        for (i, param) in params.unbox().items.into_iter().enumerate() {
            let BindingPattern::BindingIdentifier(name_box) = param.pattern else {
                unreachable!()
            };
            let name = name_box.unbox().name;
            let value = if i < args.len() {
                std::mem::replace(
                    &mut args[i],
                    builder.expression_identifier(SPAN, "undefined"),
                )
            } else {
                builder.expression_identifier(SPAN, "undefined")
            };
            let pattern = builder.binding_pattern_binding_identifier(SPAN, name);
            let declarator = builder.variable_declarator(
                SPAN,
                VariableDeclarationKind::Var,
                pattern,
                NONE,
                Some(value),
                false,
            );
            let decls = builder.vec1(declarator);
            let var_decl = builder.alloc_variable_declaration(
                SPAN,
                VariableDeclarationKind::Var,
                decls,
                false,
            );
            new_decls.push(Statement::VariableDeclaration(var_decl));
        }

        let iife_body_unbox = iife_body.unwrap().unbox();
        let mut body_stmts: Vec<Statement<'a>> = iife_body_unbox.statements.into_iter().collect();
        let return_stmt = body_stmts
            .pop()
            .expect("returnish body guarantees a return");

        let bucket = self
            .buckets
            .last_mut()
            .expect("lift called outside any function body");
        for s in new_decls {
            bucket.push(s);
        }
        for s in body_stmts {
            bucket.push(s);
        }

        let Statement::ReturnStatement(r) = return_stmt else {
            unreachable!()
        };
        let result = r
            .unbox()
            .argument
            .unwrap_or_else(|| builder.expression_identifier(SPAN, "undefined"));

        *it = result;
    }
}

struct SubstVisitor<'a> {
    allocator: &'a Allocator,
    map: HashMap<&'a str, Expression<'a>>,
}

impl<'a> VisitMut<'a> for SubstVisitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        if let Expression::Identifier(ident) = it {
            if let Some(replacement) = self.map.get(ident.name.as_str()) {
                *it = replacement.clone_in(self.allocator);
                return;
            }
        }
        walk_mut::walk_expression(self, it);
    }
}

fn unwrap_parens(mut expr: Expression<'_>) -> Expression<'_> {
    loop {
        match expr {
            Expression::ParenthesizedExpression(inner) => expr = inner.unbox().expression,
            other => return other,
        }
    }
}

fn is_iife_with_returnish_body(expr: &Expression<'_>) -> bool {
    let Expression::CallExpression(call) = expr else {
        return false;
    };
    if call.optional || call.arguments.iter().any(Argument::is_spread) {
        return false;
    }
    let Expression::FunctionExpression(func) = call.callee.get_inner_expression() else {
        return false;
    };
    if func.generator || func.r#async || func.id.is_some() {
        return false;
    }
    if func.params.rest.is_some() {
        return false;
    }
    if !func.params.items.iter().all(|p| {
        matches!(p.pattern, BindingPattern::BindingIdentifier(_)) && p.initializer.is_none()
    }) {
        return false;
    }
    let Some(body) = func.body.as_ref() else {
        return false;
    };
    if !body.directives.is_empty() || body.statements.is_empty() {
        return false;
    }
    let Some(last) = body.statements.len().checked_sub(1) else {
        return false;
    };
    if !matches!(body.statements[last], Statement::ReturnStatement(_)) {
        return false;
    }
    body.statements[..last].iter().all(|s| {
        matches!(s, Statement::VariableDeclaration(d) if matches!(d.kind, VariableDeclarationKind::Var))
    })
}

fn iife_captures_param(expr: &Expression<'_>) -> bool {
    let Expression::CallExpression(call) = expr else {
        return false;
    };
    let Expression::FunctionExpression(func) = call.callee.get_inner_expression() else {
        return false;
    };
    let mut params = HashSet::new();
    for p in func.params.items.iter() {
        if let BindingPattern::BindingIdentifier(name) = &p.pattern {
            params.insert(name.name.as_str());
        }
    }
    if params.is_empty() {
        return false;
    }
    let Some(body) = func.body.as_ref() else {
        return false;
    };
    let mut v = CaptureCheck {
        params: &params,
        depth: 0,
        found: false,
    };
    for stmt in body.statements.iter() {
        match stmt {
            Statement::VariableDeclaration(decl) => {
                for declarator in decl.declarations.iter() {
                    if let Some(init) = declarator.init.as_ref() {
                        v.visit_expression(init);
                        if v.found {
                            return true;
                        }
                    }
                }
            }
            Statement::ReturnStatement(ret) => {
                if let Some(arg) = ret.argument.as_ref() {
                    v.visit_expression(arg);
                    if v.found {
                        return true;
                    }
                }
            }
            _ => {}
        }
    }
    v.found
}

fn args_are_pure(expr: &Expression<'_>) -> bool {
    let Expression::CallExpression(call) = expr else {
        return false;
    };
    call.arguments.iter().all(|arg| match arg {
        Argument::SpreadElement(_) => false,
        Argument::NullLiteral(_)
        | Argument::BooleanLiteral(_)
        | Argument::NumericLiteral(_)
        | Argument::StringLiteral(_)
        | Argument::BigIntLiteral(_)
        | Argument::RegExpLiteral(_)
        | Argument::Identifier(_)
        | Argument::ThisExpression(_)
        | Argument::FunctionExpression(_)
        | Argument::ArrowFunctionExpression(_)
        | Argument::ClassExpression(_) => true,
        Argument::UnaryExpression(u) => {
            !matches!(u.operator, UnaryOperator::Delete) && is_pure_expr(&u.argument)
        }
        Argument::BinaryExpression(b) => is_pure_expr(&b.left) && is_pure_expr(&b.right),
        Argument::LogicalExpression(l) => is_pure_expr(&l.left) && is_pure_expr(&l.right),
        Argument::ConditionalExpression(c) => {
            is_pure_expr(&c.test) && is_pure_expr(&c.consequent) && is_pure_expr(&c.alternate)
        }
        Argument::ParenthesizedExpression(p) => is_pure_expr(&p.expression),
        _ => false,
    })
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

struct CaptureCheck<'p> {
    params: &'p HashSet<&'p str>,
    depth: u32,
    found: bool,
}

impl<'a, 'p> Visit<'a> for CaptureCheck<'p> {
    fn visit_function(&mut self, it: &Function<'a>, flags: ScopeFlags) {
        self.depth += 1;
        walk::walk_function(self, it, flags);
        self.depth -= 1;
    }

    fn visit_arrow_function_expression(&mut self, it: &ArrowFunctionExpression<'a>) {
        self.depth += 1;
        walk::walk_arrow_function_expression(self, it);
        self.depth -= 1;
    }

    fn visit_identifier_reference(&mut self, it: &IdentifierReference<'a>) {
        if self.depth > 0 && self.params.contains(it.name.as_str()) {
            self.found = true;
        }
    }
}

fn is_iife_statement(stmt: &Statement<'_>) -> bool {
    let Statement::ExpressionStatement(expr_stmt) = stmt else {
        return false;
    };
    let Expression::CallExpression(call) = &expr_stmt.expression else {
        return false;
    };
    matches!(
        call.callee.get_inner_expression(),
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_)
    )
}
