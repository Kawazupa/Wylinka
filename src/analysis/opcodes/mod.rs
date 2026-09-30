use std::collections::HashMap;

use oxc_allocator::{Allocator, Vec as AVec};
use oxc_ast::ast::{
    Argument, ArrayExpressionElement, AssignmentTarget, BindingPattern, Expression,
    ForStatementLeft, Function, Program, Statement,
};
use oxc_syntax::operator::{BinaryOperator, LogicalOperator, UnaryOperator};

use super::extractors::extract_opcode_array;
use crate::semantic::{
    BinOp, Expr, ExprList, ExprRef, InitialSlot, Opcode, OpcodeMap, Pre, SymbolTable, UnaryOp,
    Value,
};

pub fn extract<'a, 'p>(
    program: &'p Program<'p>,
    source: &'a str,
    alloc: &'a Allocator,
) -> Option<OpcodeMap<'a>> {
    let (state_var, array) = extract_opcode_array(program)?;

    let mut initial: AVec<InitialSlot> = AVec::with_capacity_in(8, alloc);
    let mut ops: HashMap<u32, Opcode<'a>> = HashMap::with_capacity(array.elements.len());
    let mut in_ops = false;

    for (idx, elem) in array.elements.iter().enumerate() {
        match elem {
            ArrayExpressionElement::NullLiteral(_) if !in_ops => {
                initial.push(InitialSlot::Null);
            }
            ArrayExpressionElement::ArrayExpression(arr) if !in_ops && arr.elements.is_empty() => {
                initial.push(InitialSlot::EmptyArray);
            }
            ArrayExpressionElement::FunctionExpression(func) => {
                in_ops = true;
                ops.insert(
                    idx as u32,
                    map_function(idx as u32, func, state_var, source, alloc),
                );
            }
            _ => eprintln!("opcodes: unexpected element kind at slot {idx}"),
        }
    }
    Some(OpcodeMap {
        initial,
        ops,
        state_var: alloc.alloc_str(state_var),
    })
}

fn capture_source_levels<'a, 'p>(func: &'p Function<'p>, source: &'a str) -> Vec<&'a str> {
    let mut levels = Vec::new();
    let outer_start = func.span.start as usize;
    let outer_end = func.span.end as usize;
    if outer_end > source.len() || outer_start > outer_end {
        return levels;
    }
    levels.push(&source[outer_start..outer_end]);
    let mut cur = func;
    loop {
        let Some(body) = cur.body.as_ref() else {
            break;
        };
        if body.statements.len() != 1 {
            break;
        }
        let Statement::ReturnStatement(ret) = &body.statements[0] else {
            break;
        };
        let Some(arg) = ret.argument.as_ref() else {
            break;
        };
        let Expression::FunctionExpression(inner) = arg else {
            break;
        };
        let s = inner.span.start as usize;
        let e = inner.span.end as usize;
        if e > source.len() || s > e {
            break;
        }
        levels.push(&source[s..e]);
        cur = inner;
    }
    levels
}

fn map_function<'a, 'p>(
    id: u32,
    func: &'p Function<'p>,
    state_var: &'p str,
    source: &'a str,
    alloc: &'a Allocator,
) -> Opcode<'a> {
    let source_levels = capture_source_levels(func, source);
    if func.params.items.len() > 1 {
        let mut symbols = SymbolTable::new();
        for p in func.params.items.iter() {
            if let BindingPattern::BindingIdentifier(id) = &p.pattern {
                symbols.push(alloc.alloc_str(id.name.as_str()));
            }
        }
        return Opcode {
            id,
            depth: symbols.len(),
            symbols,
            pre: Vec::new(),
            body: alloc.alloc(Expr::HashRoutine),
            source_levels,
        };
    }

    let mut ctx = Ctx {
        state_var,
        env: HashMap::new(),
        thunks: HashMap::new(),
        iter_var: None,
        catch_var: None,
    };
    let mut pre: Vec<Pre<'a>> = Vec::new();
    let mut symbols = SymbolTable::new();
    let body = lower_curry(func, &mut ctx, &mut pre, &mut symbols, alloc);
    Opcode {
        id,
        depth: symbols.len(),
        symbols,
        pre,
        body,
        source_levels,
    }
}

fn lower_curry<'a, 'p>(
    func: &'p Function<'p>,
    ctx: &mut Ctx<'p, 'a>,
    pre: &mut Vec<Pre<'a>>,
    symbols: &mut SymbolTable<'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    for p in func.params.items.iter() {
        if let BindingPattern::BindingIdentifier(id) = &p.pattern {
            let name = alloc.alloc_str(id.name.as_str());
            symbols.push(name);
            ctx.env.insert(id.name.as_str(), alloc.alloc(Expr::Arg(name)));
        }
    }

    let Some(body) = func.body.as_ref() else {
        return alloc.alloc(Expr::Const(Value::Undefined));
    };
    let stmts = &body.statements;
    if stmts.is_empty() {
        return alloc.alloc(Expr::Const(Value::Undefined));
    }
    let last_idx = stmts.len() - 1;
    let depth = symbols.len();

    for stmt in &stmts[..last_idx] {
        bind_curry_decls(stmt, ctx, pre, depth, alloc);
    }

    match &stmts[last_idx] {
        Statement::ReturnStatement(ret) => match &ret.argument {
            Some(Expression::FunctionExpression(inner)) if inner.params.items.len() == 1 => {
                lower_curry(inner, ctx, pre, symbols, alloc)
            }
            Some(Expression::FunctionExpression(thunk)) => lower_thunk(thunk, ctx, alloc),
            Some(arg) => lower_expr(arg, ctx, alloc),
            None => alloc.alloc(Expr::Const(Value::Undefined)),
        },
        _ => alloc.alloc(Expr::HashRoutine),
    }
}

fn bind_curry_decls<'a, 'p>(
    stmt: &'p Statement<'p>,
    ctx: &mut Ctx<'p, 'a>,
    pre: &mut Vec<Pre<'a>>,
    depth: u8,
    alloc: &'a Allocator,
) {
    if let Statement::ExpressionStatement(es) = stmt {
        let value = lower_expr(&es.expression, ctx, alloc);
        pre.push(Pre {
            name: "_eff",
            depth,
            value,
        });
        return;
    }
    let Statement::VariableDeclaration(decl) = stmt else {
        return;
    };
    for d in decl.declarations.iter() {
        let BindingPattern::BindingIdentifier(id) = &d.id else {
            continue;
        };
        let key = id.name.as_str();
        let Some(init) = &d.init else {
            ctx.env
                .insert(key, alloc.alloc(Expr::Const(Value::Undefined)));
            continue;
        };
        if let Expression::FunctionExpression(func) = init {
            ctx.thunks.insert(key, func.as_ref());
        } else {
            let value = lower_expr(init, ctx, alloc);
            let name = alloc.alloc_str(key);
            pre.push(Pre { name, depth, value });
            ctx.env.insert(key, alloc.alloc(Expr::Local(name)));
        }
    }
}

fn bind_inline_decls<'a, 'p>(
    stmt: &'p Statement<'p>,
    ctx: &mut Ctx<'p, 'a>,
    effects: &mut Vec<ExprRef<'a>>,
    alloc: &'a Allocator,
) {
    if let Statement::ExpressionStatement(es) = stmt {
        effects.push(lower_expr(&es.expression, ctx, alloc));
        return;
    }
    let Statement::VariableDeclaration(decl) = stmt else {
        return;
    };
    for d in decl.declarations.iter() {
        let BindingPattern::BindingIdentifier(id) = &d.id else {
            continue;
        };
        let key = id.name.as_str();
        let Some(init) = &d.init else {
            ctx.env
                .insert(key, alloc.alloc(Expr::Const(Value::Undefined)));
            continue;
        };
        if let Expression::FunctionExpression(func) = init {
            ctx.thunks.insert(key, func.as_ref());
        } else {
            let value = lower_expr(init, ctx, alloc);
            ctx.env.insert(key, value);
        }
    }
}

fn lower_thunk<'a, 'p>(
    thunk: &'p Function<'p>,
    ctx: &mut Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    let Some(body) = thunk.body.as_ref() else {
        return alloc.alloc(Expr::Thunk(alloc.alloc(Expr::HashRoutine)));
    };
    let stmts = &body.statements;
    if stmts.is_empty() {
        return alloc.alloc(Expr::Thunk(alloc.alloc(Expr::Const(Value::Undefined))));
    }
    let last_idx = stmts.len() - 1;

    let mut effects: Vec<ExprRef<'a>> = Vec::new();
    for stmt in &stmts[..last_idx] {
        bind_inline_decls(stmt, ctx, &mut effects, alloc);
    }

    let body_expr = match &stmts[last_idx] {
        Statement::TryStatement(t) => {
            let try_body = lower_block_as_expr(&t.block.body, ctx, alloc);
            let catch_body = if let Some(handler) = &t.handler {
                let prev_catch = ctx.catch_var;
                if let Some(param) = &handler.param {
                    if let BindingPattern::BindingIdentifier(id) = &param.pattern {
                        ctx.catch_var = Some(id.name.as_str());
                    }
                }
                let cb = lower_block_as_expr(&handler.body.body, ctx, alloc);
                ctx.catch_var = prev_catch;
                cb
            } else {
                alloc.alloc(Expr::Const(Value::Undefined))
            };
            alloc.alloc(Expr::TryCatch {
                try_body,
                catch_body,
            })
        }
        Statement::ForInStatement(f) => {
            let iter = lower_expr(&f.right, ctx, alloc);
            let prev_iter = ctx.iter_var;
            if let ForStatementLeft::VariableDeclaration(decl) = &f.left {
                if let Some(d) = decl.declarations.first() {
                    if let BindingPattern::BindingIdentifier(id) = &d.id {
                        ctx.iter_var = Some(id.name.as_str());
                    }
                }
            }
            let body_expr = lower_stmt_as_expr(&f.body, ctx, alloc);
            ctx.iter_var = prev_iter;
            alloc.alloc(Expr::ForIn {
                iter,
                body: body_expr,
            })
        }
        Statement::WhileStatement(w) => {
            let test = lower_expr(&w.test, ctx, alloc);
            let body_expr = lower_stmt_as_expr(&w.body, ctx, alloc);
            alloc.alloc(Expr::While {
                test,
                body: body_expr,
            })
        }
        other => lower_stmt_as_expr(other, ctx, alloc),
    };

    let body_expr = if effects.is_empty() {
        body_expr
    } else {
        let mut seq: ExprList<'a> = AVec::with_capacity_in(effects.len() + 1, alloc);
        for e in effects {
            seq.push(e);
        }
        seq.push(body_expr);
        alloc.alloc(Expr::Seq(seq))
    };

    alloc.alloc(Expr::Thunk(body_expr))
}

fn lower_block_as_expr<'a, 'p>(
    stmts: &'p oxc_allocator::Vec<'p, Statement<'p>>,
    ctx: &Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    if stmts.is_empty() {
        return alloc.alloc(Expr::Const(Value::Undefined));
    }
    if stmts.len() == 1 {
        return lower_stmt_as_expr(&stmts[0], ctx, alloc);
    }
    alloc.alloc(Expr::HashRoutine)
}

fn lower_stmt_as_expr<'a, 'p>(
    stmt: &'p Statement<'p>,
    ctx: &Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    match stmt {
        Statement::BlockStatement(b) => lower_block_as_expr(&b.body, ctx, alloc),
        Statement::ExpressionStatement(es) => lower_expr(&es.expression, ctx, alloc),
        Statement::ReturnStatement(ret) => match &ret.argument {
            Some(a) => lower_expr(a, ctx, alloc),
            None => alloc.alloc(Expr::Const(Value::Undefined)),
        },
        _ => alloc.alloc(Expr::HashRoutine),
    }
}

#[derive(Clone)]
struct Ctx<'p, 'a> {
    state_var: &'p str,
    env: HashMap<&'p str, ExprRef<'a>>,
    thunks: HashMap<&'p str, &'p Function<'p>>,
    iter_var: Option<&'p str>,
    catch_var: Option<&'p str>,
}

fn lower_expr<'a, 'p>(
    expr: &'p Expression<'p>,
    ctx: &Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    match expr {
        Expression::Identifier(id) => lower_ident(id.name.as_str(), ctx, alloc),
        Expression::ThisExpression(_) => alloc.alloc(Expr::This),
        Expression::NullLiteral(_) => alloc.alloc(Expr::Const(Value::Null)),
        Expression::BooleanLiteral(b) => alloc.alloc(Expr::Const(Value::Bool(b.value))),
        Expression::NumericLiteral(n) => alloc.alloc(Expr::Const(Value::Number(n.value))),
        Expression::StringLiteral(s) => {
            let v = alloc.alloc_str(s.value.as_str());
            alloc.alloc(Expr::Const(Value::String(v)))
        }
        Expression::ParenthesizedExpression(p) => lower_expr(&p.expression, ctx, alloc),
        Expression::ComputedMemberExpression(m) => {
            let is_state = matches!(
                m.object.get_inner_expression(),
                Expression::Identifier(id) if id.name.as_str() == ctx.state_var
            );
            let prop = lower_expr(&m.expression, ctx, alloc);
            if is_state {
                if let Some(n) = const_int(prop) {
                    return alloc.alloc(Expr::Slot(n as u32));
                }
            }
            let obj = lower_expr(&m.object, ctx, alloc);
            alloc.alloc(Expr::Index(obj, prop))
        }
        Expression::StaticMemberExpression(m) => {
            let obj = lower_expr(&m.object, ctx, alloc);
            let name = alloc.alloc_str(m.property.name.as_str());
            let prop = alloc.alloc(Expr::Const(Value::String(name)));
            alloc.alloc(Expr::Index(obj, prop))
        }
        Expression::CallExpression(c) => {
            if let Expression::Identifier(callee_id) = &c.callee {
                if let Some(&thunk) = ctx.thunks.get(callee_id.name.as_str()) {
                    return inline_call(thunk, &c.arguments, ctx, alloc);
                }
            }
            let callee = lower_expr(&c.callee, ctx, alloc);
            let mut args_list: ExprList<'a> = AVec::with_capacity_in(c.arguments.len(), alloc);
            for a in c.arguments.iter() {
                if let Argument::SpreadElement(_) = a {
                    continue;
                }
                if let Some(e) = a.as_expression() {
                    args_list.push(lower_expr(e, ctx, alloc));
                }
            }
            match callee {
                Expr::Arg(_) | Expr::Local(_) if args_list.is_empty() => {
                    alloc.alloc(Expr::Force(callee))
                }
                Expr::Arg(name) => alloc.alloc(Expr::Apply(name, args_list)),
                _ => alloc.alloc(Expr::Call(callee, args_list)),
            }
        }
        Expression::NewExpression(n) => {
            let callee = lower_expr(&n.callee, ctx, alloc);
            let mut args_list: ExprList<'a> = AVec::with_capacity_in(n.arguments.len(), alloc);
            for a in n.arguments.iter() {
                if let Argument::SpreadElement(_) = a {
                    continue;
                }
                if let Some(e) = a.as_expression() {
                    args_list.push(lower_expr(e, ctx, alloc));
                }
            }
            alloc.alloc(Expr::New(callee, args_list))
        }
        Expression::BinaryExpression(b) => {
            let op = map_bin_op(b.operator);
            let l = lower_expr(&b.left, ctx, alloc);
            let r = lower_expr(&b.right, ctx, alloc);
            alloc.alloc(Expr::BinOp(op, l, r))
        }
        Expression::LogicalExpression(b) => {
            let op = map_log_op(b.operator);
            let l = lower_expr(&b.left, ctx, alloc);
            let r = lower_expr(&b.right, ctx, alloc);
            alloc.alloc(Expr::BinOp(op, l, r))
        }
        Expression::UnaryExpression(u) => {
            let op = map_un_op(u.operator);
            let v = lower_expr(&u.argument, ctx, alloc);
            alloc.alloc(Expr::UnaryOp(op, v))
        }
        Expression::ConditionalExpression(c) => {
            let test = lower_expr(&c.test, ctx, alloc);
            let then = lower_expr(&c.consequent, ctx, alloc);
            let alt = lower_expr(&c.alternate, ctx, alloc);
            alloc.alloc(Expr::Cond { test, then, alt })
        }
        Expression::AssignmentExpression(a) => {
            let target = lower_assign_target(&a.left, ctx, alloc);
            let value = lower_expr(&a.right, ctx, alloc);
            alloc.alloc(Expr::Assign(target, value))
        }
        Expression::FunctionExpression(_) | Expression::ArrowFunctionExpression(_) => {
            alloc.alloc(Expr::HashRoutine)
        }
        _ => alloc.alloc(Expr::Unknown(node_kind(expr))),
    }
}

fn lower_assign_target<'a, 'p>(
    t: &'p AssignmentTarget<'p>,
    ctx: &Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    match t {
        AssignmentTarget::AssignmentTargetIdentifier(id) => {
            lower_ident(id.name.as_str(), ctx, alloc)
        }
        AssignmentTarget::ComputedMemberExpression(m) => {
            let is_state = matches!(
                m.object.get_inner_expression(),
                Expression::Identifier(id) if id.name.as_str() == ctx.state_var
            );
            let prop = lower_expr(&m.expression, ctx, alloc);
            if is_state {
                if let Some(n) = const_int(prop) {
                    return alloc.alloc(Expr::Slot(n as u32));
                }
            }
            let obj = lower_expr(&m.object, ctx, alloc);
            alloc.alloc(Expr::Index(obj, prop))
        }
        AssignmentTarget::StaticMemberExpression(m) => {
            let obj = lower_expr(&m.object, ctx, alloc);
            let name = alloc.alloc_str(m.property.name.as_str());
            let prop = alloc.alloc(Expr::Const(Value::String(name)));
            alloc.alloc(Expr::Index(obj, prop))
        }
        _ => alloc.alloc(Expr::Unknown("AssignmentTarget")),
    }
}

fn lower_ident<'a, 'p>(name: &'p str, ctx: &Ctx<'p, 'a>, alloc: &'a Allocator) -> ExprRef<'a> {
    if let Some(&resolved) = ctx.env.get(name) {
        return resolved;
    }
    if Some(name) == ctx.iter_var {
        return alloc.alloc(Expr::IterVar);
    }
    if Some(name) == ctx.catch_var {
        return alloc.alloc(Expr::CatchVar);
    }
    if ctx.thunks.contains_key(name) {
        return alloc.alloc(Expr::HashRoutine);
    }
    if name == "undefined" {
        return alloc.alloc(Expr::Const(Value::Undefined));
    }
    if name == "arguments" {
        return alloc.alloc(Expr::InvokeArgs);
    }
    let owned = alloc.alloc_str(name);
    alloc.alloc(Expr::Global(owned))
}

fn inline_call<'a, 'p>(
    thunk: &'p Function<'p>,
    call_args: &'p oxc_allocator::Vec<'p, Argument<'p>>,
    ctx: &Ctx<'p, 'a>,
    alloc: &'a Allocator,
) -> ExprRef<'a> {
    let Some(body) = thunk.body.as_ref() else {
        return alloc.alloc(Expr::Const(Value::Undefined));
    };
    let stmts = &body.statements;
    if stmts.is_empty() {
        return alloc.alloc(Expr::Const(Value::Undefined));
    }
    let last_idx = stmts.len() - 1;

    let mut new_ctx = ctx.clone();
    for (i, param) in thunk.params.items.iter().enumerate() {
        let BindingPattern::BindingIdentifier(id) = &param.pattern else {
            continue;
        };
        let name = id.name.as_str();
        match call_args.get(i).and_then(|a| a.as_expression()) {
            Some(Expression::FunctionExpression(f)) => {
                new_ctx.thunks.insert(name, f.as_ref());
            }
            Some(e) => {
                let lowered = lower_expr(e, ctx, alloc);
                new_ctx.env.insert(name, lowered);
            }
            None => {
                new_ctx
                    .env
                    .insert(name, alloc.alloc(Expr::Const(Value::Undefined)));
            }
        }
    }

    for stmt in &stmts[..last_idx] {
        let Statement::VariableDeclaration(decl) = stmt else {
            continue;
        };
        for d in decl.declarations.iter() {
            let BindingPattern::BindingIdentifier(id) = &d.id else {
                continue;
            };
            let name = id.name.as_str();
            let Some(init) = &d.init else {
                new_ctx
                    .env
                    .insert(name, alloc.alloc(Expr::Const(Value::Undefined)));
                continue;
            };
            if let Expression::FunctionExpression(func) = init {
                new_ctx.thunks.insert(name, func.as_ref());
            } else {
                let lowered = lower_expr(init, &new_ctx, alloc);
                new_ctx.env.insert(name, lowered);
            }
        }
    }

    match &stmts[last_idx] {
        Statement::ReturnStatement(ret) => match &ret.argument {
            Some(arg) => lower_expr(arg, &new_ctx, alloc),
            None => alloc.alloc(Expr::Const(Value::Undefined)),
        },
        _ => alloc.alloc(Expr::HashRoutine),
    }
}

fn map_bin_op(op: BinaryOperator) -> BinOp {
    use BinaryOperator::*;
    match op {
        Addition => BinOp::Add,
        Subtraction => BinOp::Sub,
        Multiplication => BinOp::Mul,
        Division => BinOp::Div,
        Remainder => BinOp::Rem,
        Exponential => BinOp::Exp,
        ShiftLeft => BinOp::Shl,
        ShiftRight => BinOp::Shr,
        ShiftRightZeroFill => BinOp::ShrU,
        BitwiseOR => BinOp::BitOr,
        BitwiseXOR => BinOp::BitXor,
        BitwiseAnd => BinOp::BitAnd,
        Equality => BinOp::Eq,
        Inequality => BinOp::Neq,
        StrictEquality => BinOp::StrictEq,
        StrictInequality => BinOp::StrictNeq,
        LessThan => BinOp::Lt,
        LessEqualThan => BinOp::Le,
        GreaterThan => BinOp::Gt,
        GreaterEqualThan => BinOp::Ge,
        In => BinOp::In,
        Instanceof => BinOp::InstanceOf,
    }
}

fn map_log_op(op: LogicalOperator) -> BinOp {
    use LogicalOperator::*;
    match op {
        Or => BinOp::LogOr,
        And => BinOp::LogAnd,
        Coalesce => BinOp::Coalesce,
    }
}

fn map_un_op(op: UnaryOperator) -> UnaryOp {
    use UnaryOperator::*;
    match op {
        UnaryNegation => UnaryOp::Neg,
        UnaryPlus => UnaryOp::Plus,
        LogicalNot => UnaryOp::Not,
        BitwiseNot => UnaryOp::BitNot,
        Typeof => UnaryOp::TypeOf,
        Void => UnaryOp::Void,
        Delete => UnaryOp::Delete,
    }
}

fn const_int(e: ExprRef<'_>) -> Option<i64> {
    match e {
        Expr::Const(Value::Number(n)) => Some(*n as i64),
        Expr::Const(Value::Bool(b)) => Some(if *b { 1 } else { 0 }),
        Expr::Const(Value::Null) => Some(0),
        Expr::UnaryOp(UnaryOp::Plus, x) => const_int(x),
        Expr::UnaryOp(UnaryOp::Neg, x) => const_int(x).map(|v| -v),
        Expr::UnaryOp(UnaryOp::BitNot, x) => const_int(x).map(|v| !v),
        Expr::UnaryOp(UnaryOp::Not, x) => const_int(x).map(|v| if v == 0 { 1 } else { 0 }),
        Expr::BinOp(op, l, r) => {
            let lv = const_int(l)?;
            let rv = const_int(r)?;
            Some(match op {
                BinOp::Add => lv.wrapping_add(rv),
                BinOp::Sub => lv.wrapping_sub(rv),
                BinOp::Mul => lv.wrapping_mul(rv),
                BinOp::Shl => (lv as i32).wrapping_shl((rv & 31) as u32) as i64,
                BinOp::Shr => (lv as i32).wrapping_shr((rv & 31) as u32) as i64,
                BinOp::ShrU => (lv as u32).wrapping_shr((rv & 31) as u32) as i64,
                BinOp::BitOr => lv | rv,
                BinOp::BitAnd => lv & rv,
                BinOp::BitXor => lv ^ rv,
                _ => return None,
            })
        }
        _ => None,
    }
}

fn node_kind(expr: &Expression) -> &'static str {
    match expr {
        Expression::ArrayExpression(_) => "ArrayExpression",
        Expression::ObjectExpression(_) => "ObjectExpression",
        Expression::SequenceExpression(_) => "SequenceExpression",
        Expression::TemplateLiteral(_) => "TemplateLiteral",
        Expression::TaggedTemplateExpression(_) => "TaggedTemplateExpression",
        Expression::ChainExpression(_) => "ChainExpression",
        Expression::AwaitExpression(_) => "AwaitExpression",
        Expression::YieldExpression(_) => "YieldExpression",
        Expression::UpdateExpression(_) => "UpdateExpression",
        Expression::BigIntLiteral(_) => "BigIntLiteral",
        Expression::RegExpLiteral(_) => "RegExpLiteral",
        Expression::Super(_) => "Super",
        Expression::MetaProperty(_) => "MetaProperty",
        Expression::ImportExpression(_) => "ImportExpression",
        _ => "Other",
    }
}
