use oxc_allocator::Allocator;
use oxc_ast::AstBuilder;
use oxc_ast::ast::{
    BinaryExpression, ComputedMemberExpression, Expression, Program, UnaryExpression,
};
use oxc_ast_visit::{VisitMut, walk_mut};
use oxc_span::SPAN;
use oxc_syntax::number::NumberBase;
use oxc_syntax::operator::{BinaryOperator, UnaryOperator};

use super::Transformer;

pub struct ConstFolder;

impl ConstFolder {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ConstFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> Transformer<'a> for ConstFolder {
    fn name(&self) -> &'static str {
        "const_folder"
    }

    fn transform(&mut self, allocator: &'a Allocator, program: &mut Program<'a>) {
        let mut v = ConstFoldVisitor { allocator };
        v.visit_program(program);
    }
}

struct ConstFoldVisitor<'a> {
    allocator: &'a Allocator,
}

impl<'a> VisitMut<'a> for ConstFoldVisitor<'a> {
    fn visit_expression(&mut self, it: &mut Expression<'a>) {
        walk_mut::walk_expression(self, it);
        if let Some(val) = eval(it) {
            if let Some(new_expr) = val.into_expr(self.allocator) {
                *it = new_expr;
            }
        }
    }
}

#[derive(Clone)]
enum Value {
    Null,
    Undefined,
    Bool(bool),
    Number(f64),
    String(String),
    Window,
}

impl Value {
    fn into_expr<'a>(self, allocator: &'a Allocator) -> Option<Expression<'a>> {
        let b = AstBuilder::new(allocator);
        Some(match self {
            Value::Null => b.expression_null_literal(SPAN),
            Value::Undefined => b.expression_identifier(SPAN, "undefined"),
            Value::Bool(v) => b.expression_boolean_literal(SPAN, v),
            Value::Number(n) => b.expression_numeric_literal(SPAN, n, None, NumberBase::Decimal),
            Value::String(s) => {
                let interned: &'a str = allocator.alloc_str(&s);
                b.expression_string_literal(SPAN, interned, None)
            }
            Value::Window => return None,
        })
    }
}

fn eval(expr: &Expression<'_>) -> Option<Value> {
    match expr {
        Expression::NullLiteral(_) => Some(Value::Null),
        Expression::BooleanLiteral(b) => Some(Value::Bool(b.value)),
        Expression::NumericLiteral(n) => Some(Value::Number(n.value)),
        Expression::StringLiteral(s) => Some(Value::String(s.value.to_string())),
        Expression::Identifier(id) if id.name == "undefined" => Some(Value::Undefined),
        Expression::Identifier(id) if id.name == "window" => Some(Value::Window),
        Expression::UnaryExpression(u) => eval_unary(u),
        Expression::BinaryExpression(b) => eval_binary(b),
        Expression::ParenthesizedExpression(p) => eval(&p.expression),
        Expression::ComputedMemberExpression(m) => eval_member(m),
        _ => None,
    }
}

fn eval_member(m: &ComputedMemberExpression<'_>) -> Option<Value> {
    let Value::String(s) = eval(&m.object)? else {
        return None;
    };
    let n = to_number(&eval(&m.expression)?);
    if !n.is_finite() || n.fract() != 0.0 || n < 0.0 {
        return None;
    }
    let unit = s.encode_utf16().nth(n as usize)?;
    Some(Value::String(String::from_utf16_lossy(&[unit])))
}

fn eval_unary(u: &UnaryExpression<'_>) -> Option<Value> {
    let v = eval(&u.argument)?;
    Some(match u.operator {
        UnaryOperator::LogicalNot => Value::Bool(!to_bool(&v)),
        UnaryOperator::UnaryPlus => Value::Number(to_number(&v)),
        UnaryOperator::UnaryNegation => safe_num(-to_number(&v))?,
        UnaryOperator::BitwiseNot => Value::Number((!to_int32(&v)) as f64),
        UnaryOperator::Void => Value::Undefined,
        _ => return None,
    })
}

fn eval_binary(b: &BinaryExpression<'_>) -> Option<Value> {
    let l = eval(&b.left)?;
    let r = eval(&b.right)?;
    let string_like = matches!(l, Value::String(_) | Value::Window)
        || matches!(r, Value::String(_) | Value::Window);
    if string_like && !matches!(b.operator, BinaryOperator::Addition) {
        return None;
    }
    Some(match b.operator {
        BinaryOperator::Addition => {
            if string_like {
                Value::String(format!("{}{}", to_string(&l), to_string(&r)))
            } else {
                safe_num(to_number(&l) + to_number(&r))?
            }
        }
        BinaryOperator::Subtraction => safe_num(to_number(&l) - to_number(&r))?,
        BinaryOperator::Multiplication => safe_num(to_number(&l) * to_number(&r))?,
        BinaryOperator::Division => safe_num(to_number(&l) / to_number(&r))?,
        BinaryOperator::Remainder => safe_num(to_number(&l) % to_number(&r))?,
        BinaryOperator::Exponential => safe_num(to_number(&l).powf(to_number(&r)))?,
        BinaryOperator::ShiftLeft => Value::Number(i32::wrapping_shl(to_int32(&l), to_uint32(&r) & 31) as f64),
        BinaryOperator::ShiftRight => Value::Number(i32::wrapping_shr(to_int32(&l), to_uint32(&r) & 31) as f64),
        BinaryOperator::ShiftRightZeroFill => Value::Number(u32::wrapping_shr(to_uint32(&l), to_uint32(&r) & 31) as f64),
        BinaryOperator::BitwiseOR => Value::Number((to_int32(&l) | to_int32(&r)) as f64),
        BinaryOperator::BitwiseXOR => Value::Number((to_int32(&l) ^ to_int32(&r)) as f64),
        BinaryOperator::BitwiseAnd => Value::Number((to_int32(&l) & to_int32(&r)) as f64),
        BinaryOperator::Equality => Value::Bool(loose_eq(&l, &r)),
        BinaryOperator::Inequality => Value::Bool(!loose_eq(&l, &r)),
        BinaryOperator::StrictEquality => Value::Bool(strict_eq(&l, &r)),
        BinaryOperator::StrictInequality => Value::Bool(!strict_eq(&l, &r)),
        BinaryOperator::LessThan => Value::Bool(to_number(&l) < to_number(&r)),
        BinaryOperator::LessEqualThan => Value::Bool(to_number(&l) <= to_number(&r)),
        BinaryOperator::GreaterThan => Value::Bool(to_number(&l) > to_number(&r)),
        BinaryOperator::GreaterEqualThan => Value::Bool(to_number(&l) >= to_number(&r)),
        _ => return None,
    })
}

fn to_bool(v: &Value) -> bool {
    match v {
        Value::Null | Value::Undefined => false,
        Value::Bool(b) => *b,
        Value::Number(n) => *n != 0.0 && !n.is_nan(),
        Value::String(s) => !s.is_empty(),
        Value::Window => true,
    }
}

fn to_number(v: &Value) -> f64 {
    match v {
        Value::Null => 0.0,
        Value::Undefined => f64::NAN,
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        Value::Number(n) => *n,
        Value::String(s) => {
            let t = s.trim();
            if t.is_empty() { 0.0 } else { t.parse::<f64>().unwrap_or(f64::NAN) }
        }
        Value::Window => f64::NAN,
    }
}

fn to_string(v: &Value) -> String {
    match v {
        Value::Null => "null".to_string(),
        Value::Undefined => "undefined".to_string(),
        Value::Bool(true) => "true".to_string(),
        Value::Bool(false) => "false".to_string(),
        Value::Number(n) => format_num(*n),
        Value::String(s) => s.clone(),
        Value::Window => "[object Window]".to_string(),
    }
}

fn format_num(n: f64) -> String {
    if n.is_nan() { return "NaN".to_string(); }
    if n.is_infinite() { return if n > 0.0 { "Infinity" } else { "-Infinity" }.to_string(); }
    if n.fract() == 0.0 && n.abs() < 1e21 {
        return format!("{}", n as i64);
    }
    format!("{}", n)
}

fn to_int32(v: &Value) -> i32 {
    let n = to_number(v);
    if !n.is_finite() { return 0; }
    n as i64 as i32
}

fn to_uint32(v: &Value) -> u32 {
    let n = to_number(v);
    if !n.is_finite() { return 0; }
    n as i64 as u32
}

fn strict_eq(l: &Value, r: &Value) -> bool {
    match (l, r) {
        (Value::Null, Value::Null) | (Value::Undefined, Value::Undefined) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Number(a), Value::Number(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Window, Value::Window) => true,
        _ => false,
    }
}

fn loose_eq(l: &Value, r: &Value) -> bool {
    match (l, r) {
        (Value::Null, Value::Null | Value::Undefined)
        | (Value::Undefined, Value::Null | Value::Undefined) => true,
        _ => strict_eq(l, r),
    }
}

fn safe_num(n: f64) -> Option<Value> {
    n.is_finite().then_some(Value::Number(n))
}
