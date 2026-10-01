use base64::prelude::{BASE64_STANDARD, Engine};
use oxc_ast::ast::{Expression, Program, Statement};

pub fn extract_bytecode(program: &Program<'_>) -> Result<Vec<u16>, String> {
    let b64 = find_atob_string(program)
        .ok_or("bytecode extraction failed: no window.atob(\"...\") string found in the main IIFE")?;
    let bytes = BASE64_STANDARD
        .decode(b64)
        .map_err(|e| format!("bytecode extraction failed: invalid base64: {e}"))?;
    if bytes.len() % 2 != 0 {
        return Err(format!(
            "bytecode extraction failed: decoded length {} is odd, expected whole u16 words",
            bytes.len()
        ));
    }
    Ok(bytes
        .chunks_exact(2)
        .map(|c| u16::from_le_bytes([c[0], c[1]]))
        .collect())
}

fn find_atob_string<'a>(program: &'a Program<'_>) -> Option<&'a str> {
    let Statement::ExpressionStatement(es) = program.body.first()? else {
        return None;
    };
    let Expression::CallExpression(outer) = &es.expression else {
        return None;
    };
    let Expression::FunctionExpression(func) = outer.callee.get_inner_expression() else {
        return None;
    };
    let body = func.body.as_ref()?;
    body.statements.iter().find_map(extract_from_stmt)
}

fn extract_from_stmt<'a>(stmt: &'a Statement<'_>) -> Option<&'a str> {
    let Statement::VariableDeclaration(decl) = stmt else {
        return None;
    };
    decl.declarations
        .iter()
        .find_map(|d| d.init.as_ref().and_then(is_atob_call))
}

fn is_atob_call<'a>(expr: &'a Expression<'_>) -> Option<&'a str> {
    let Expression::CallExpression(call) = expr else {
        return None;
    };
    let Expression::StaticMemberExpression(mem) = call.callee.get_inner_expression() else {
        return None;
    };
    let Expression::Identifier(obj) = &mem.object else {
        return None;
    };
    if obj.name != "window" || mem.property.name != "atob" {
        return None;
    }
    let arg = call.arguments.first()?;
    let Expression::StringLiteral(s) = arg.as_expression()? else {
        return None;
    };
    Some(s.value.as_str())
}
