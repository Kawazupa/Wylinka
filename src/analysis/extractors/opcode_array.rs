use oxc_ast::ast::{ArrayExpression, BindingPattern, Expression, Program, Statement};

pub fn extract_opcode_array<'p>(
    program: &'p Program<'p>,
) -> Option<(&'p str, &'p ArrayExpression<'p>)> {
    let Statement::ExpressionStatement(stmt) = program.body.first()? else {
        return None;
    };
    let Expression::CallExpression(call) = &stmt.expression else {
        return None;
    };
    let Expression::FunctionExpression(func) = call.callee.get_inner_expression() else {
        return None;
    };
    let body = func.body.as_ref()?;

    let mut best: Option<(&'p str, &'p ArrayExpression<'p>)> = None;
    for s in body.statements.iter() {
        let Statement::VariableDeclaration(decl) = s else {
            continue;
        };
        for d in decl.declarations.iter() {
            let Some(Expression::ArrayExpression(arr)) = &d.init else {
                continue;
            };
            let BindingPattern::BindingIdentifier(id) = &d.id else {
                continue;
            };
            let len = arr.elements.len();
            if best.map_or(true, |(_, b)| len > b.elements.len()) {
                best = Some((id.name.as_str(), arr.as_ref()));
            }
        }
    }
    best
}
