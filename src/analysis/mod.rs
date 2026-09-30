pub mod extractors;
pub mod opcodes;
pub mod transformers;

use oxc_allocator::Allocator;
use oxc_ast::ast::{BindingPattern, Expression, Statement};
use oxc_codegen::Codegen;
use oxc_parser::Parser;
use oxc_span::SourceType;

use crate::semantic::OpcodeMap;
use transformers::Transformer;
use transformers::const_folder::ConstFolder;
use transformers::iife::{IifeFolder, IifeIsolator};
use transformers::local_simplifier::LocalSimplifier;
use transformers::thunk_inliner::ThunkInliner;
use transformers::var_renamer::VarRenamer;

pub struct AnalysisResult<'a> {
    pub opcode_map: OpcodeMap<'a>,
    pub bytecode: Vec<u16>,
    pub code: String,
}

pub fn run<'a>(allocator: &'a Allocator, source: &'a str) -> Result<AnalysisResult<'a>, String> {
    let ret = Parser::new(allocator, source, SourceType::default()).parse();
    if !ret.errors.is_empty() {
        let msg = ret
            .errors
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()
            .join("\n");
        return Err(format!("parse errors:\n{msg}"));
    }

    let mut program = ret.program;

    {
        let mut iso = IifeIsolator::new();
        iso.transform(allocator, &mut program);
    }

    let original_state_var: &'a str = {
        let mut name: Option<&str> = None;
        if let Some((s, _)) = extractors::extract_opcode_array(&program) {
            name = Some(s);
        } else if let Some(Statement::ExpressionStatement(stmt)) = program.body.first() {
            if let Expression::CallExpression(call) = &stmt.expression {
                if let Expression::FunctionExpression(func) = call.callee.get_inner_expression() {
                    if let Some(body) = func.body.as_ref() {
                        for s in body.statements.iter() {
                            if let Statement::VariableDeclaration(decl) = s {
                                for d in decl.declarations.iter() {
                                    if let Some(Expression::ArrayExpression(_)) = &d.init {
                                        if let BindingPattern::BindingIdentifier(id) = &d.id {
                                            name = Some(id.name.as_str());
                                            break;
                                        }
                                    }
                                }
                            }
                            if name.is_some() {
                                break;
                            }
                        }
                    }
                }
            }
        }
        let chosen = name.unwrap_or("v3");
        allocator.alloc_str(chosen)
    };

    let mut pipeline: Vec<Box<dyn Transformer<'a>>> = vec![
        Box::new(VarRenamer::new()),
        Box::new(IifeFolder::new()),
        Box::new(ThunkInliner::new()),
        Box::new(ConstFolder::new()),
    ];
    for t in pipeline.iter_mut() {
        t.transform(allocator, &mut program);
    }

    let mut cleanup: Vec<Box<dyn Transformer<'a>>> = vec![
        Box::new(LocalSimplifier::new()),
        Box::new(IifeFolder::new()),
        Box::new(ThunkInliner::new()),
        Box::new(ConstFolder::new()),
    ];
    let mut code = Codegen::new().build(&program).code;
    for _ in 0..8 {
        for t in cleanup.iter_mut() {
            t.transform(allocator, &mut program);
        }
        let next = Codegen::new().build(&program).code;
        if next == code {
            break;
        }
        code = next;
    }

    let bytecode = extractors::extract_bytecode(&program)
        .ok_or_else(|| "bytecode extraction failed".to_string())?;
    let mut opcode_map = opcodes::extract(&program, source, allocator)
        .ok_or_else(|| "opcode extraction failed".to_string())?;
    opcode_map.state_var = original_state_var;

    Ok(AnalysisResult {
        opcode_map,
        bytecode,
        code,
    })
}
