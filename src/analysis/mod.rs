pub mod extractors;
pub mod opcodes;
pub mod transformers;

use oxc_allocator::Allocator;
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
        let (chosen, _) = extractors::extract_opcode_array(&program)
            .ok_or("state var extraction failed: no array literal found in the main IIFE")?;
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

    let bytecode = extractors::extract_bytecode(&program)?;
    let mut opcode_map = opcodes::extract(&program, source, allocator)
        .ok_or_else(|| "opcode extraction failed".to_string())?;
    opcode_map.state_var = original_state_var;

    Ok(AnalysisResult {
        opcode_map,
        bytecode,
        code,
    })
}
