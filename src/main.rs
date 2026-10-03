pub mod analysis;
pub mod processing;
pub mod semantic;

use std::fs;
use std::path::{Path, PathBuf};
use std::process::exit;

use clap::Parser;
use oxc_allocator::Allocator;

use processing::{flatten, fold, ingest};

#[derive(Parser)]
#[command(name = "wylinka", about = "devirt imperva reese84 vm")]
struct Cli {
    #[arg(default_value = "input/one.js")]
    files: Vec<PathBuf>,

    #[arg(short, long, default_value = "output")]
    out_dir: PathBuf,
}

fn main() {
    std::thread::Builder::new()
        .stack_size(1 << 30)
        .spawn(run)
        .expect("spawn")
        .join()
        .expect("join");
}

fn run() {
    let cli = Cli::parse();

    let mut failed = false;
    for src in &cli.files {
        let stem = src.file_stem().and_then(|s| s.to_str()).unwrap_or("out");
        let output = cli.out_dir.join(format!("{stem}.clean.js"));
        if let Err(e) = analyze(src, &output) {
            eprintln!("{}: {e}", src.display());
            failed = true;
        }
    }
    if failed {
        exit(1);
    }
}

fn analyze(src: &Path, output: &Path) -> Result<(), String> {
    let source = fs::read_to_string(src).map_err(|e| format!("read {}: {e}", src.display()))?;

    let allocator = Allocator::default();
    let result = analysis::run(&allocator, &source)?;

    if let Some(parent) = output.parent() {
        if !parent.as_os_str().is_empty() {
            fs::create_dir_all(parent).map_err(|e| format!("mkdir {}: {e}", parent.display()))?;
        }
    }
    fs::write(output, &result.code).map_err(|e| format!("write {}: {e}", output.display()))?;

    let triplets = ingest::parse(&result.bytecode);
    let dropped = result.bytecode.len() % 3;
    if dropped != 0 {
        eprintln!(
            "{}: warning: {dropped} trailing bytecode word(s) do not form a full triplet and were dropped",
            src.display()
        );
    }
    println!("{} -> {} ({}b)", src.display(), output.display(), result.code.len());
    println!("{} instrs, {} triplets", result.bytecode.len(), triplets.len());

    let flat = flatten::run(&allocator, &result.opcode_map, &triplets);
    let flat = fold::run(&allocator, &result.opcode_map, flat);
    let flat_out = output.with_extension("flat.js");
    fs::write(&flat_out, flatten::dump(&flat))
        .map_err(|e| format!("write {}: {e}", flat_out.display()))?;
    println!(
        "{} stmts, {} unhandled, {} folds, {} forces -> {}",
        flat.stmts.len(),
        flat.unhandled,
        flat.folds,
        flat.forces,
        flat_out.display()
    );

    Ok(())
}
