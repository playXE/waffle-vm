use std::path::PathBuf;

use clap::Parser;
use wafflec::compile::{compile, CompileOpts};

#[derive(Parser)]
#[clap(author, version, about)]
pub struct Opt {
    input: PathBuf,
    output: PathBuf,
}

fn main() -> Result<(), std::io::Error> {
    let mut opts = CompileOpts::parse();
    let input = opts.input.clone();
    if let Ok(dir) = std::env::current_dir() {
        opts.path.push(dir.display().to_string());
    }
    if let Ok(var) = std::env::var("WAFFLEPATH") {
        for path in std::env::split_paths(&var) {
            opts.path.push(path.display().to_string());
        }
    }
    if let Some(p) = input.parent() {
        opts.path.push(p.display().to_string());
    }
    let mut src = String::new();
    match compile(opts, &mut src) {
        Ok(_) => (),
        Err(e) => e
            .report(&input.display().to_string(), &src)
            .finish()
            .eprint(ariadne::sources(vec![(input.display().to_string(), src)]))
            .unwrap(),
    }
    Ok(())
}
