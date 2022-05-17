use std::path::PathBuf;

use cc::compile::Compiler;
use clap::Parser;
use waffle::reflect::Context;
#[derive(Parser)]
#[clap(author, version, about)]
pub struct Opt {
    input: PathBuf,
    output: PathBuf,
}

fn main() -> Result<(), std::io::Error> {
    let opt = Opt::parse();

    let file = std::fs::read_to_string(&opt.input)?;
    let mut parser = cc::parser::Parser::new(&file);
    match parser.parse_program() {
        Ok(exprs) => {
            let (globals, code) = Context::compile(|ctx| {
                let mut cc = Compiler {
                    ctx,
                    toplevel: true,
                };
                for expr in exprs {
                    println!("{}", expr);
                    cc.expr(&expr, false);
                }
                Ok(())
            })
            .unwrap();
            let str = waffle::reflect::disassembly(&globals, &code);
            println!("{}", str);
            let bc = waffle::bytecode::write_module(&code, &globals);
            std::fs::write(&opt.output, bc)?;
        }
        Err(e) => {
            e.report(opt.input.display().to_string(), &file);
        }
    }
    Ok(())
}
