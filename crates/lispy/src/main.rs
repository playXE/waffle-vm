use lispy::{compile::Compiler, parser::Parser};
use std::path::PathBuf;
use structopt::StructOpt;
use waffle::reflect::Context;

#[derive(StructOpt)]
struct Opt {
    input: PathBuf,
    out: Option<PathBuf>,
    #[structopt(long = "static", help = "Link statically with all imports")]
    static_: bool,
    #[structopt(long = "disassembly", help = "print bytecode")]
    disasm: bool,
}
fn main() -> Result<(), std::io::Error> {
    let opt = Opt::from_args();
    let r = lexpr::parse::IoRead::new(std::fs::File::open(&opt.input)?);

    let ast = Parser::new(r).parse().unwrap();

    let code = Context::compile(|ctx| {
        let mut compiler = Compiler { ctx };
        for expr in ast.iter() {
            compiler
                .compile_expr(expr, false)
                .map_err(|err| err.to_string())?;
        }

        Ok(())
    });
    match code {
        Ok((globals, ops)) => {
            //
            if opt.disasm && !opt.static_ {
                let str = waffle::reflect::disassembly(&globals, &ops);
                println!("{}", str);
            }
            let bc = waffle::bytecode::write_module(&ops, &globals);
            let out = opt.out.map(|x| x.display().to_string()).unwrap_or_else(|| {
                format!(
                    "{}.waffle",
                    opt.input.file_stem().unwrap().to_str().unwrap().to_string()
                )
            });

            std::fs::write(&out, bc)?;

            if opt.static_ {
                match waffle::linker::link(&[out.clone()]) {
                    Ok((globals, ops)) => {
                        if opt.disasm {
                            let str = waffle::reflect::disassembly(&globals, &ops);
                            println!("{}", str);
                            let bc = waffle::bytecode::write_module(&ops, &globals);
                            std::fs::write(&out, bc)?;
                        }
                    }
                    Err(e) => {
                        eprintln!("linking failed: {}", e);
                    }
                }
            }
        }
        Err(e) => eprintln!("compilation failed: {}", e),
    }
    Ok(())
}
