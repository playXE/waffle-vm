use lispy::{compile::Compiler, parser::Parser};
use std::path::PathBuf;
use structopt::StructOpt;
use waffle::{
    opcode::Op,
    reflect::{disassembly, Context},
};

#[derive(StructOpt)]
struct Opt {
    input: PathBuf,
    out: PathBuf,
}
fn main() -> Result<(), std::io::Error> {
    let opt = Opt::from_args();
    let r = lexpr::parse::IoRead::new(std::fs::File::open(opt.input)?);

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
        Ok((globals, mut ops)) => {
            ops.push(Op::Leave);
            let str = disassembly(&globals, &ops);
            println!("{}", str);
            let bc = waffle::bytecode::write_module(&ops, &globals);
            std::fs::write(opt.out, bc)?;
        }
        Err(e) => eprintln!("compilation failed: {}", e),
    }
    Ok(())
}
