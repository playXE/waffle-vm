use std::{panic::AssertUnwindSafe, path::PathBuf};

use waffle::{
    gc_frame, load::waffle_default_loader, memory::minimark::MemoryError, value::Value, vm::VM,
};

use clap::Parser;

/// Simple bytecode virtual machine
#[derive(Parser, Debug)]
#[clap(author,about,long_about = None)]
pub enum Cli {
    Run {
        file: PathBuf,
    },
    Link {
        modules: Vec<String>,
        #[clap(long, short)]
        output: PathBuf,
    },
}

fn main() -> Result<(), String> {
    let cli = Cli::parse();
    match cli {
        Cli::Run { file: fname } => {
            let mut vm = VM::new(None);

            let mut mload = waffle_default_loader(&mut vm);
            let mut args = [Value::Null, mload];

            gc_frame!(vm.gc().roots() => args: [Value;2],mload: Value);

            args[0] = Value::Str(vm.gc().str(fname.display().to_string()));

            let key = Value::Symbol(vm.intern("loadmodule"));
            let f = mload.field(vm, &key);

            let mut exc = None;
            let res = std::panic::catch_unwind(AssertUnwindSafe(|| {
                unsafe {
                    vm.callex(mload.get(), f, &args.get(), &mut exc);
                }
                if let Some(exc) = exc {
                    eprintln!("exception thrown: {}", exc);
                }
            }));

            match res {
                Ok(_) => {}
                Err(e) if e.is::<MemoryError>() => {
                    eprintln!("out of memory");
                }
                Err(e) => std::panic::resume_unwind(e),
            }
        }
        Cli::Link { modules, output } => {
            let (globals, ops) = waffle::linker::link(&modules).map_err(|err| err.to_string())?;
            //  println!("{}", waffle::reflect::disassembly(&globals, &ops));
            let raw = waffle::bytecode::write_module(&ops, &globals);

            std::fs::write(output, raw).map_err(|e| e.to_string())?;
        }
    }
    Ok(())
}
