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

/*
pub struct Compiler<'a, 'b> {
    ctx: &'b mut Context<'a>,
}

impl<'a, 'b> Compiler<'a, 'b> {
    pub fn compile<'c, P: lexpr::parse::Read<'c>>(
        mut parser: lexpr::Parser<P>,
    ) -> Result<(Vec<Rc<Global>>, Vec<Op>), String> {
        Context::compile(|ctx| {
            let mut cc = Compiler { ctx };

            while let Some(exp) = parser.next_value().map_err(|x| x.to_string())? {
                cc.compile_sexp(&exp, false)?;
            }
            drop(cc);
            Ok(())
        })
    }
    pub fn compile_sexp(&mut self, sexp: &Value, tail: bool) -> Result<(), String> {
        match sexp {
            Value::Bool(x) => {
                if *x {
                    self.ctx.write(Op::AccTrue);
                } else {
                    self.ctx.write(Op::AccFalse);
                }
                Ok(())
            }
            Value::Number(x) => {
                if let Some(x) = x.as_i64() {
                    if x as i32 as i64 == x {
                        self.ctx.write(Op::AccInt(x as i32));
                    } else {
                        let g = self.ctx.global(Rc::new(waffle::reflect::Global::Int(x)));
                        self.ctx.write(Op::AccGlobal(g as _));
                    }
                } else if let Some(x) = x.as_f64() {
                    let g = self
                        .ctx
                        .global(Rc::new(waffle::reflect::Global::Float(x.to_bits())));
                    self.ctx.write(Op::AccGlobal(g as _));
                }

                Ok(())
            }
            Value::Symbol(x) => {
                if x.starts_with('$') {
                    let mut acc_builtin = || {
                        let x = self
                            .ctx
                            .global(Rc::new(Global::Symbol(x[1..].to_string().into_boxed_str())));
                        self.ctx.write(Op::AccBuiltin(x as _));

                        Ok(())
                    };
                    match &**x {
                        "$amake" | "$array" | "$asize" | "$minorGC" | "$fullGC" | "$throw" => {
                            return acc_builtin()
                        }

                        _ => (),
                    }
                }
                self.ctx.use_var(x);
                Ok(())
            }
            Value::Cons(x) => {
                return self.compile_application(x.car(), x.cdr(), tail);
            }
            Value::String(x) => {
                let x = self.ctx.global(Rc::new(Global::Str(x.clone())));
                self.ctx.write(Op::AccGlobal(x as _));
                Ok(())
            }

            _ => todo!("{}", sexp),
        }
    }

    fn compile_application(
        &mut self,
        value: &Value,
        params: &Value,
        tail: bool,
    ) -> Result<(), String> {
        match value {
            Value::Symbol(x) => match &**x {
                "begin" => return self.compile_begin(params, tail),
                "let" => return self.compile_let(params, tail),
                "set!" => return self.compile_set(params),
                "$aget" => {
                    let arr = params.as_cons().unwrap().car();
                    self.compile_sexp(arr, false)?;
                    self.ctx.write(Op::Push);
                    let ix = params.as_cons().unwrap().cdr().as_cons().unwrap().car();
                    self.compile_sexp(ix, false)?;

                    self.ctx.write(Op::AccArray);
                    return Ok(());
                }
                "$aset!" => {
                    let rest = params.as_cons().unwrap().cdr().as_cons().unwrap();
                    let ix = rest.car();
                    self.compile_sexp(ix, false)?;
                    self.ctx.write(Op::Push);
                    let arr = params.as_cons().unwrap().car();
                    self.compile_sexp(arr, false)?;
                    self.ctx.write(Op::Push);
                    let val = rest.cdr().as_cons().unwrap().car();
                    self.compile_sexp(val, false)?;
                    self.ctx.write(Op::SetArray);
                }
                "lambda" | "𝛌" | "λ" => {
                    let lambda = params
                        .as_cons()
                        .ok_or_else(|| format!("malformed lambda"))?;
                    let args = lambda.car();
                    let body = lambda.cdr();

                    return self.compile_lambda(args, body);
                }

                "define" => {
                    let define = params
                        .as_cons()
                        .ok_or_else(|| format!("malformed define"))?;
                    let name = define
                        .car()
                        .as_symbol()
                        .ok_or_else(|| format!("malformed define: name expected"))?;
                    let val = define
                        .cdr()
                        .as_cons()
                        .ok_or_else(|| format!("malformed define"))?
                        .car();

                    self.compile_sexp(val, false)?;
                    let gid = self
                        .ctx
                        .global(Rc::new(Global::Var(name.to_string().into_boxed_str())));

                    self.ctx.write(Op::SetGlobal(gid as _));
                    return Ok(());
                }
                "if" => {
                    self.compile_if(params, tail)?;
                    return Ok(());
                }
                "+" | "-" | "*" | "/" | "%" | ">>" | "<<" | ">>>" | "^" | "&" | "|" | "!" | ">"
                | "<" | "=" | "<=" | ">=" => {
                    self.compile_binop(x, params, tail)?;
                    return Ok(());
                }

                _ => {}
            },
            _ => {}
        }
        let mut nargs = 0;

        for param in params
            .list_iter()
            .ok_or_else(|| "application expects cons list of parameters".to_string())?
        {
            nargs += 1;
            self.compile_sexp(param, false)?;
            self.ctx.write(Op::Push);
        }
        self.compile_sexp(value, false)?;
        if tail {
            self.ctx.write(Op::TailCall(
                self.ctx.stack as i16 - self.ctx.limit as i16,
                nargs as i16,
            ));
        } else {
            self.ctx.write(Op::Call(nargs));
        }
        Ok(())
    }
    fn compile_binop(&mut self, ops: &str, params: &Value, tail: bool) -> Result<(), String> {
        let op = str_to_op(ops);
        match op {
            Op::Not => {
                let p = self.expect_pair(params.clone())?.car().clone();
                self.compile_sexp(&p, false)?;
                self.ctx.write(Op::Not);
                Ok(())
            }
            _ => {
                if !params.is_list() {
                    return Err(format!("list of operands expected to `{}`", ops));
                }
                let list = params.list_iter().unwrap().cloned().collect::<Vec<Value>>();
                if list.len() == 1 {
                    match op {
                        Op::Add => {
                            self.compile_sexp(&list[0], false)?;
                            self.ctx.write(Op::ToNum);
                        }
                        Op::Sub => {
                            self.compile_sexp(&list[0], false)?;
                            self.ctx.write(Op::Push);
                            self.ctx.write(Op::AccInt(-1));
                            self.ctx.write(Op::Mul);
                        }

                        _ => {
                            // other opcodes just return the same value you passed
                            self.compile_sexp(&list[0], tail)?;
                        }
                    }
                } else if list.len() == 2 {
                    self.compile_sexp(&list[0], false)?;
                    self.ctx.write(Op::Push);
                    self.compile_sexp(&list[1], false)?;
                    self.ctx.write(op);
                } else {
                    todo!()
                }
                Ok(())
            }
        }
    }
    fn expect_pair<'c>(&'c self, val: Value) -> Result<Cons, String> {
        val.as_cons()
            .cloned()
            .ok_or_else(|| format!("pair expected but {} found", val))
    }
    fn compile_if(&mut self, exp: &Value, tail: bool) -> Result<(), String> {
        let exp = self.expect_pair(exp.clone())?;
        let cond = exp.car();
        let then_and_else = self.expect_pair(exp.cdr().clone())?;
        let else_ = then_and_else.cdr().as_cons();

        let then = then_and_else.car();

        self.compile_sexp(cond, false)?;
        let cjmp = self.ctx.cjmp(false);
        self.compile_sexp(then, tail)?;
        match else_ {
            Some(exp) => {
                let jmp = self.ctx.jmp();
                cjmp(self.ctx);
                self.compile_sexp(exp.car(), tail)?;
                jmp(self.ctx);
            }
            None => {
                cjmp(self.ctx);
            }
        }
        Ok(())
    }
    fn compile_lambda(&mut self, args: &Value, body: &Value) -> Result<(), String> {
        let mut params = vec![];
        for key in args
            .list_iter()
            .ok_or_else(|| format!("parameter list expected"))?
        {
            params.push(
                key.as_symbol()
                    .ok_or_else(|| format!("parameter name must be a symbol"))?
                    .to_string(),
            );
        }
        self.ctx.compile_function(&params, |ctx| {
            let mut cc = Compiler { ctx };

            cc.compile_begin(body, true)?;
            Ok(())
        })
    }
    fn compile_begin(&mut self, body: &Value, tail: bool) -> Result<(), String> {
        if body.is_null() {
            return Ok(());
        }

        if let Value::Cons(x) = body {
            if x.cdr().is_null() {
                self.compile_sexp(x.car(), tail)
            } else {
                self.compile_sexp(x.car(), false)?;
                self.compile_begin(x.cdr(), tail)
            }
        } else {
            Err("malformed begin".to_string())
        }
    }

    fn compile_let(&mut self, body: &Value, tail: bool) -> Result<(), String> {
        // iterate ((k v)) pairs
        let body = body.as_cons().ok_or_else(|| "malformed let".to_string())?;
        let defs = body.car();
        let rest = body.cdr();
        let locals = self.ctx.locals.clone();
        let stack = self.ctx.stack;
        for kv in defs
            .list_iter()
            .ok_or_else(|| "malformed let".to_string())?
        {
            let kv = kv.as_cons().ok_or_else(|| "malformed let".to_string())?;
            let key = kv
                .car()
                .as_symbol()
                .ok_or_else(|| "symbol expected as variable name in let".to_string())?;
            let value = kv.cdr().as_cons().unwrap().car();
            self.compile_sexp(value, false)?;
            self.ctx.add_var(key, true);
        }

        self.compile_begin(rest, tail)?;
        if stack < self.ctx.stack {
            self.ctx
                .write(Op::Pop(self.ctx.stack as i32 - stack as i32));
        }
        *self.ctx.locals = locals;

        Ok(())
    }

    fn compile_set(&mut self, exp: &Value) -> Result<(), String> {
        let exp = exp.as_cons().ok_or_else(|| "malformed set!".to_string())?;
        let name = exp
            .car()
            .as_symbol()
            .ok_or_else(|| "malformed set!: name expected".to_string())?;
        let val = exp
            .cdr()
            .as_cons()
            .map(|x| x.car())
            .ok_or_else(|| "malformed set!: value expected".to_string())?;

        self.compile_sexp(val, false)?;
        let acc = self.ctx.access_var(name);
        self.ctx.compile_access_set(&acc);

        Ok(())
    }
}

fn str_to_op(op: &str) -> Op {
    match op {
        "+" => Op::Add,
        "-" => Op::Sub,
        "/" => Op::Div,
        "*" => Op::Mul,
        ">>" => Op::Shr,
        ">>>" => Op::UShr,
        "<<" => Op::Shl,
        "<" => Op::Lt,
        "<=" => Op::Lte,
        "=" => Op::Eq,
        ">" => Op::Gt,
        ">=" => Op::Gte,
        "%" => Op::Mod,
        "!" => Op::Not,
        "^" => Op::Xor,
        "!=" => Op::Neq,
        "|" => Op::Or,
        "&" => Op::And,
        _ => unreachable!(),
    }
}
*/
