use std::{collections::HashMap, fmt::Display, rc::Rc};

use crate::{load::MODULE_PATH, opcode::Op, reflect::Global};

#[derive(Default)]
pub struct Context {
    globals: Vec<Rc<Global>>,
    opcodes: Vec<Op>,
    loaded: HashMap<String, Option<usize>>,
}

fn path() -> Vec<String> {
    let path = match std::env::var("WAFFLEPATH") {
        Ok(p) => p,
        Err(_) => format!(
            "{},/usr/local/lib/waffle,/usr/lib/waffle,/usr/local/bin,/usr/bin",
            MODULE_PATH
        ),
    };

    let paths = path.split(",").map(|x| x.to_string()).collect::<Vec<_>>();
    paths
}

pub fn file_open(paths: &[String], file: &str) -> Result<Vec<u8>, std::io::Error> {
    if let Ok(file) = std::fs::read(file) {
        return Ok(file);
    }

    for path in paths {
        let path = format!("{}/{}", path, file);
        if let Ok(file) = std::fs::read(path) {
            return Ok(file);
        }
    }

    Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        format!("file `{}` not found", file),
    ))
}

pub fn do_link(
    paths: &[String],
    ctx: &mut Context,
    module: &str,
) -> Result<usize, Box<dyn std::error::Error>> {
    let p = if module.ends_with(".waffle") {
        module.to_string()
    } else {
        format!("{}.waffle", module)
    };
    let ch = file_open(paths, &p)?;
    let (ops, globals) = crate::bytecode::read_module(&ch);

    let mut funcs = vec![];
    let gtbl = globals
        .iter()
        .map(|g| match &**g {
            Global::Var(_) => {
                let k = ctx.globals.len();
                ctx.globals.push(g.clone());
                k
            }
            Global::Func(p, nargs) => {
                let k = ctx.globals.len();
                ctx.globals.push(g.clone());
                funcs.push((*p, *nargs, k));
                k
            }
            Global::Upval(_) => {
                let k = ctx.globals.len();
                ctx.globals.push(g.clone());
                k
            }
            Global::Str(_) | Global::Float(_) | Global::Int(_) | Global::Symbol(_) => {
                if let Some((ix, _)) = ctx.globals.iter().enumerate().find(|(_, x)| &**x == g) {
                    ix
                } else {
                    let k = ctx.globals.len();
                    ctx.globals.push(g.clone());
                    k
                }
            }
        })
        .collect::<Vec<_>>();

    // Initialize module object
    let mid = {
        let new = Global::Symbol("new".to_string().into_boxed_str());
        let n = ctx
            .globals
            .iter()
            .enumerate()
            .find(|(_, g)| &***g == &new)
            .map(|x| x.0)
            .unwrap();
        let mid = ctx.globals.len();

        ctx.globals
            .push(Rc::new(Global::Var(module.to_string().into_boxed_str())));

        ctx.opcodes.extend_from_slice(&[
            Op::AccNull,
            Op::Push,
            Op::AccBuiltin(n as _),
            Op::Call(1),
            Op::SetGlobal(mid as _),
        ]);
        mid
    };
    ctx.loaded.insert(module.to_string(), None);
    let nops = ops.len();
    let mut opmap = vec![-1; nops];
    let mut jumps = vec![];

    let mut jump = |ctx: &mut Context, mkop: fn(i32) -> Op, p, i| {
        let k = ctx.opcodes.len();
        ctx.opcodes.push(Op::Jump(0));
        jumps.push((mkop, k, p, i));
    };

    let load = Global::Symbol("loader".to_string().into_boxed_str());
    let loadm = Global::Symbol("loadmodule".to_string().into_boxed_str());
    let mut p = 0;
    'link: while p < ops.len() {
        match &ops[p..] {
            [Op::AccGlobal(str), Op::Push, Op::AccBuiltin(l), Op::Push, Op::AccBuiltin(l2), Op::Push, Op::AccField(f), Op::ObjCall(2), ..]
                if {
                    matches!(*globals[*str as usize], Global::Str(_))
                        && *globals[*l as usize] == load
                        && *globals[*l2 as usize] == load
                        && *globals[*f as usize] == loadm
                } =>
            {
                match &*globals[*str as usize] {
                    Global::Str(s) => {
                        let mid = match ctx.loaded.get(&**s).copied() {
                            Some(Some(mid)) => mid,
                            Some(None) => return Err(Box::new(RecursiveLoading(s.clone()))),
                            None => do_link(paths, ctx, &**s)?,
                        };
                        opmap[p] = ctx.opcodes.len() as isize;
                        ctx.opcodes.push(Op::AccGlobal(mid as _));
                        p += 8;
                        continue 'link;
                    }
                    _ => unreachable!(),
                }
            }
            [x, ..] => {
                opmap[p] = ctx.opcodes.len() as isize;
                match *x {
                    Op::AccBuiltin(x) => {
                        let g = ctx.globals[gtbl[x as usize]].clone();
                        match &*g {
                            Global::Symbol(ref x) if &**x == "exports" => {
                                ctx.opcodes.push(Op::AccGlobal(mid as _));
                            }
                            _ => {
                                ctx.opcodes.push(Op::AccBuiltin(gtbl[x as usize] as _));
                            }
                        }
                    }
                    Op::AccGlobal(g) => {
                        ctx.opcodes.push(Op::AccGlobal(gtbl[g as usize] as _));
                    }
                    Op::SetGlobal(g) => {
                        ctx.opcodes.push(Op::SetGlobal(gtbl[g as usize] as _));
                    }
                    Op::SetField(g) => {
                        ctx.opcodes.push(Op::SetField(gtbl[g as usize] as _));
                    }
                    Op::AccField(g) => {
                        ctx.opcodes.push(Op::AccField(gtbl[g as usize] as _));
                    }
                    Op::Jump(i) => jump(ctx, Op::Jump, p, i),
                    Op::JumpIf(i) => jump(ctx, Op::JumpIf, p, i),
                    Op::JumpIfNot(i) => jump(ctx, Op::JumpIfNot, p, i),
                    Op::Trap(i) => jump(ctx, Op::Trap, p, i),
                    _ => ctx.opcodes.push(*x),
                }
            }
            _ => unreachable!(),
        }

        p += 1;
    }

    for (op, k, p, i) in jumps {
        let ik = opmap[(p as isize + i as isize) as usize];

        ctx.opcodes[k] = op(ik as i32);
    }

    for (p, nargs, k) in funcs {
        ctx.globals[k] = Rc::new(Global::Func(opmap[p as usize] as _, nargs as _));
    }

    ctx.loaded.insert(module.to_string(), Some(mid));
    ctx.opcodes.push(Op::AccGlobal(mid as _));

    Ok(mid)
}

#[derive(Debug)]
pub struct RecursiveLoading(Box<str>);

impl Display for RecursiveLoading {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "recursive loading of `{}` module", self.0)
    }
}

impl std::error::Error for RecursiveLoading {}

pub fn link(modules: &[String]) -> Result<(Vec<Rc<Global>>, Vec<Op>), Box<dyn std::error::Error>> {
    let mut ctx = Context::default();
    ctx.globals
        .push(Rc::new(Global::Symbol("new".to_string().into_boxed_str())));
    let p = path();
    for module in modules {
        do_link(&p, &mut ctx, &*module)?;
    }

    Ok((ctx.globals, ctx.opcodes))
}
