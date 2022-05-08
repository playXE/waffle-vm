use std::{
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use crate::{
    builtin::make_prim,
    gc_frame,
    memory::gcwrapper::{Array, Gc},
    opcode::Op,
    reflect::Global,
    value::{Function, Module, Obj, Value},
    vm::{Id, VM},
};

pub fn read_module(vm: &mut VM, ops: &[Op], globals: &[Rc<Global>], loader: Value) -> Gc<Module> {
    unsafe {
        let mut arr = vm.gc().array(globals.len(), Value::Null);

        let module = vm.gc().malloc_varsize::<Module>(ops.len(), &mut [&mut arr]);
        module.as_mut_ptr().write(Module {
            name: Value::Null,
            globals: arr.nullable(),
            exports: Value::Null,
            loader,
            code_size: ops.len(),
            code: [],
        });

        let mut module = module.assume_init();
        gc_frame!(vm.gc().roots() => module: Gc<Module>);
        let exports = Obj::new(vm);
        module.exports = Value::Object(exports);
        for (pos, global) in globals.iter().enumerate() {
            let val = match &**global {
                Global::Func(pos, nargs) => {
                    let func = vm.gc().fixed(Function {
                        nargs: if *nargs < 0 {
                            (-(*nargs)) as _
                        } else {
                            *nargs as u32
                        },
                        varsize: *nargs < 0,
                        env: Value::Null,
                        module: module.nullable(),
                        addr: *pos as usize,
                    });
                    Value::Function(func)
                }
                Global::Float(x) => Value::Float(f64::from_bits(*x)),
                Global::Int(x) => Value::Int(*x),
                Global::Str(x) => Value::Str(vm.gc().str(x)),
                Global::Symbol(x) => Value::Symbol(vm.intern(x)),
                Global::Var(_) => Value::Null,
            };
            module.globals[pos] = val;
        }

        core::ptr::copy_nonoverlapping(ops.as_ptr(), module.code.as_mut_ptr(), ops.len());

        for i in 0..module.code_size {
            let op = module[i];
            match op {
                Op::AccBuiltin(ix) => {
                    let f = module.globals[ix as usize];
                    match f {
                        Value::Symbol(x) => match &**x.name {
                            "loader" => module[i] = Op::AccBuiltinResolved(loader),
                            "exports" => module[i] = Op::AccBuiltinResolved(module.exports),
                            _ => {
                                if let Value::Object(builtins) = vm.builtins {
                                    let tmp = builtins.field(vm, &f);
                                    module[i] = Op::AccBuiltinResolved(tmp);
                                }
                            }
                        },
                        _ => unreachable!(),
                    }
                }
                _ => (),
            }
        }
        module.get()
    }
}

fn select_file(path: &[Value], mname: &str, ext: &str) -> Option<PathBuf> {
    println!("{} {}", mname, ext);
    if Path::new(&format!("{}{}", mname, ext)).exists() {
        return Some(PathBuf::from_str(mname).unwrap());
    }
    for path in path {
        match PathBuf::from_str(&format!("{}", path)) {
            Ok(mut path) => {
                path.push(mname);
                path.push(".n");
                if path.exists() {
                    return Some(path);
                }
            }
            Err(_) => {}
        }
    }
    None
}

fn open_module(path: &[Value], mname: &str) -> std::io::Result<Vec<u8>> {
    let fname = if mname.ends_with(".n") {
        select_file(path, mname, "")
    } else {
        select_file(path, mname, ".n")
    }
    .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "file does not exist"))?;
    println!("open {}", fname.display());
    let f = std::fs::read(fname)?;

    Ok(f)
}

pub extern "C" fn load_module(vm: &mut VM, mname: &Value, vthis: &Value) -> Value {
    let mut o = vm.vthis;
    let mut cache = Value::Null;
    let mut scache = Value::Symbol(vm.id(Id::Cache));
    let mut spath = Value::Symbol(vm.id(Id::Path));
    println!("{} {}", mname, vthis);
    gc_frame!(vm.gc().roots() => o: Value,cache: Value,scache: Value,spath: Value );
    cache.set(o.field(vm, &scache));
    {
        let mut mid = Value::Symbol(vm.intern(format!("{}", mname)));
        let mv = cache.field(vm, &mid);
        if let Value::Module(module) = mv {
            return module.exports;
        }
        let path = o.field(vm, &spath).array();

        let code = open_module(&*path, &format!("{}", mname))
            .unwrap_or_else(|err| vm.throw_str(format!("failed to open module: {}", err)));
        let (ops, globals) = crate::bytecode::read_module(&code);
        let mut m = read_module(vm, &ops, &globals, *vthis);
        let mut mv = Value::Module(m);
        gc_frame!(vm.gc().roots() => m: Gc<Module>, mv:Value, mid:Value);
        cache.set_field(vm, &mid, &mv);
        match vm.execute(m.get()) {
            Err(e) => vm.throw(e),
            _ => (),
        }
        m.exports
    }
}

const MODULE_PATH: &'static str = "~/.waffle";

fn get_loader_path(vm: &mut VM) -> Value {
    let path = match std::env::var("WAFFLEPATH") {
        Ok(p) => p,
        Err(_) => format!(
            "{},/usr/local/lib/waffle,/usr/lib/waffle,/usr/local/bin,/usr/bin",
            MODULE_PATH
        ),
    };

    let paths = path.split(",").collect::<Vec<_>>();

    let mut buf = vm.gc().array(paths.len(), Value::Null);
    gc_frame!(vm.gc().roots() => buf: Gc<Array<Value>>);
    for (pos, path) in paths.iter().enumerate() {
        buf[pos] = Value::Str(vm.gc().str(path));
        vm.gc().write_barrier(buf.get());
    }

    Value::Array(buf.get())
}

pub fn waffle_default_loader(vm: &mut VM) -> Value {
    let mut loader = Value::Null;
    let mut args = vm.gc().array(std::env::args().len(), Value::Null);
    gc_frame!(vm.gc().roots() => args: Gc<Array<Value>>,loader: Value);
    for (pos, arg) in std::env::args().enumerate() {
        let arg = vm.gc().str(arg);
        args[pos] = Value::Str(arg);
        vm.gc().write_barrier(args.get());
    }
    loader.set(Value::Object(Obj::new(vm)));

    let path = get_loader_path(vm);
    let pathk = vm.id(Id::Path);
    loader.set_field(vm, &Value::Symbol(pathk), &path);
    let obj = Obj::new(vm);
    let cache = vm.id(Id::Cache);
    loader.set_field(vm, &Value::Symbol(cache), &Value::Object(obj));
    let kargs = vm.intern("args");
    loader.set_field(vm, &Value::Symbol(kargs), &Value::Array(args.get()));
    let mut f = make_prim(vm, load_module as _, 2, false);
    let mut s = Value::Symbol(vm.intern("loadmodule"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    loader.set_field(vm, &s, &f);
    loader.get()
}
