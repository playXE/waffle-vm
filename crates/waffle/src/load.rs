use std::{
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use crate::{
    builtin::make_prim,
    gc_frame,
    memory::gcwrapper::{Array, Gc, Nullable},
    opcode::Op,
    reflect::Global,
    value::{Function, Module, Obj, Value},
    vm::{Id, Lib, LibList, VM},
};

pub fn read_module(vm: &mut VM, ops: &[Op], globals: &[Rc<Global>], loader: Value) -> Gc<Module> {
    unsafe {
        let mut arr = vm.gc().array(globals.len(), Value::Null);

        let module = vm
            .gc()
            .malloc_varsize::<Module>(ops.len() + 1, &mut [&mut arr]);
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
        module.code.as_mut_ptr().add(ops.len()).write(Op::Leave);
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
                        x => unreachable!("{:?}", x),
                    }
                }
                _ => (),
            }
        }
        module.get()
    }
}

fn select_file(path: &[Value], mname: &str, ext: &str) -> Option<PathBuf> {
    if Path::new(&format!("{}{}", mname, ext)).exists() {
        return Some(PathBuf::from_str(&format!("{}{}", mname, ext)).unwrap());
    }
    for path in path {
        match PathBuf::from_str(&format!("{}", path)) {
            Ok(mut path) => {
                path.push(mname);
                path.push(ext);
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
    let fname = if mname.ends_with(".waffle") {
        select_file(path, mname, "")
    } else {
        select_file(path, mname, ".waffle")
    }
    .ok_or_else(|| std::io::Error::new(std::io::ErrorKind::NotFound, "file does not exist"))?;

    let f = std::fs::read(fname)?;

    Ok(f)
}

pub extern "C" fn load_module(vm: &mut VM, mname: &Value, vthis: &Value) -> Value {
    let mut o = vm.vthis;
    let mut cache = Value::Null;
    let mut scache = Value::Symbol(vm.id(Id::Cache));
    let mut spath = Value::Symbol(vm.id(Id::Path));

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

#[cfg(all(
    target_family = "unix",
    not(target_os = "macos"),
    not(target_os = "ios")
))]
const DYNLIB_POSTFIX: &'static str = ".so";
#[cfg(any(target_os = "macos", target_os = "ios"))]
const DYNLIB_POSTFIX: &'static str = ".dylib";
#[cfg(windows)]
const DYNLIB_POSTFIX: &'static str = ".dll";

pub fn load_primitive(
    vm: &mut VM,
    liblist: &mut LibList,
    path: &[Value],
    prim: &Value,
    nargs: &Value,
) -> usize {
    let nargs = if let Value::Int(nargs) = nargs {
        if *nargs < 0 {
            -1i32
        } else {
            *nargs as i32
        }
    } else {
        vm.throw_str("`loadprimitive` expects number of arguments to be passed as integer")
    };

    let prim = if let Value::Str(str) = prim {
        str.to_string()
    } else {
        vm.throw_str("`loadprimitive` expects string as primitive name");
    };

    let pos = prim.find("@").unwrap_or_else(|| {
        vm.throw_str(&format!(
            "primitive name `{}` does not have `@` as separator",
            prim
        ))
    });

    let (dll, prim) = prim.split_at(pos);
    let mut lib = None;
    {
        for l in liblist.v.iter() {
            if l.name == dll {
                lib = Some(&l.handle);
                break;
            }

            todo!()
        }
        if lib.is_none() {
            let file = select_file(path, dll, DYNLIB_POSTFIX)
                .unwrap_or_else(|| vm.throw_str(&format!("dynlib `{}` not found", dll)));

            let handle = libloading_mini::Library::new(&file).unwrap_or_else(|| {
                vm.throw_str(&format!("failed to open dynlib `{}`", file.display()))
            });

            let ptr = handle.get("__waffle_entry_point".as_bytes());
            if let Some(ptr) = ptr {
                unsafe {
                    let ptr = std::mem::transmute::<_, extern "C" fn(&mut VM)>(ptr);
                    ptr(vm);
                }
            }
            liblist.v.push(Lib {
                handle,
                name: dll.to_string(),
            });

            lib = Some(&liblist.v.last().unwrap().handle);
        }
    }

    // invoek function that actually returns pointer to a primitive
    let lib = lib.unwrap();
    let name = if nargs == -1 {
        format!("{}__MULT", &prim[1..])
    } else {
        format!("{}__{}", &prim[1..], nargs)
    };
    let sym = lib
        .get(name.as_bytes())
        .unwrap_or_else(|| vm.throw_str(&format!("symbol `{}` not found", name)));
    unsafe {
        let ptr = std::mem::transmute::<_, extern "C" fn() -> usize>(sym);
        return ptr();
    }
}

pub extern "C" fn loader_loadprim(vm: &mut VM, prim: &Value, nargs: &Value) -> Value {
    let o = vm.vthis;
    let id = vm.id(Id::Libs);
    let libs = o.field(vm, &Value::Symbol(id));
    let mut libs = match libs {
        Value::Abstract(x) => match x.downcast::<LibList>() {
            Some(liblist) => liblist,
            _ => vm.throw_str("library list malformed"),
        },
        _ => vm.throw_str("library list malformed"),
    };

    let spath = Value::Symbol(vm.id(Id::Path));
    let mut path = o.field(vm, &spath).array();

    gc_frame!(vm.gc().roots() =>libs: Gc<LibList>,path: Gc<Array<Value>>);

    let ptr = load_primitive(vm, &mut libs, &path, prim, nargs);
    let f = nargs.int();
    let nargs = if f < 0 { 0 } else { f as u32 };
    let varsize = f < 0;
    let f = vm.gc().fixed(Function {
        nargs,
        varsize,
        env: Value::Null,
        module: Nullable::NULL,
        addr: ptr,
    });
    Value::Primitive(f)
}

pub const MODULE_PATH: &'static str = "~/.waffle";

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
    let mut f = make_prim(vm, loader_loadprim as _, 2, false);
    let mut s = Value::Symbol(vm.intern("loadprim"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    loader.set_field(vm, &s, &f);

    let mut liblist = Value::Abstract(vm.gc().fixed(LibList { v: vec![] }).as_dyn());
    let mut id = Value::Symbol(vm.id(Id::Libs));

    gc_frame!(vm.gc().roots() => liblist: Value,id: Value);
    loader.set_field(vm, &id, &liblist);
    loader.get()
}
