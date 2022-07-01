use std::{
    path::{Path, PathBuf},
    rc::Rc,
    str::FromStr,
};

use crate::{
    builtin::make_prim,
    gc_frame,
    memory::gcwrapper::{Array, Gc, Nullable, Str},
    object::*,
    opcode::Feedback,
    opcode::Op,
    reflect::Global,
    runtime::array::{array_iter, new_array},
    value::{Function, Module, Value},
    vm::{symbol_table, Id, Internable, Lib, LibList, Symbol, VM},
};

pub fn read_module(
    vm: &mut VM,
    ops: &[Op],
    globals: &[Rc<Global>],
    loader: Value,
    feedback: u32,
) -> Gc<Module> {
    unsafe {
        gc_frame!(vm.gc().roots() => arr = vm.gc().array(globals.len(), Value::Null));
        gc_frame!(vm.gc().roots() => fdbk = vm.gc().array(feedback as _, Feedback::None));

        let module = vm.gc().malloc_varsize::<Module>(ops.len() + 1, &mut []);

        module.as_mut_ptr().write(Module {
            name: Value::Null,
            globals: arr.nullable(),
            exports: Value::Null,
            loader,
            feedback: fdbk.nullable(),
            code_size: ops.len() + 1,
            code: [],
        });

        let mut module = module.assume_init();
        gc_frame!(vm.gc().roots() => module: Gc<Module>);
        let exports = Object::new_empty(vm);
        module.exports = Value::new(exports);
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
                        env: Nullable::NULL,
                        module: module.nullable(),
                        addr: *pos as usize,
                        prim: false,
                        construct_struct: Nullable::NULL,
                    });
                    Value::Function(func)
                }
                Global::Float(x) => Value::Float(f64::from_bits(*x)),
                Global::Int(x) => Value::new(*x as i32),
                Global::Str(x) => Value::Str(vm.gc().str(x)),
                Global::Symbol(x) => Value::Symbol(vm.gc().fixed(x.intern())),
                Global::Var(_) => Value::Null,
                Global::Upval(upvals) => {
                    let mut arr = vm.gc().array(upvals.len(), Value::Null);
                    for i in 0..upvals.len() {
                        let (local, ix) = upvals[i];
                        let int = std::mem::transmute::<_, i32>([local as i16, ix as i16]);
                        arr[i] = Value::new(int);
                    }
                    Value::Array(arr)
                }
                Global::Object => {
                    Value::new(Object::new_empty(vm))
                }
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
                        x if x.is_symbol() => match x
                            .downcast_ref::<Symbol>()
                            .unwrap()
                            .description(symbol_table())
                            .as_str()
                        {
                            "loader" => module[i] = Op::AccBuiltinResolved(loader),
                            "exports" => module[i] = Op::AccBuiltinResolved(module.exports),
                            _ => {
                                let b = vm.builtins;
                                let id = f.to_symbol(vm);
                                let tmp = b.field(vm, id);
                                module[i] = Op::AccBuiltinResolved(tmp);
                            }
                        },
                        x => unreachable!("{:?}", x),
                    }
                }
                _ => (),
            }
        }
        module.get_copy()
    }
}

fn select_file(path: &[String], mname: &str, ext: &str) -> Option<PathBuf> {
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

fn open_module(path: &[String], mname: &str) -> std::io::Result<Vec<u8>> {
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
    let scache = vm.id(Id::Cache);
    let spath = vm.id(Id::Path);

    gc_frame!(vm.gc().roots() => o: Value,cache: Value);
    cache.set(o.field(vm, scache));
    {
        let mid = format!("{}", mname).intern();
        let mv = cache.field(vm, mid);
        if let Some(module) = mv.downcast_ref::<Module>() {
            return module.exports;
        }
        let mut path = vec![];
        let arr = o.field(vm, spath);
        array_iter(vm, arr, |_, _, val| {
            path.push(val.to_string());
            true
        });
        let code = open_module(&*path, &format!("{}", mname))
            .unwrap_or_else(|err| vm.throw_str(format!("failed to open module: {}", err)));
        let (ops, globals, feedback) = crate::bytecode::read_module(&code);

        let mut m = read_module(vm, &ops, &globals, *vthis, feedback);
        m.name = Value::Symbol(vm.gc().fixed(mid));
        let mut mv = Value::encode_object_value(m);
        gc_frame!(vm.gc().roots() => m: Gc<Module>, mv:Value);
        cache.set_field(vm, mid, mv.get_copy());
        match vm.execute(m.get_copy()) {
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
    path: &[String],
    prim: &Value,
    nargs: &Value,
) -> usize {
    let nargs = if nargs.is_int32() {
        let nargs = nargs.get_int32();
        if nargs < 0 {
            -1i32
        } else {
            nargs as i32
        }
    } else {
        vm.throw_str("`loadprimitive` expects number of arguments to be passed as integer")
    };

    let prim = if let Some(str) = prim.downcast_ref::<Str>() {
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
    let libs = o.field(vm, id);
    let mut libs = match libs.downcast_ref::<LibList>() {
        Some(liblist) => liblist,
        _ => vm.throw_str("library list malformed"),
    };

    let spath = vm.id(Id::Path);
    let mut path = vec![];
    let arr = o.field(vm, spath);
    array_iter(vm, arr, |_, _, val| {
        path.push(val.to_string());
        true
    });

    gc_frame!(vm.gc().roots() =>libs: Gc<LibList>);

    let ptr = load_primitive(vm, &mut libs, &path, prim, nargs);
    let f = nargs.int();
    let nargs = if f < 0 { 0 } else { f as u32 };
    let varsize = f < 0;
    let f = vm.gc().fixed(Function {
        nargs,
        varsize,
        env: Nullable::NULL,
        module: Nullable::NULL,
        addr: ptr,
        prim: true,
        construct_struct: Nullable::NULL,
    });
    Value::Primitive(f)
}

pub const MODULE_PATH: &'static str = "~/.waffle";

fn get_loader_path(vm: &mut VM) -> Value {
    let path = match std::env::var("WAFFLEPATH") {
        Ok(p) => p,
        Err(_) => format!(
            "{}:/usr/local/lib/waffle:/usr/lib/waffle:/usr/local/bin:/usr/bin:",
            MODULE_PATH
        ),
    };

    let paths = std::env::split_paths(&path)
        .map(|x| x.display().to_string())
        .collect::<Vec<String>>();

    let mut buf = new_array(vm, paths.len());
    gc_frame!(vm.gc().roots() => buf: Gc<Array<Value>>);
    for (pos, path) in paths.iter().enumerate() {
        let str = Value::Str(vm.gc().str(path));
        buf.put(vm, Symbol::Index(pos as _), str, false);
    }

    Value::new(*buf)
}

pub fn waffle_default_loader(vm: &mut VM) -> Value {
    let mut loader = Value::Null;
    let mut args = new_array(vm, std::env::args().len());
    gc_frame!(vm.gc().roots() => args: Gc<Object>,loader: Value);
    for (pos, arg) in std::env::args().enumerate() {
        let arg = vm.gc().str(arg);
        args.put(vm, Symbol::Index(pos as _), Value::Str(arg), false);
    }
    loader.set(Value::new(Object::new_empty(vm)));

    let path = get_loader_path(vm);
    let pathk = vm.id(Id::Path);
    loader.set_field(vm, pathk, path);
    let obj = Object::new_empty(vm);
    let cache = vm.id(Id::Cache);
    loader.set_field(vm, cache, Value::new(obj));
    let kargs = "args".intern();
    loader.set_field(vm, kargs, Value::new(args.get_copy()));
    let mut f = make_prim(vm, load_module as _, 2, false);
    let s = "loadmodule".intern();
    gc_frame!(vm.gc().roots() => f: Value);
    loader.set_field(vm, s, f.get_copy());

    let mut f = make_prim(vm, loader_loadprim as _, 2, false);
    let s = "loadprim".intern();
    gc_frame!(vm.gc().roots() => f: Value);
    loader.set_field(vm, s, f.get_copy());

    let mut liblist = Value::Abstract(vm.gc().fixed(LibList { v: vec![] }).as_dyn());
    let id = vm.id(Id::Libs);

    gc_frame!(vm.gc().roots() => liblist: Value);
    loader.set_field(vm, id, liblist.get_copy());
    loader.get_copy()
}
