use crate::{
    gc_frame,
    memory::gcwrapper::{Array, Gc, Nullable, Str},
    value::{ByteBuffer, Cell, Function, Obj, Sym, Value},
    vm::VM,
};

pub static BUILTINS: [usize; 0] = [];

pub fn make_prim(vm: &mut VM, f: usize, nargs: usize, var: bool) -> Value {
    let prim = Function {
        addr: f,
        nargs: nargs as _,
        varsize: var,
        env: Nullable::NULL,
        module: Nullable::NULL,
        prim: true,
    };

    Value::Primitive(vm.gc().fixed(prim))
}

pub fn builtin_array(vm: &mut VM, args: &[Value]) -> Value {
    let mut a = vm.gc().array(args.len(), Value::Null);

    for i in 0..args.len() {
        a[i] = args[i];
    }

    Value::Array(a)
}

pub extern "C" fn builtin_amake(vm: &mut VM, size: &Value) -> Value {
    let s = size.int();
    Value::Array(vm.gc().array(s as _, Value::Null))
}

pub extern "C" fn builtin_asize(vm: &mut VM, a: &Value) -> Value {
    if !a.is_array() {
        vm.throw_str("array expected");
    }

    let arr = a.array();
    Value::Int(arr.len() as _)
}

pub extern "C" fn builtin_acopy(vm: &mut VM, a: &Value) -> Value {
    if !a.is_array() {
        vm.throw_str("array expected");
    }

    let arr = a.array();
    let mut cpy = vm.gc().array(arr.len(), Value::Null);
    for i in 0..arr.len() {
        cpy[i] = arr[i];
    }

    Value::Array(cpy)
}

pub extern "C" fn builtin_full_gc(vm: &mut VM) -> Value {
    vm.gc().collect(2, &mut []);
    Value::Null
}

pub extern "C" fn builtin_minor_gc(vm: &mut VM) -> Value {
    vm.gc().collect(0, &mut []);
    Value::Null
}

/// $throw: any -> !
///
/// Throws exception
pub extern "C" fn builtin_throw(vm: &mut VM, val: &Value) -> Value {
    vm.throw(*val);
}

/// $new: object | null -> object
///
/// Creates new object with specified prototype or no prototype if null
pub extern "C" fn builtin_new(vm: &mut VM, val: &Value) -> Value {
    if !val.is_null() && !val.is_obj() {
        vm.throw_str(&format!(
            "`new` expects object or null value as argument but `{}` was found",
            val
        ));
    }

    match val {
        proto if proto.is_obj() => {
            Value::Object(Obj::with_proto(vm, proto.downcast_ref().unwrap()))
        }
        _ => Value::Object(Obj::new(vm)),
    }
}

/// $symbol: str | sym -> sym
///
/// Interns new symbol
pub extern "C" fn builtin_symbol(vm: &mut VM, val: &Value) -> Value {
    /*match val {
        Value::Symbol(_) => *val,
        Value::Str(x) => Value::Symbol(vm.intern(&***x)),
        _ => vm.throw_str("`symbol` expects string as argument"),
    }*/
    if let Some(sym) = val.downcast_ref::<Sym>() {
        Value::Symbol(sym)
    } else if let Some(mut str) = val.downcast_ref::<Str>() {
        gc_frame!(vm.gc().roots() => str: Gc<Str>);
        Value::Symbol(vm.intern(&***str))
    } else {
        vm.throw_str("`symbol` expects string as argument")
    }
}

pub extern "C" fn builtin_ofields(vm: &mut VM, obj: &Value) -> Value {
    /*match obj {
        Value::Object(ref object) => {
            let mut arr = vm.gc().array(object.table.count, Value::Null);

            gc_frame!(vm.gc().roots() => arr: Gc<Array<Value>>);
            let mut c = 0;
            for i in 0..object.table.cells.len() {
                let mut cell = object.table.cells[i];
                gc_frame!(vm.gc().roots() => cell: Nullable<Cell>);
                while cell.is_not_null() {
                    let mut farr = vm.gc().array(2, Value::Null);
                    farr[0] = cell.key;
                    farr[1] = (**cell).value;
                    arr[c] = Value::Array(farr);
                    c += 1;
                    vm.gc().write_barrier(*arr);
                    cell.set(cell.next);
                }
            }

            Value::Array(*arr)
        }
        _ => vm.throw_str("ofields expects object"),
    }*/
    if let Some(mut object) = obj.downcast_ref::<Obj>() {
        gc_frame!(vm.gc().roots() => object: Gc<Obj>);
        let mut arr = vm.gc().array(object.table.count, Value::Null);

        gc_frame!(vm.gc().roots() => arr: Gc<Array<Value>>);
        let mut c = 0;
        for i in 0..object.table.cells.len() {
            let mut cell = object.table.cells[i];
            gc_frame!(vm.gc().roots() => cell: Nullable<Cell>);
            while cell.is_not_null() {
                let mut farr = vm.gc().array(2, Value::Null);
                farr[0] = cell.key;
                farr[1] = (**cell).value;
                arr[c] = Value::Array(farr);
                c += 1;
                vm.gc().write_barrier(*arr);
                cell.set(cell.next);
            }
        }

        Value::Array(*arr)
    } else {
        vm.throw_str("ofields expects objects")
    }
}

pub extern "C" fn builtin_string(vm: &mut VM, x: &Value) -> Value {
    let x = x.to_string();
    Value::Str(vm.gc().str(x))
}

pub extern "C" fn builtin_string_concat(vm: &mut VM, x: &Value, y: &Value) -> Value {
    let x = x.to_string();
    let y = y.to_string();
    Value::Str(vm.gc().str(format!("{}{}", x, y)))
}

pub extern "C" fn builtin_print(_: &mut VM, x: &Value) -> Value {
    println!("{}", x);
    Value::Null
}

pub fn alloc_buffer(vm: &mut VM, size: usize) -> Gc<ByteBuffer> {
    unsafe {
        let p = vm.gc().malloc_varsize::<ByteBuffer>(size, &mut []);
        let ptr = p.as_mut_ptr();
        core::ptr::write_bytes((*ptr).data.as_mut_ptr(), 0, size);
        p.assume_init()
    }
}

/// bmake: string | array<int> | int -> bytebuffer
pub extern "C" fn builtin_bmake(vm: &mut VM, x: &Value) -> Value {
    if x.is_array() {
        let mut arr = x.array();
        let mut buf = Nullable::<ByteBuffer>::NULL;
        gc_frame!(vm.gc().roots() => arr: Gc<Array<Value>>,buf: Nullable<ByteBuffer>);
        let mut tmp = vec![];
        for i in 0..arr.len() {
            if !arr[i].is_int32() {
                vm.throw_str("bmake: array of integers or string expected");
            }
            tmp.push(arr[i].get_int32() as u8);
        }

        buf.set(alloc_buffer(vm, arr.len()).nullable());

        unsafe {
            core::ptr::copy_nonoverlapping(tmp.as_ptr(), buf.data.as_mut_ptr(), arr.len());
        }
        return Value::encode_object_value(buf.as_gc());
    } else if x.is_str() {
        let len = x.str().len();
        let mut buf = alloc_buffer(vm, len);
        let s = x.str();
        let str = s.as_bytes();
        unsafe {
            core::ptr::copy_nonoverlapping(str.as_ptr(), buf.as_mut_ptr(), len);
        }

        return Value::encode_object_value(buf);
    } else if x.is_int32() {
        return Value::encode_object_value(alloc_buffer(vm, x.get_int32() as _));
    }
    vm.throw_str("bmake: string or array of integers expected")
}

/// bcopy: bytebuffer -> bytebuffer
pub extern "C" fn builtin_bcopy(vm: &mut VM, x: &Value) -> Value {
    if let Some(mut buf) = x.downcast_ref::<ByteBuffer>() {
        gc_frame!(vm.gc().roots() => buf: Gc<ByteBuffer>);
        let mut copy = alloc_buffer(vm, buf.len());
        unsafe {
            core::ptr::copy_nonoverlapping(buf.as_ptr(), copy.as_mut_ptr(), buf.len());
        }
        Value::encode_object_value(copy)
    } else {
        vm.throw_str("bcopy: bytebuffer expected")
    }
}

pub extern "C" fn builtin_bget(vm: &mut VM, x: &Value, at: &Value) -> Value {
    if let Some(buf) = x.downcast_ref::<ByteBuffer>() {
        if !at.is_int32() {
            vm.throw_str("bget: int32 is expected as second argument");
        }
        let ix = at.get_int32() as u32 as usize;
        Value::Int(buf[ix] as i32)
    } else {
        vm.throw_str("bget: bytebuffer expected as first argument");
    }
}

pub extern "C" fn builtin_bset(vm: &mut VM, x: &Value, at: &Value, val: &Value) -> Value {
    if let Some(mut buf) = x.downcast_ref::<ByteBuffer>() {
        if !at.is_int32() {
            vm.throw_str("bset: int32 is expected as second argument");
        }
        if !val.is_int32() {
            vm.throw_str("bset: int32 is expected as third argument");
        }
        let ix = at.get_int32() as u32 as usize;
        buf[ix] = val.get_int32() as u8;
        Value::Null
    } else {
        vm.throw_str("bset: bytebuffer expected as first argument");
    }
}

pub extern "C" fn builtin_bsize(vm: &mut VM, x: &Value) -> Value {
    if let Some(buf) = x.downcast_ref::<ByteBuffer>() {
        Value::Int(buf.len() as _)
    } else {
        vm.throw_str("bsize: bytebuffer expected")
    }
}

pub extern "C" fn builtin_bstr(vm: &mut VM, x: &Value) -> Value {
    if let Some(buf) = x.downcast_ref::<ByteBuffer>() {
        let str = String::from_utf8(buf.to_vec()).unwrap().to_string();
        Value::Str(vm.gc().str(str))
    } else {
        vm.throw_str("bstr: bytebuffer expected")
    }
}

pub extern "C" fn builtin_typename(vm: &mut VM, x: &Value) -> Value {
    Value::Symbol(if x.is_int32() {
        vm.intern("Int")
    } else if x.is_double() {
        vm.intern("Float")
    } else if x.is_bool() {
        vm.intern("Bool")
    } else if x.is_null() {
        vm.intern("Null")
    } else {
        unsafe {
            let obj = x.get_object().header.as_ref().tid().as_vtable.type_name;
            vm.intern(obj)
        }
    })
}

use crate::ffi;
pub extern "C" fn ffi_open(vm: &mut VM, args: &Gc<Array<Value>>) -> Value {
    let mut paths = vec![];
    for arg in args.iter() {
        if let Some(str) = arg.downcast_ref::<Str>() {
            paths.push(str.to_string());
        } else {
            vm.throw_str("ffi_open: string array expected");
        }
    }
    let lib = ffi::Library::open(&paths).unwrap_or_else(|| vm.throw_str("failed to open library"));

    Value::encode_object_value(vm.gc().fixed(lib))
}

pub extern "C" fn ffi_attach(
    vm: &mut VM,
    lib: &Value,
    name: &Value,
    args: &Value,
    rtype: &Value,
) -> Value {
    let lib = lib
        .downcast_ref::<ffi::Library>()
        .unwrap_or_else(|| vm.throw_str("ffi_attach: library expected"));
    let name = name
        .downcast_ref::<Str>()
        .unwrap_or_else(|| vm.throw_str("ffi_attach: name expected"));
    let args = args
        .downcast_ref::<Array<Value>>()
        .unwrap_or_else(|| vm.throw_str("array of arguments expected"));

    for arg in args.iter() {
        if !arg.is_int32() {
            vm.throw_str("argument must be an int32");
        }
    }
    let rtype = if rtype.is_int32() {
        *rtype
    } else {
        vm.throw_str("rtype int32")
    };
    unsafe {
        let func = ffi::Function::attach(&lib, &name, &args, rtype).unwrap_or_else(|err| {
            vm.throw_str(format!("failed to attach function '{}': {}", &**name, err))
        });
        match func {
            Some(f) => Value::encode_object_value(vm.gc().fixed(f)),
            None => Value::Null,
        }
    }
}

pub extern "C" fn ffi_call(vm: &mut VM, func: &Value, args: &Value) -> Value {
    if let (Some(mut func), Some(mut args)) = (
        func.downcast_ref::<ffi::Function>(),
        args.downcast_ref::<Array<Value>>(),
    ) {
        gc_frame!(vm.gc().roots() => func: Gc<Function>,args: Gc<Array<Value>>);
        unsafe {
            func.call(vm, &*args)
                .unwrap_or_else(|err| vm.throw_str(err))
        }
    } else {
        vm.throw_str("ffi_call: invalid arguments")
    }
}

pub extern "C" fn ffi_attach_pointer(vm: &mut VM, lib: &Value, name: &Value) -> Value {
    let lib = lib
        .downcast_ref::<ffi::Library>()
        .unwrap_or_else(|| vm.throw_str("ffi_attach_pointer: library expected"));
    let name = name
        .downcast_ref::<Str>()
        .unwrap_or_else(|| vm.throw_str("ffi_attach_pointer: name expected"));

    let raw_ptr = unsafe {
        (*lib)
            .get(&**name)
            .map(|ptr| {
                Value::encode_object_value(vm.gc().fixed(ffi::Pointer::new(ptr.as_ptr() as _)))
            })
            .unwrap_or_else(|| Value::Null)
    };
    raw_ptr
}

use std::fs::{self, File, OpenOptions};
use std::io::{Read, Seek, SeekFrom, Write};

/// Read only
pub const MODE_RO: i32 = 0;
/// Write only
pub const MODE_WO: i32 = 1;
/// Append only
pub const MODE_AO: i32 = 2;
/// Read write
pub const MODE_RW: i32 = 3;
/// Read append
pub const MODE_RA: i32 = 4;

pub extern "C" fn file_open(vm: &mut VM, path: &Value, mode: &Value) -> Value {
    let path = vm.expect_str(path);
    let mode = vm.expect_int(mode);
    let mut options = OpenOptions::new();
    match mode {
        MODE_RO => options.read(true),
        MODE_WO => options.write(true).truncate(true).create(true),
        MODE_AO => options.append(true).create(true),
        MODE_RW => options.read(true).write(true).create(true),
        MODE_RA => options.read(true).append(true).create(true),
        _ => vm.throw_str(format!("unknown read mode:  '{}'", mode)),
    };
    Value::encode_object_value(
        options
            .open(&**path)
            .map(|file| vm.gc().fixed(file))
            .unwrap_or_else(|err| vm.throw_str(format!("failed to open '{}': {}", &**path, err))),
    )
}

pub extern "C" fn file_read(vm: &mut VM, file: &Value, buff: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    let mut buff = vm.expect_bytebuffer(buff);

    match file.read(&mut **buff) {
        Err(e) => vm.throw_str(format!("failed to read from file: {}", e)),
        Ok(nread) => Value::Int(nread as _),
    }
}

pub extern "C" fn file_read_to_end(vm: &mut VM, file: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    let mut buf = vec![];
    file.read_to_end(&mut buf)
        .unwrap_or_else(|err| vm.throw_str(format!("failed to read file: {}", err)));
    let mut bbuf = alloc_buffer(vm, buf.len());
    unsafe {
        std::ptr::copy_nonoverlapping(buf.as_ptr(), bbuf.as_mut_ptr(), buf.len());
    }
    Value::encode_object_value(bbuf)
}

pub extern "C" fn file_write(vm: &mut VM, file: &Value, buf: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    if let Some(buf) = buf.downcast_ref::<ByteBuffer>() {
        file.write(&*buf)
            .map(|c| Value::Int(c as _))
            .unwrap_or_else(|err| vm.throw_str(format!("failed to write bytes to file: {}", err)))
    } else if let Some(buf) = buf.downcast_ref::<Str>() {
        file.write(buf.as_bytes())
            .map(|c| Value::Int(c as _))
            .unwrap_or_else(|err| vm.throw_str(format!("failed to write bytes to file: {}", err)))
    } else {
        vm.throw_str("string or bytebuffer expected")
    }
}

pub extern "C" fn file_write_all(vm: &mut VM, file: &Value, buf: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    if let Some(buf) = buf.downcast_ref::<ByteBuffer>() {
        file.write_all(&*buf)
            .map(|_| Value::Null)
            .unwrap_or_else(|err| vm.throw_str(format!("failed to write bytes to file: {}", err)))
    } else if let Some(buf) = buf.downcast_ref::<Str>() {
        file.write_all(buf.as_bytes())
            .map(|_| Value::Null)
            .unwrap_or_else(|err| vm.throw_str(format!("failed to write bytes to file: {}", err)))
    } else {
        vm.throw_str("string or bytebuffer expected")
    }
}

pub extern "C" fn file_seek(vm: &mut VM, file: &Value, offset: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    let offset = vm.expect_int(offset);
    let seek = if offset < 0 {
        SeekFrom::End(offset as _)
    } else {
        SeekFrom::Start(offset as _)
    };

    let result = file
        .seek(seek)
        .map(|x| Value::Int(x as _))
        .unwrap_or_else(|err| vm.throw_str(format!("seek failed: {}", err)));
    result
}

pub extern "C" fn file_flush(vm: &mut VM, file: &Value) -> Value {
    let mut file = vm.expect::<File, _>(file, "file expected");
    file.flush()
        .unwrap_or_else(|err| vm.throw_str(format!("flush failed: {}", err)));
    Value::Null
}

pub(crate) fn init_builtin(vm: &mut VM) {
    let mut obj = Obj::with_capacity(vm, 128); // we do not want to relocate this table, less garbage to cleanup

    gc_frame!(vm.gc().roots() => obj: Gc<Obj>);

    let mut f = make_prim(vm, builtin_acopy as _, 1, false);
    let mut s = Value::Symbol(vm.intern("acopy"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_amake as _, 1, false);
    let mut s = Value::Symbol(vm.intern("amake"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_array as _, 0, true);
    let mut s = Value::Symbol(vm.intern("array"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_asize as _, 1, false);
    let mut s = Value::Symbol(vm.intern("asize"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_full_gc as _, 0, false);
    let mut s = Value::Symbol(vm.intern("fullGC"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_minor_gc as _, 0, false);
    let mut s = Value::Symbol(vm.intern("minorGC"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);
    let mut f = make_prim(vm, builtin_throw as _, 1, false);
    let mut s = Value::Symbol(vm.intern("throw"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_new as _, 1, false);
    let mut s = Value::Symbol(vm.intern("new"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_symbol as _, 1, false);
    let mut s = Value::Symbol(vm.intern("symbol"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_ofields as _, 1, false);
    let mut s = Value::Symbol(vm.intern("ofields"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_string as _, 1, false);
    let mut s = Value::Symbol(vm.intern("string"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);
    let mut f = make_prim(vm, builtin_string_concat as _, 2, false);
    let mut s = Value::Symbol(vm.intern("string_concat"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_print as _, 1, false);
    let mut s = Value::Symbol(vm.intern("print"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bmake as _, 1, false);
    let mut s = Value::Symbol(vm.intern("bmake"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bcopy as _, 1, false);
    let mut s = Value::Symbol(vm.intern("bcopy"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bget as _, 2, false);
    let mut s = Value::Symbol(vm.intern("bget"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bset as _, 3, false);
    let mut s = Value::Symbol(vm.intern("bset"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bsize as _, 1, false);
    let mut s = Value::Symbol(vm.intern("bsize"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_bstr as _, 1, false);
    let mut s = Value::Symbol(vm.intern("bstr"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, builtin_typename as _, 1, false);
    let mut s = Value::Symbol(vm.intern("typename"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, ffi_open as _, 0, true);
    let mut s = Value::Symbol(vm.intern("ffi_open"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, ffi_attach as _, 4, false);
    let mut s = Value::Symbol(vm.intern("ffi_attach"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, ffi_call as _, 2, false);
    let mut s = Value::Symbol(vm.intern("ffi_call"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, ffi_attach_pointer as _, 2, false);
    let mut s = Value::Symbol(vm.intern("ffi_attach_pointer"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_open as _, 2, false);
    let mut s = Value::Symbol(vm.intern("file_open"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_read as _, 2, false);
    let mut s = Value::Symbol(vm.intern("file_read"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_read_to_end as _, 1, false);
    let mut s = Value::Symbol(vm.intern("file_read_to_end"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_write as _, 2, false);
    let mut s = Value::Symbol(vm.intern("file_write"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_write_all as _, 2, false);
    let mut s = Value::Symbol(vm.intern("file_write_all"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    let mut f = make_prim(vm, file_flush as _, 1, false);
    let mut s = Value::Symbol(vm.intern("file_flush"));
    gc_frame!(vm.gc().roots() => f: Value,s: Value);
    obj.insert(vm, &s, &f);

    vm.builtins = Value::Object(obj.get());
}
