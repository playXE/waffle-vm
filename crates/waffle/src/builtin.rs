use crate::{
    gc_frame,
    memory::gcwrapper::{Gc, Nullable},
    value::{Function, Obj, Value},
    vm::VM,
};

pub static BUILTINS: [usize; 0] = [];

pub fn make_prim(vm: &mut VM, f: usize, nargs: usize, var: bool) -> Value {
    let prim = Function {
        addr: f,
        nargs: nargs as _,
        varsize: var,
        env: Value::Null,
        module: Nullable::NULL,
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
    if !matches!(a, Value::Array(_)) {
        vm.throw_str("array expected");
    }

    let arr = a.array();
    Value::Int(arr.len() as _)
}

pub extern "C" fn builtin_acopy(vm: &mut VM, a: &Value) -> Value {
    if !matches!(a, Value::Array(_)) {
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
    if !matches!(val, Value::Null | Value::Object(_)) {
        vm.throw_str(&format!(
            "`new` expects object or null value as argument but `{}` was found",
            val
        ));
    }

    match val {
        Value::Object(proto) => Value::Object(Obj::with_proto(vm, *proto)),
        _ => Value::Object(Obj::new(vm)),
    }
}

/// $symbol: str | sym -> sym
///
/// Interns new symbol
pub extern "C" fn builtin_symbol(vm: &mut VM, val: &Value) -> Value {
    match val {
        Value::Symbol(_) => *val,
        Value::Str(x) => Value::Symbol(vm.intern(&***x)),
        _ => vm.throw_str("`symbol` expects string as argument"),
    }
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

    vm.builtins = Value::Object(obj.get());
}
