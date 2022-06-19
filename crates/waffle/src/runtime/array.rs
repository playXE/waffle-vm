use crate::{
    builtin::make_prim,
    class::*,
    gc_frame, js_method_table,
    memory::gcwrapper::{Array, Gc},
    object::*,
    structure::Structure,
    value::*,
    vm::{Internable, Symbol, VM},
};

pub static ARRAY_CLASS: &'static Class = &Class {
    name: "Array",
    method_table: js_method_table!(Object),
    previous: Some(&OBJECT_CLASS),
};

pub fn new_array(vm: &mut VM, n: usize) -> Gc<Object> {
    gc_frame!(vm.gc().roots() => structure = vm.global.array_structure.as_gc());
    let mut arr = Object::new(vm, &structure, &ARRAY_CLASS, Value::Null);
    arr.indexed.set_length(n as _);
    arr
}

pub fn constructor(vm: &mut VM, args: &Gc<Array<Value>>) -> Value {
    if args.len() == 0 {
        Value::Object(new_array(vm, 0))
    } else if args.len() == 1 {
        if args[0].is_int32() {
            return Value::Object(new_array(vm, args[0].get_int32() as _));
        } else {
            gc_frame!(vm.gc().roots() => ary = new_array(vm, 1));
            ary.put(vm, Symbol::Index(0), args[0], false);
            return Value::Object(*ary);
        }
    } else {
        gc_frame!(vm.gc().roots() => ary = new_array(vm, args.len()));
        for i in 0..args.len() {
            ary.put(vm, Symbol::Index(i as _), args[i], false);
        }
        Value::Object(*ary)
    }
}

pub fn init(vm: &mut VM) {
    let structure = Structure::new_indexed(vm, None, false);
    vm.global.array_structure = structure.nullable();
    gc_frame!(vm.gc().roots() => obj_proto = vm.global.object_prototype.as_gc());
    gc_frame!(vm.gc().roots() => structure = Structure::new_unique_indexed(vm, Some(*obj_proto), false));
    gc_frame!(vm.gc().roots() => prototype = Object::new(vm, &structure, &ARRAY_CLASS, Value::Null));

    vm.global
        .array_structure
        .as_gc()
        .change_prototype_with_no_transition(*prototype);

    let f = make_prim(vm, constructor as _, 0, true);

    prototype.put(vm, "constructor".intern(), f, false);
    gc_frame!(vm.gc().roots() => builtins = vm.builtins.downcast_ref::<Object>().unwrap());

    builtins.put(vm, "array".intern(), Value::Object(*prototype), false);
}
