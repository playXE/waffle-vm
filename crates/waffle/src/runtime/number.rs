use crate::{
    builtin::make_prim,
    class::Class,
    class::*,
    gc_frame, js_method_table,
    object::{Object, OBJECT_CLASS},
    structure::Structure,
    value::Value,
    vm::{Internable, VM},
};

/// $Number int | float -> $Int | $Float
pub extern "C" fn constructor(vm: &mut VM, num: &Value) -> Value {
    if num.is_int32() {
        super::int::constructor(vm, num)
    } else if num.is_double() {
        super::float::constructor(vm, num)
    } else {
        vm.throw_str("number expected")
    }
}

pub extern "C" fn value_of(vm: &mut VM) -> Value {
    if let Some(object) = vm.this().downcast_ref::<Object>() {
        if Object::is_instance_of_class(&object, &NUMBER_CLASS) {
            return object.data();
        }
    }

    vm.throw_str("incorrect invocation of Int::valueOf")
}

pub extern "C" fn to_string(vm: &mut VM) -> Value {
    if let Some(object) = vm.this().downcast_ref::<Object>() {
        if Object::is_instance_of_class(&object, &NUMBER_CLASS) {
            return Value::encode_object_value(vm.gc().str(object.data().get_double().to_string()));
        }
    }

    vm.throw_str("incorrect invocation of Int::toString")
}

pub static NUMBER_CLASS: &'static Class = &Class {
    name: "Number",
    method_table: js_method_table!(Object),
    previous: Some(OBJECT_CLASS),
};

pub fn init(vm: &mut VM) {
    {
        gc_frame!(vm.gc().roots() => structure = Structure::new_unique_with_proto(vm, None, false));
        structure.change_prototype_with_no_transition(vm.global.object_prototype.as_gc());

        gc_frame!(vm.gc().roots() => object = Object::new(vm, &structure, &NUMBER_CLASS, Value::Int(0)));

        let f = make_prim(vm, constructor as _, 1, false);
        object.put(vm, "constructor", f, false);

        let f = make_prim(vm, value_of as _, 0, false);
        object.put(vm, "value_of", f, false);

        let f = make_prim(vm, to_string as _, 0, false);
        object.put(vm, "to_string", f, false);
        vm.global.number_prototype = object.nullable();
    }
    {
        gc_frame!(vm.gc().roots() => builtins = vm.builtins);
        let p = vm.global.number_prototype;
        builtins.set_field(vm, "number".intern(), Value::Object(p.as_gc()));
    }
}
