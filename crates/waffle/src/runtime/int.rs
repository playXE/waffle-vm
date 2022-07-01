use crate::{
    builtin::make_prim, class::*, gc_frame, js_method_table, object::Object, structure::Structure,
    value::Value, vm::VM,
};

use super::number::NUMBER_CLASS;

pub extern "C" fn constructor(vm: &mut VM, val: &Value) -> Value {
    if !val.is_int32() {
        vm.throw_str("Int expects an integer value");
    }
    gc_frame!(vm.gc().roots() => structure = vm.global.int_structure.as_gc());
    let object = Object::new(vm, &structure, &INT_CLASS, *val);

    Value::encode_object_value(object)
}

pub extern "C" fn value_of(vm: &mut VM) -> Value {
    if let Some(object) = vm.this().downcast_ref::<Object>() {
        if Object::is_instance_of_class(&object, &INT_CLASS) {
            return object.data();
        }
    }

    vm.throw_str("incorrect invocation of Int::valueOf")
}

pub extern "C" fn to_string(vm: &mut VM) -> Value {
    if let Some(object) = vm.this().downcast_ref::<Object>() {
        if Object::is_instance_of_class(&object, &INT_CLASS) {
            return Value::encode_object_value(vm.gc().str(object.data().int().to_string()));
        }
    }

    vm.throw_str("incorrect invocation of Int::toString")
}

pub static INT_CLASS: &'static Class = &Class {
    name: "Int",
    method_table: js_method_table!(Object),
    previous: Some(NUMBER_CLASS),
};

pub fn init(vm: &mut VM) {
    let structure = Structure::new_unique_with_proto(vm, vm.global.number_prototype.into(), false);
    vm.global.int_structure = structure.nullable();
    gc_frame!(vm.gc().roots() => object = Object::new(vm, &structure, &INT_CLASS, Value::new(0)));
    let f = make_prim(vm, constructor as _, 1, false);
    object.put(vm, "constructor", f, false);

    let f = make_prim(vm, value_of as _, 0, false);
    object.put(vm, "value_of", f, false);

    let f = make_prim(vm, to_string as _, 0, false);
    object.put(vm, "to_string", f, false);

    vm.global
        .int_structure
        .as_gc()
        .change_prototype_with_no_transition(*object);
}
