use crate::{
    builtin::make_prim,
    gc_frame,
    memory::gcwrapper::*,
    object::{Object, OBJECT_CLASS},
    structure::Structure,
    value::Value,
    vm::{Internable, VM},
};

pub extern "C" fn object_constructor(vm: &mut VM) -> Value {
    Value::encode_object_value(Object::new_empty(vm))
}

pub extern "C" fn object_to_string(vm: &mut VM) -> Value {
    let this = *vm.this();
    Value::encode_object_value(if this.is_undefined() {
        vm.gc().str("[object Undefined]").as_dyn()
    } else if this.is_null() {
        vm.gc().str("[object Null]").as_dyn()
    } else {
        todo!()
    })
}

pub extern "C" fn object_set_prototype(vm: &mut VM, object: &Value, proto: &Value) -> Value {
    let this = *object;

    if let (Some(object), Some(new_prototype)) =
        (this.downcast_ref::<Object>(), proto.downcast_ref())
    {
        gc_frame!(vm.gc().roots() => object = object, structure = *object.structure());

        let transition = structure.change_prototype_transition(vm, Some(new_prototype));
        *object.structure_mut() = transition;
        if structure.prototype().is_null() {
            Value::Object(structure.prototype().as_gc())
        } else {
            Value::Null
        }
    } else {
        if this.is_obj() {
            vm.throw_str("new object prototype is not an object");
        } else {
            vm.throw_str("'this' is not an object");
        }
    }
}

pub extern "C" fn object_prototype(vm: &mut VM, arg: &Value) -> Value {
    let obj = arg.to_object(vm);
    let proto = obj.prototype();
    if proto.is_null() {
        Value::Null
    } else {
        Value::Object(proto.as_gc())
    }
}

pub fn init(vm: &mut VM) {
    {
        gc_frame!(vm.gc().roots() => structure = Structure::new_unique_indexed(vm, None, false));
        gc_frame!(vm.gc().roots() => prototype = Object::new(vm, &structure, OBJECT_CLASS, Value::Null));

        let f = make_prim(vm, object_constructor as _, 0, false);
        prototype.put(vm, "constructor".intern(), f, false);

        let f = make_prim(vm, object_to_string as _, 0, false);
        prototype.put(vm, "to_string".intern(), f, false);

        let f = make_prim(vm, object_set_prototype as _, 2, false);
        prototype.put(vm, "set_prototype".intern(), f, false);
        let f = make_prim(vm, object_prototype as _, 1, false);
        prototype.put(vm, "prototype".intern(), f, false);

        vm.global.object_prototype = prototype.nullable();
    }

    {
        vm.global.empty_object_struct = Structure::new_indexed(vm, None, false).nullable();
        vm.global
            .object_prototype
            .structure_mut()
            .instance_structure = Some(vm.global.empty_object_struct.as_gc());
        vm.global
            .empty_object_struct
            .as_gc()
            .change_prototype_with_no_transition(vm.global.object_prototype.as_gc());
    }
}
