use crate::{
    class::{Class, MethodTable},
    memory::gcwrapper::{Gc, Str},
    object::{Hint, Object, Slot, OBJECT_CLASS},
    value::*,
    vm::{self, Id, Symbol, VM},
};

pub extern "C" fn constructor(vm: &mut VM, value: &Value) -> Value {
    let str = value.format(vm);
    todo!()
}

pub struct StringObject;

#[allow(non_snake_case)]
impl StringObject {
    fn GetOwnNonIndexedPropertySlotMethod(
        obj: &mut Gc<Object>,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> bool {
        if name == ctx.id(Id::Length) {
            if let Some(str) = obj.data().downcast_ref::<Str>() {
                slot.set_value(Value::new(str.len() as i32));
                slot.make_uncacheable();
                return true;
            }
        }

        (obj.class()
            .previous
            .map(|x| x.method_table.GetOwnNonIndexedPropertySlot)
            .unwrap_or_else(|| Object::GetOwnNonIndexedPropertySlotMethod))(
            obj, ctx, name, slot
        )
    }

    fn DefaultValueMethod(obj: &mut Gc<Object>, ctx: &mut VM, hint: Hint) -> Value {
        if hint == Hint::String {
            let data = obj
                .data()
                .downcast_ref::<Str>()
                .unwrap_or_else(|| ctx.throw_str("string data violated"));
            return Value::new(data);
        }

        let data = obj
            .data()
            .downcast_ref::<Str>()
            .unwrap_or_else(|| ctx.throw_str("string data violated"));

        if let Ok(num) = data.parse::<f64>() {
            if num as i32 as f64 == num {
                Value::new(num as i32)
            } else {
                Value::new(num)
            }
        } else {
            Value::new(data)
        }
    }
}

pub static STRING_CLASS: &'static Class = &Class {
    name: "string",
    previous: Some(&OBJECT_CLASS),
    method_table: MethodTable {
        GetOwnNonIndexedPropertySlot: StringObject::GetOwnNonIndexedPropertySlotMethod,
        GetIndexedPropertySlot: Object::GetIndexedPropertySlotMethod,
        GetIndexedSlot: Object::GetIndexedSlotMethod,
        GetNonIndexedPropertySlot: Object::GetNonIndexedPropertySlotMethod,
        GetNonIndexedSlot: Object::GetNonIndexedSlotMethod,
        GetOwnIndexedPropertySlot: Object::GetOwnIndexedPropertySlotMethod,
        GetOwnPropertyNames: Object::GetOwnPropertyNamesMethod,
        GetPropertyNames: Object::GetPropertyNamesMethod,
        PutIndexedSlot: Object::PutIndexedSlotMethod,
        PutNonIndexedSlot: Object::PutNonIndexedSlotMethod,
        DefaultValue: StringObject::DefaultValueMethod,
        DefineOwnIndexedPropertySlot: Object::DefineOwnIndexedPropertySlotMethod,
        DefineOwnNonIndexedPropertySlot: Object::DefineOwnNonIndexedPropertySlotMethod,
        DeleteIndexed: Object::DeleteIndexedMethod,
        DeleteNonIndexed: Object::DeleteNonIndexedMethod,
    },
};
