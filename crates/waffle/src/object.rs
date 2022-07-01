use super::class::MethodTable;
use super::value::*;
use crate::class::Class;
use crate::indexed_elements::{IndexedElements, MAX_VECTOR_SIZE};
use crate::memory::array::ArrayStorage;
use crate::memory::gcwrapper::Nullable;
use crate::runtime::array::ARRAY_CLASS;
use crate::structure::Structure;
use crate::vm::{symbol_table, Id, Internable, Symbol, VM};
use crate::{attributes::*, gc_frame, js_method_table};
use crate::{
    memory::{gcwrapper::Gc, Finalize, Managed, Trace, Visitor},
    value::Value,
};
use property_descriptor::*;
use std::ops::{Deref, DerefMut};

pub const OBJ_FLAG_TUPLE: u32 = 0x4;
pub const OBJ_FLAG_CALLABLE: u32 = 0x2;
pub const OBJ_FLAG_EXTENSIBLE: u32 = 0x1;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(u32)]
pub enum PutResultType {
    None = 0,
    Replace,
    New,
    IndexedOptimized,
}

impl Deref for Slot {
    type Target = StoredSlot;
    fn deref(&self) -> &Self::Target {
        &self.parent
    }
}

impl DerefMut for Slot {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parent
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum EnumerationMode {
    Default,
    IncludeNotEnumerable,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum Hint {
    String,
    Number,
    None,
}
pub struct Object {
    class: &'static Class,
    structure: Gc<Structure>,
    slots: Gc<ArrayStorage>,
    pub(crate) indexed: IndexedElements,
    flags: u32,
    /// external objects might store additional abstract data there
    data: Value,
}

fn is_absent_descriptor(desc: &PropertyDescriptor) -> bool {
    if !desc.is_enumerable() && !desc.is_enumerable_absent() {
        return false;
    }

    if !desc.is_configurable() && !desc.is_configurable_absent() {
        return false;
    }
    if desc.is_accessor() {
        return false;
    }
    if desc.is_data() {
        return DataDescriptor { parent: *desc }.is_writable()
            && DataDescriptor { parent: *desc }.is_writable_absent();
    }
    true
}

#[allow(non_snake_case)]
impl Object {
    pub fn structure(&self) -> &Gc<Structure> {
        &self.structure
    }

    pub fn structure_mut(&mut self) -> &mut Gc<Structure> {
        &mut self.structure
    }
    pub fn new_empty(vm: &mut VM) -> Gc<Self> {
        gc_frame!(vm.gc().roots() => structure = vm.global.empty_object_struct.as_gc());

        Self::new(
            vm,
            &structure,
            &OBJECT_CLASS,
            Value::encode_undefined_value(),
        )
    }
    pub(crate) fn indexed(&self) -> &IndexedElements {
        &self.indexed
    }
    pub fn new(
        vm: &mut VM,
        structure: &Gc<Structure>,
        class: &'static Class,
        data: Value,
    ) -> Gc<Self> {
        let indexed = IndexedElements::new(vm);
        gc_frame!(vm.gc().roots() => indexed = Some(indexed), storage = ArrayStorage::with_size(vm.gc(),structure.storage_capacity(), structure.storage_capacity()));

        let this = Self {
            structure: *structure,
            class,
            slots: *storage,
            indexed: indexed.take().unwrap(),
            flags: OBJ_FLAG_EXTENSIBLE,
            data,
        };

        let this = vm.gc().fixed(this);

        this
    }

    pub fn direct(&self, n: usize) -> &Value {
        self.slots.at(n as _)
    }

    pub fn direct_mut(&mut self, n: usize) -> &mut Value {
        self.slots.at_mut(n as _)
    }

    pub fn class(&self) -> &'static Class {
        self.class
    }

    pub fn is(obj: &Self, class: &'static Class) -> bool {
        std::ptr::eq(obj.class(), class)
    }

    pub fn is_instance_of_class(obj: &Self, class: &'static Class) -> bool {
        let mut cur = Some(obj.class());
        while let Some(cls) = cur {
            if std::ptr::eq(cls, class) {
                return true;
            }
            cur = cls.previous;
        }
        false
    }

    pub fn data(&self) -> Value {
        self.data
    }

    pub fn GetNonIndexedSlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> Value {
        if obj.get_non_indexed_property_slot(ctx, name, slot) {
            return slot.get(ctx, Value::encode_object_value(obj.as_dyn()));
        }
        Value::encode_undefined_value()
    }
    pub fn GetIndexedSlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        index: u32,
        slot: &mut Slot,
    ) -> Value {
        if obj.get_indexed_property_slot(ctx, index, slot) {
            return slot.get(ctx, Value::encode_object_value(obj.as_dyn()));
        }

        Value::encode_undefined_value()
    }

    pub fn DeleteNonIndexedMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        name: Symbol,
        throwable: bool,
    ) -> bool {
        let mut slot = Slot::new();
        if !obj.get_own_property_slot(ctx, name, &mut slot) {
            return true;
        }

        if !slot.attributes().is_configurable() {
            if throwable {
                ctx.throw_str("Can not delete uncofigurable property");
            }
            return false;
        }

        let offset = if slot.has_offset() {
            slot.offset()
        } else {
            let entry = obj.structure.get(ctx, name);
            if entry.is_not_found() {
                return true;
            }
            entry.offset
        };

        let s = obj.structure.delete_property_transition(ctx, name);
        obj.structure = s;
        *obj.direct_mut(offset as _) = Value::encode_empty_value();
        true
    }

    #[allow(clippy::unnecessary_unwrap)]
    pub fn delete_indexed_internal(&mut self, ctx: &mut VM, index: u32, throwable: bool) -> bool {
        if self.indexed.length() <= index {
            return true;
        }

        if self.indexed.dense() {
            if index < self.indexed.vector.size() as u32 {
                *self.indexed.vector.at_mut(index as _) = Value::encode_empty_value();
                return true;
            }

            if index < MAX_VECTOR_SIZE as u32 {
                return true;
            }
        }

        if self.indexed.map.is_none() {
            return true;
        }
        let map = self.indexed.map.as_mut().unwrap();

        /*match map.entry(index) {
            Entry::Vacant(_) => Ok(true),
            Entry::Occupied(x) => {
                if !x.get().attributes().is_configurable() {
                    if throwable {
                        let msg = JsString::new(_ctx, "trying to delete non-configurable property");
                        return Err(JsValue::encode_object_value(JsTypeError::new(
                            _ctx, msg, None,
                        )));
                    }
                    return Ok(false);
                }
                x.remove();
                if map.is_empty() {
                    self.indexed.make_dense();
                }
                Ok(true)
            }
        }*/
        match map.get(&index) {
            Some(x) => {
                let attr = x.attributes();
                if attr.is_configurable() {
                    if throwable {
                        ctx.throw_str("trying to delete non-configurable property")
                    }
                    return false;
                }
                map.remove(&index).unwrap();
                if map.len() == 0 {
                    self.indexed.make_dense();
                }
                true
            }
            None => true,
        }
    }
    pub fn DeleteIndexedMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        index: u32,
        throwable: bool,
    ) -> bool {
        if obj.class.method_table.GetOwnIndexedPropertySlot as usize
            == Self::GetOwnIndexedPropertySlotMethod as usize
        {
            return obj.delete_indexed_internal(ctx, index, throwable);
        }
        let mut slot = Slot::new();
        if !(obj.class.method_table.GetOwnIndexedPropertySlot)(obj, ctx, index, &mut slot) {
            return true;
        }

        if !slot.attributes().is_configurable() {
            if throwable {
                ctx.throw_str("Cannot delete non configurable property");
            }
            return false;
        }

        obj.delete_indexed_internal(ctx, index, throwable)
    }

    pub fn GetNonIndexedPropertySlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> bool {
        gc_frame!(ctx.gc().roots() => obj = *obj);
        loop {
            if obj.get_own_non_indexed_property_slot(ctx, name, slot) {
                break true;
            }
            let proto = *obj.prototype();

            if proto.is_null() {
                break false;
            } else {
                *obj = proto.as_gc();
            }
        }
    }

    pub fn GetIndexedPropertySlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        index: u32,
        slot: &mut Slot,
    ) -> bool {
        gc_frame!(ctx.gc().roots() => obj = *obj);
        loop {
            if obj.get_own_indexed_property_slot(ctx, index, slot) {
                return true;
            }

            let proto: Option<Gc<Object>> = (*obj.prototype()).into();
            match proto {
                Some(proto) => *obj = proto,
                _ => break false,
            }
        }
    }

    pub fn PutNonIndexedSlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        name: Symbol,
        val: Value,
        slot: &mut Slot,
        throwable: bool,
    ) {
        if !obj.can_put(ctx, name, slot) {
            if throwable {
                ctx.throw_str("put failed");
            }

            return;
        }
        if !slot.is_not_found() {
            if let Some(base) = slot.base() {
                if Gc::ptr_eq(*base, obj.as_dyn()) && slot.attributes().is_data() {
                    gc_frame!(ctx.gc().roots() => desc = *DataDescriptor::new(val, UNDEF_ENUMERABLE | UNDEF_CONFIGURABLE | UNDEF_WRITABLE));
                    obj.define_own_non_indexed_property_slot(ctx, name, &desc, slot, throwable);
                    return;
                }
            }

            if slot.attributes().is_accessor() {
                /*letroot!(ac = stack, slot.accessor());
                let mut tmp = [JsValue::encode_undefined_value()];
                letroot!(
                    args = stack,
                    Arguments::new(JsValue::encode_object_value(*obj), &mut tmp)
                );
                *args.at_mut(0) = val;
                return ac
                    .setter()
                    .get_object()
                    .downcast::<JsObject>()
                    .unwrap()
                    .as_function_mut()
                    .call(ctx, &mut args, JsValue::encode_object_value(*obj))
                    .map(|_| ());
                    */
                todo!()
            }
        }

        gc_frame!(ctx.gc().roots() => desc = *DataDescriptor::new(val, W | C | E));
        obj.define_own_non_indexed_property_slot(ctx, name, &desc, slot, throwable);
    }

    pub fn PutIndexedSlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        index: u32,
        val: Value,
        slot: &mut Slot,
        throwable: bool,
    ) {
        if index < MAX_VECTOR_SIZE as u32
            && obj.indexed.dense()
            && obj.class.method_table.GetOwnIndexedPropertySlot as usize
                == Self::GetOwnIndexedPropertySlotMethod as usize
            && (obj.prototype().is_null() || obj.prototype().as_gc().has_indexed_property())
        {
            slot.mark_put_result(PutResultType::IndexedOptimized, index);
            obj.define_own_indexe_value_dense_internal(ctx, index, val, false);
            ctx.gc().write_barrier(*obj);
            return;
        }

        if !obj.can_put_indexed(ctx, index, slot) {
            if throwable {
                ctx.throw_str("put failed")
            }
            return;
        }
        if !slot.is_not_found() {
            if let Some(base) = slot.base() {
                if Gc::ptr_eq(*base, obj.as_dyn()) && slot.attributes().is_data() {
                    obj.define_own_indexed_property_slot(
                        ctx,
                        index,
                        &*DataDescriptor::new(
                            val,
                            UNDEF_ENUMERABLE | UNDEF_CONFIGURABLE | UNDEF_WRITABLE,
                        ),
                        slot,
                        throwable,
                    );
                    return;
                }
            }

            if slot.attributes().is_accessor() {
                /*
                letroot!(ac = stack, slot.accessor());
                let mut tmp = [JsValue::encode_undefined_value()];
                letroot!(
                    args = stack,
                    Arguments::new(JsValue::encode_object_value(*obj), &mut tmp,)
                );

                *args.at_mut(0) = val;
                return ac
                    .setter()
                    .get_object()
                    .downcast::<JsObject>()
                    .unwrap()
                    .as_function_mut()
                    .call(ctx, &mut args, ac.setter())
                    .map(|_| ());
                */
                todo!()
            }
        }

        obj.define_own_indexed_property_slot(
            ctx,
            index,
            &*DataDescriptor::new(val, W | E | C),
            slot,
            throwable,
        );
    }

    pub fn GetOwnNonIndexedPropertySlotMethod(
        obj: &mut Gc<Object>,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> bool {
        let entry = obj.structure.get(ctx, name);

        if !entry.is_not_found() {
            slot.set_woffset(
                *obj.direct(entry.offset as _),
                entry.attrs as _,
                Some(obj.as_dyn()),
                entry.offset,
            );

            return true;
        }
        false
    }

    #[allow(unused_variables)]
    pub fn GetOwnPropertyNamesMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        collector: &mut dyn FnMut(Symbol, u32),
        mode: EnumerationMode,
    ) {
        if obj.indexed.dense() {
            for index in 0..obj.indexed.vector.size() {
                let it = obj.indexed.vector.at(index);
                if !it.is_empty() {
                    collector(Symbol::Index(index as _), u32::MAX);
                }
            }
        }

        if let Some(map) = &obj.indexed.map {
            map.for_each(|k, v| {
                if mode == EnumerationMode::IncludeNotEnumerable || v.attributes().is_enumerable() {
                    collector(Symbol::Index(*k), u32::MAX);
                }
                true
            });
        }

        obj.structure.get_own_property_names(
            ctx,
            mode == EnumerationMode::IncludeNotEnumerable,
            collector,
        );
    }
    #[allow(unused_variables)]
    pub fn GetPropertyNamesMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        collector: &mut dyn FnMut(Symbol, u32),
        mode: EnumerationMode,
    ) {
        obj.get_own_property_names(ctx, collector, mode);
        let mut obj = unsafe { obj.prototype_mut() };
        loop {
            if obj.is_null() {
                break;
            }
            obj.as_gc().get_own_property_names(ctx, collector, mode);
            obj = unsafe { obj.prototype_mut() };
        }
    }
    pub fn DefaultValueMethod(obj: &mut Gc<Self>, ctx: &mut VM, hint: Hint) -> Value {
        macro_rules! try_ {
            ($sym: expr) => {
                let m = obj.get(ctx, $sym);

                if m.is_function() {
                    let res = ctx.ocall0(Value::new(*obj), m);
                    if res.is_primitive() || (res.is_null() || res.is_undefined()) {
                        return res;
                    }
                }
            };
        }

        if hint == Hint::String {
            try_!("to_string".intern());
            try_!("value_of".intern());
        } else {
            try_!("value_of".intern());
            try_!("to_string".intern());
        }

        ctx.throw_str("invalid default value")
    }

    pub fn GetOwnIndexedPropertySlotMethod(
        obj: &mut Gc<Self>,
        _ctx: &mut VM,
        index: u32,
        slot: &mut Slot,
    ) -> bool {
        if obj.indexed.dense() && index < obj.indexed.vector.size() as u32 {
            let value = obj.indexed.vector.at(index as _);
            if value.is_empty() {
                return false;
            }

            slot.set_1(*value, object_data(), Some(obj.as_dyn()));
            return true;
        }
        if let Some(map) = obj.indexed.map.as_ref() {
            if index < obj.indexed.length() {
                let it = map.get(&index);
                if let Some(it) = it {
                    slot.set_from_slot(it, Some((*obj).as_dyn()));
                    return true;
                }
            }
        }

        false
    }

    pub fn DefineOwnNonIndexedPropertySlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        name: Symbol,
        desc: &PropertyDescriptor,
        slot: &mut Slot,
        throwable: bool,
    ) -> bool {
        if !slot.is_used() {
            obj.get_own_property_slot(ctx, name, slot);
        }

        if !slot.is_not_found() {
            if let Some(base) = slot.base() {
                if Gc::ptr_eq(*base, obj.as_dyn()) {
                    let mut returned = false;
                    if slot.is_defined_property_accepted(ctx, desc, throwable, &mut returned) {
                        if slot.has_offset() {
                            let old = slot.attributes();
                            slot.merge(ctx, desc);
                            if old != slot.attributes() {
                                let new_struct = obj.structure.change_attributes_transition(
                                    ctx,
                                    name,
                                    slot.attributes(),
                                );
                                obj.structure = new_struct;
                            }
                            *obj.direct_mut(slot.offset() as _) = slot.value();
                            ctx.gc().write_barrier(*obj);
                            slot.mark_put_result(PutResultType::Replace, slot.offset());
                        } else {
                            let mut offset = 0;
                            slot.merge(ctx, desc);
                            let new_struct = obj.structure.add_property_transition(
                                ctx,
                                name,
                                slot.attributes(),
                                &mut offset,
                            );
                            obj.structure = new_struct;
                            let s = &obj.structure;
                            let sz = s.storage_capacity();
                            gc_frame!(ctx.gc().roots() => slots = obj.slots);
                            // letroot!(slots = stack, obj.slots);

                            slots.resize(ctx.gc(), sz as _);
                            obj.slots = *slots;
                            *obj.direct_mut(offset as _) = slot.value();
                            ctx.gc().write_barrier(*obj);
                            slot.mark_put_result(PutResultType::New, offset);
                        }
                    }
                    return returned;
                }
            }
        }

        if !obj.is_extensible() {
            if throwable {
                ctx.throw_str("object not extensible");
            }

            return false;
        }

        let mut offset = 0;
        let stored = StoredSlot::new(ctx, desc);
        let s = obj
            .structure
            .add_property_transition(ctx, name, stored.attributes(), &mut offset);

        obj.structure = s;
        ctx.gc().write_barrier(*obj);
        let s = &obj.structure;
        let sz = s.storage_capacity();
        gc_frame!(ctx.gc().roots() => slots = obj.slots);
        slots.resize(ctx.gc(), sz as _);
        obj.slots = *slots;

        *obj.direct_mut(offset as _) = stored.value();
        slot.mark_put_result(PutResultType::New, offset);
        slot.base = Some(obj.as_dyn());
        ctx.gc().write_barrier(*obj);
        true
    }

    pub fn DefineOwnIndexedPropertySlotMethod(
        obj: &mut Gc<Self>,
        ctx: &mut VM,
        index: u32,
        desc: &PropertyDescriptor,
        slot: &mut Slot,
        throwable: bool,
    ) -> bool {
        if obj.class.method_table.GetOwnIndexedPropertySlot as usize
            != Self::GetOwnIndexedPropertySlotMethod as usize
        {
            // We should reject following case
            //   var str = new String('str');
            //   Object.defineProperty(str, '0', { value: 0 });
            if !slot.is_used() {
                obj.get_own_indexed_property_slot(ctx, index, slot);
            }

            let mut returned = false;
            if !slot.is_not_found() {
                if let Some(base) = slot.base() {
                    if Gc::ptr_eq(*base, obj.as_dyn())
                        && !slot.is_defined_property_accepted(ctx, desc, throwable, &mut returned)
                    {
                        return returned;
                    }
                }
            }
        }

        let r = obj.define_own_indexed_property_internal(ctx, index as _, desc, throwable);
        ctx.gc().write_barrier(*obj);
        r
    }

    fn define_own_indexe_value_dense_internal(
        &mut self,
        ctx: &mut VM,
        index: u32,
        val: Value,
        absent: bool,
    ) {
        if index < self.indexed.vector.size() as u32 {
            if !absent {
                *self.indexed.vector.at_mut(index as _) = val;
            } else {
                *self.indexed.vector.at_mut(index as _) = Value::encode_undefined_value();
            }
        } else {
            if !self.structure.is_indexed() {
                let s = self.structure.change_indexed_transition(ctx);

                self.structure = s;
            }
            /*let stack = ctx.shadowstack();
            letroot!(vector = stack, self.indexed.vector);
            vector.mut_handle().resize(ctx.heap(), index + 1);
            self.indexed.vector = *vector;*/
            self.indexed.vector.resize(ctx.gc(), index as usize + 1);
        }
        if !absent {
            *self.indexed.vector.at_mut(index as _) = val;
        } else {
            *self.indexed.vector.at_mut(index as _) = Value::encode_undefined_value();
        }
        if index >= self.indexed.length() {
            self.indexed.set_length(index + 1);
        }
    }

    pub fn define_own_indexed_property_internal(
        &mut self,
        ctx: &mut VM,
        index: usize,
        desc: &PropertyDescriptor,
        throwable: bool,
    ) -> bool {
        if index >= self.indexed.length() as usize && !self.indexed.writable() {
            if throwable {
                ctx.throw_str(
                    "adding an element to the array which length is not writable is rejected",
                );
            }
            return false;
        }

        if self.indexed.dense() {
            if desc.is_default() {
                if index < MAX_VECTOR_SIZE {
                    self.define_own_indexe_value_dense_internal(
                        ctx,
                        index as _,
                        desc.value(),
                        desc.is_value_absent(),
                    );

                    return true;
                }
            } else {
                if is_absent_descriptor(desc)
                    && index < self.indexed.vector.size()
                    && !self.indexed.vector.at(index).is_empty()
                {
                    if !desc.is_value_absent() {
                        *self.indexed.vector.at_mut(index) = desc.value();
                    }
                    return true;
                }

                if index < MAX_VECTOR_SIZE {
                    self.indexed.make_sparse(ctx);
                }
            }
        }

        let mut sparse = self.indexed.ensure_map(ctx, 8);
        match sparse.get_mut(&(index as u32)) {
            Some(entry) => {
                let mut returned = false;
                if entry.is_defined_property_accepted(ctx, desc, throwable, &mut returned) {
                    entry.merge(ctx, desc);
                }
                returned
            }
            None if !self.is_extensible() => {
                if throwable {
                    ctx.throw_str("object not extensible");
                }
                false
            }
            None => {
                if !self.structure.is_indexed() {
                    let s = self.structure.change_indexed_transition(ctx);
                    self.structure = s;
                }
                if index >= self.indexed.length() as usize {
                    self.indexed.set_length(index as u32 + 1);
                }
                let slot = StoredSlot::new(ctx, desc);
                sparse.insert(ctx, index as _, slot);
                true
            }
        }
    }

    pub fn is_extensible(&self) -> bool {
        (self.flags & OBJ_FLAG_EXTENSIBLE) != 0
    }
}

impl Nullable<Object> {
    pub fn prototype(&self) -> &Nullable<Object> {
        self.structure.prototype()
    }
    pub unsafe fn prototype_mut(&mut self) -> &mut Nullable<Object> {
        self.structure.prototype_mut()
    }
}
impl Gc<Object> {
    pub fn get_method(&mut self, ctx: &mut VM, name: Symbol) -> Value {
        let val = self.get(ctx, name);
        if val.is_function() {
            val
        } else {
            ctx.throw_str(format!(
                "Property '{}' is not a method: '{}'",
                name.description(symbol_table()),
                val
            ))
        }
    }

    pub fn to_primitive(&mut self, ctx: &mut VM, hint: Hint) -> Value {
        let exotic_to_prim =
            crate::vm::catch_trap::<Value>(ctx, |vm| self.get_method(vm, "to_primitive".intern()));
        match exotic_to_prim {
            Ok(mut prim) => {
                gc_frame!(ctx.gc().roots() => prim: Value);
                let hint = match hint {
                    Hint::Number | Hint::None => Value::Str(ctx.gc().str("number")),
                    Hint::String => Value::Str(ctx.gc().str("string")),
                };
                ctx.ocall1(Value::new(*self), *prim, hint)
            }
            Err(_) => (self.class.method_table.DefaultValue)(self, ctx, hint),
        }
    }

    pub fn get_property_slot(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool {
        if let Symbol::Index(index) = name {
            self.get_indexed_property_slot(ctx, index, slot)
        } else {
            self.get_non_indexed_property_slot(ctx, name, slot)
        }
    }

    pub fn get_property(&mut self, ctx: &mut VM, name: Symbol) -> PropertyDescriptor {
        let mut slot = Slot::new();
        if self.get_property_slot(ctx, name, &mut slot) {
            return slot.to_descriptor();
        }
        PropertyDescriptor::new_val(Value::encode_empty_value(), AttrSafe::not_found())
    }
    pub fn get_own_property_names(
        &mut self,
        ctx: &mut VM,
        collector: &mut dyn FnMut(Symbol, u32),
        mode: EnumerationMode,
    ) {
        (self.class.method_table.GetOwnPropertyNames)(self, ctx, collector, mode)
    }
    pub fn get_property_names(
        &mut self,
        ctx: &mut VM,
        collector: &mut dyn FnMut(Symbol, u32),
        mode: EnumerationMode,
    ) {
        (self.class.method_table.GetPropertyNames)(self, ctx, collector, mode)
    }
    pub fn put_indexed_slot(
        &mut self,
        ctx: &mut VM,
        index: u32,
        val: Value,
        slot: &mut Slot,
        throwable: bool,
    ) {
        (self.class.method_table.PutIndexedSlot)(self, ctx, index, val, slot, throwable)
    }
    pub fn put_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        val: Value,
        slot: &mut Slot,
        throwable: bool,
    ) {
        if let Symbol::Index(index) = name {
            self.put_indexed_slot(ctx, index, val, slot, throwable)
        } else {
            self.put_non_indexed_slot(ctx, name, val, slot, throwable)
        }
    }

    pub fn get<K: Into<Symbol>>(&mut self, ctx: &mut VM, name: K) -> Value {
        let mut slot = Slot::new();
        self.get_slot(ctx, name.into(), &mut slot)
    }
    pub fn get_slot(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> Value {
        if let Symbol::Index(index) = name {
            self.get_indexed_slot(ctx, index, slot)
        } else {
            self.get_non_indexed_slot(ctx, name, slot)
        }
    }
    pub fn get_indexed_slot(&mut self, ctx: &mut VM, index: u32, slot: &mut Slot) -> Value {
        (self.class.method_table.GetIndexedSlot)(self, ctx, index, slot)
    }
    pub fn put<K: Into<Symbol>, V: Into<Value>>(
        &mut self,
        ctx: &mut VM,
        name: K,
        val: V,
        throwable: bool,
    ) -> () {
        let mut slot = Slot::new();

        self.put_slot(ctx, name.into(), val.into(), &mut slot, throwable)
    }
    pub fn put_non_indexed_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        val: Value,
        slot: &mut Slot,
        throwable: bool,
    ) {
        (self.class.method_table.PutNonIndexedSlot)(self, ctx, name, val, slot, throwable)
    }
    pub fn define_own_property_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        desc: &PropertyDescriptor,
        slot: &mut Slot,
        throwable: bool,
    ) -> bool {
        if let Symbol::Index(index) = name {
            self.define_own_indexed_property_slot(ctx, index, desc, slot, throwable)
        } else {
            self.define_own_non_indexed_property_slot(ctx, name, desc, slot, throwable)
        }
    }
    pub fn define_own_property(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        desc: &PropertyDescriptor,
        throwable: bool,
    ) -> bool {
        let mut slot = Slot::new();
        self.define_own_property_slot(ctx, name, desc, &mut slot, throwable)
    }
    pub fn define_own_non_indexed_property_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        desc: &PropertyDescriptor,
        slot: &mut Slot,
        throwable: bool,
    ) -> bool {
        (self.class.method_table.DefineOwnNonIndexedPropertySlot)(
            self, ctx, name, desc, slot, throwable,
        )
    }
    pub fn prototype(&self) -> &Nullable<Object> {
        self.structure.prototype()
    }
    pub unsafe fn prototype_mut(&mut self) -> &mut Nullable<Object> {
        self.structure.prototype_mut()
    }

    pub fn can_put(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool {
        if let Symbol::Index(index) = name {
            self.can_put_indexed(ctx, index, slot)
        } else {
            self.can_put_non_indexed(ctx, name, slot)
        }
    }

    pub fn has_indexed_property(&self) -> bool {
        let mut obj = *self;
        loop {
            if obj.structure.is_indexed() {
                return true;
            }
            match obj.prototype() {
                proto if proto.is_not_null() => obj = proto.as_gc(),
                _ => break false,
            }
        }
    }
    pub fn can_put_non_indexed(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool {
        if self.get_non_indexed_property_slot(ctx, name, slot) {
            if slot.attributes().is_accessor() {
                return slot.accessor().setter().is_pointer()
                    && !slot.accessor().setter().is_empty();
            } else {
                return slot.attributes().is_writable();
            }
        }
        self.is_extensible()
    }

    pub fn can_put_indexed(&mut self, ctx: &mut VM, index: u32, slot: &mut Slot) -> bool {
        if self.get_indexed_property_slot(ctx, index, slot) {
            if slot.attributes().is_accessor() {
                return slot.accessor().setter().is_pointer()
                    && !slot.accessor().setter().is_empty();
            } else {
                return slot.attributes().is_writable();
            }
        }
        self.is_extensible()
    }

    pub fn define_own_indexed_property_slot(
        &mut self,
        ctx: &mut VM,
        index: u32,
        desc: &PropertyDescriptor,
        slot: &mut Slot,
        throwable: bool,
    ) -> bool {
        (self.class.method_table.DefineOwnIndexedPropertySlot)(
            self, ctx, index, desc, slot, throwable,
        )
    }

    pub fn get_non_indexed_slot(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> Value {
        (self.class.method_table.GetNonIndexedSlot)(self, ctx, name, slot)
    }

    pub fn get_own_property_slot(&mut self, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool {
        if let Symbol::Index(index) = name {
            self.get_own_indexed_property_slot(ctx, index, slot)
        } else {
            self.get_own_non_indexed_property_slot(ctx, name, slot)
        }
    }
    pub fn get_own_indexed_property_slot(
        &mut self,
        ctx: &mut VM,
        index: u32,
        slot: &mut Slot,
    ) -> bool {
        (self.class.method_table.GetOwnIndexedPropertySlot)(self, ctx, index, slot)
        //unsafe { JsObject::GetOwnIndexedPropertySlotMethod(*self, ctx, index, slot) }
    }
    pub fn get_own_non_indexed_property_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> bool {
        (self.class.method_table.GetOwnNonIndexedPropertySlot)(self, ctx, name, slot)
    }
    pub fn get_non_indexed_property_slot(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        slot: &mut Slot,
    ) -> bool {
        (self.class.method_table.GetNonIndexedPropertySlot)(self, ctx, name, slot)
    }
    pub fn get_indexed_property_slot(&mut self, ctx: &mut VM, index: u32, slot: &mut Slot) -> bool {
        (self.class.method_table.GetIndexedPropertySlot)(self, ctx, index, slot)
    }
}

unsafe impl Trace for Object {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.slots.trace(visitor);
        self.structure.trace(visitor);
        self.indexed.trace(visitor);
        self.data.trace(visitor);
    }
}
unsafe impl Finalize for Object {}
impl Managed for Object {}

pub mod property_descriptor {
    use super::*;
    use crate::vm::VM;
    #[derive(Clone, Copy)]
    pub union PropertyLayout {
        data: Value,
        accessors: (Value, Value), // getter,setter
    }
    #[repr(C)]
    #[derive(Clone, Copy)]
    pub struct PropertyDescriptor {
        pub attrs: AttrExternal,
        pub value: PropertyLayout,
    }

    unsafe impl Trace for PropertyDescriptor {
        fn trace(&mut self, visitor: &mut dyn Visitor) {
            if self.attrs.is_accessor() {
                unsafe {
                    self.value.accessors.0.trace(visitor);
                    self.value.accessors.1.trace(visitor);
                }
            } else {
                unsafe {
                    self.value.data.trace(visitor);
                }
            }
        }
    }

    impl Deref for PropertyDescriptor {
        type Target = AttrExternal;
        fn deref(&self) -> &Self::Target {
            &self.attrs
        }
    }

    impl DerefMut for PropertyDescriptor {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.attrs
        }
    }

    impl PropertyDescriptor {
        pub fn get_layout(&self) -> PropertyLayout {
            self.value
        }

        pub fn data_descriptor(val: Value, attrs: u32) -> Self {
            Self {
                attrs: AttrExternal::new(Some(attrs | DATA | UNDEF_GETTER | UNDEF_SETTER)),
                value: PropertyLayout { data: val },
            }
        }

        pub fn accessor_descriptor(getter: Value, setter: Value, attrs: u32) -> Self {
            Self {
                attrs: AttrExternal::new(Some(attrs | ACCESSOR | UNDEF_VALUE | UNDEF_WRITABLE)),
                value: PropertyLayout {
                    accessors: (getter, setter),
                },
            }
        }

        pub fn accessor_setter(setter: Value, attrs: u32) -> Self {
            Self {
                attrs: AttrExternal::new(Some(
                    attrs | ACCESSOR | UNDEF_VALUE | UNDEF_SETTER | UNDEF_WRITABLE,
                )),
                value: PropertyLayout {
                    accessors: (Value::encode_undefined_value(), setter),
                },
            }
        }
        pub fn accessor_getter(getter: Value, attrs: u32) -> Self {
            Self {
                attrs: AttrExternal::new(Some(
                    attrs | ACCESSOR | UNDEF_VALUE | UNDEF_SETTER | UNDEF_WRITABLE,
                )),
                value: PropertyLayout {
                    accessors: (getter, Value::encode_undefined_value()),
                },
            }
        }

        pub fn generic(attrs: u32) -> Self {
            Self {
                attrs: AttrExternal::new(Some(
                    attrs | UNDEF_VALUE | UNDEF_GETTER | UNDEF_SETTER | UNDEF_WRITABLE,
                )),
                value: PropertyLayout {
                    data: Value::encode_empty_value(),
                },
            }
        }

        pub fn new_val(val: Value, attrs: AttrSafe) -> Self {
            Self {
                attrs: AttrExternal::new(Some(attrs.raw())),
                value: PropertyLayout { data: val },
            }
        }

        pub fn new_getter_setter(getter: Value, setter: Value, attrs: AttrSafe) -> Self {
            Self {
                attrs: AttrExternal::new(Some(attrs.raw())),
                value: PropertyLayout {
                    accessors: (getter, setter),
                },
            }
        }

        pub fn value(&self) -> Value {
            unsafe { self.value.data }
        }

        pub fn getter(&self) -> Value {
            unsafe { self.value.accessors.0 }
        }

        pub fn setter(&self) -> Value {
            unsafe { self.value.accessors.1 }
        }
    }

    #[derive(Clone, Copy)]

    pub struct StoredSlot {
        pub(crate) value: Value,
        pub(crate) attributes: AttrSafe,
    }

    unsafe impl Trace for StoredSlot {
        fn trace(&mut self, visitor: &mut dyn Visitor) {
            self.value.trace(visitor);
        }
    }

    unsafe impl Finalize for StoredSlot {}
    impl Managed for StoredSlot {}

    impl StoredSlot {
        pub fn value(&self) -> Value {
            self.value
        }
        pub fn set_value(&mut self, val: Value) {
            self.value = val;
        }

        pub fn attributes(&self) -> AttrSafe {
            self.attributes
        }
        pub fn set_attributes(&mut self, attrs: AttrSafe) {
            self.attributes = attrs;
        }

        pub fn set(&mut self, value: Value, attrs: AttrSafe) {
            self.value = value;
            self.attributes = attrs;
        }

        pub fn empty() -> Self {
            Self {
                value: Value::encode_empty_value(),
                attributes: object_data(),
            }
        }

        pub fn new_raw(value: Value, attributes: AttrSafe) -> Self {
            Self { value, attributes }
        }

        pub fn to_descriptor(&self) -> PropertyDescriptor {
            if self.attributes().is_data() {
                return PropertyDescriptor::data_descriptor(self.value, self.attributes().raw());
            }
            let accessor = self.accessor();
            PropertyDescriptor::accessor_descriptor(
                accessor.getter(),
                accessor.setter(),
                self.attributes.raw,
            )
        }
        #[allow(unused_variables)]
        pub fn get(&self, vm: &mut VM, this_binding: Value) -> Value {
            if self.attributes.is_accessor() {
                return self.accessor().invoke_getter(vm, this_binding);
            }
            return self.value;
        }

        pub fn accessor(&self) -> Gc<Accessor> {
            assert!(self.attributes.is_accessor());
            unsafe { self.value.get_object().downcast_unchecked() }
        }
        pub fn is_defined_property_accepted(
            &self,
            vm: &mut VM,
            desc: &PropertyDescriptor,
            throwable: bool,
            returned: &mut bool,
        ) -> bool {
            *returned = true;
            macro_rules! reject {
                ($str: expr) => {{
                    *returned = false;
                    if throwable {
                        vm.throw_str($str);
                    }
                    return false;
                }};
            }

            if desc.is_absent() {
                *returned = true;
                return false;
            }
            if self.merge_with_no_effect(vm, desc) {
                *returned = true;
                return false;
            }
            if !self.attributes().is_configurable() {
                if desc.is_configurable() {
                    reject!("changing [[Configurable]] of unconfigurable property not allowed");
                }
                if !desc.is_enumerable_absent()
                    && self.attributes().is_enumerable() != desc.is_enumerable()
                {
                    reject!("changing [[Enumerable]] of unconfigurable property not allowed");
                }
            }
            // step 9
            if desc.is_generic() {
            } else if self.attributes().is_data() != desc.is_data() {
                if !self.attributes().is_configurable() {
                    reject!("changing descriptor type of unconfigurable property not allowed");
                }
            } else if self.attributes().is_data() {
                if !self.attributes().is_configurable() && !self.attributes().is_writable() {
                    if desc.is_writable() {
                        reject!("changing [[Writable]] of unconfigurable property not allowed");
                    }

                    reject!("changing [[Value]] of readonly property not allowed");
                }
            } else if !self.attributes().is_configurable() {
                let lhs = self.accessor();
                let rhs = AccessorDescriptor { parent: *desc };

                if (!rhs.is_setter_absent() && (lhs.setter() != rhs.set()))
                    || (!rhs.is_getter_absent() && (lhs.getter() != rhs.get()))
                {
                    reject!("changing [[Set]] or [[Get]] of unconfigurable property not allowed")
                }
            }
            *returned = true;
            true
        }

        /// if desc merged to current and has no effect, return true
        pub fn merge_with_no_effect(&self, vm: &mut VM, desc: &PropertyDescriptor) -> bool {
            if !desc.is_configurable_absent()
                && desc.is_configurable() != self.attributes().is_configurable()
            {
                return false;
            }

            if !desc.is_enumerable_absent()
                && desc.is_enumerable() != self.attributes().is_enumerable()
            {
                return false;
            }

            if desc.ty() != self.attributes().ty() {
                return false;
            }

            if desc.is_data() {
                let data = DataDescriptor { parent: *desc };
                if !data.is_writable_absent()
                    && data.is_writable() != self.attributes().is_writable()
                {
                    return false;
                }

                if data.is_value_absent() {
                    return true;
                }
                value_eq(vm, &data.value(), &self.value).bool()
            } else if desc.is_accessor() {
                let ac = self.accessor();
                unsafe {
                    desc.value.accessors.0 == ac.getter() && desc.value.accessors.1 == ac.setter()
                }
            } else {
                true
            }
        }

        pub fn merge(&mut self, vm: &mut VM, desc: &PropertyDescriptor) {
            let mut attr = AttrExternal::new(Some(self.attributes().raw()));
            if !desc.is_configurable_absent() {
                attr.set_configurable(desc.is_configurable());
            }
            if !desc.is_enumerable_absent() {
                attr.set_enumerable(desc.is_enumerable())
            }
            if desc.is_generic() {
                self.attributes = AttrSafe::un_safe(attr);
                return;
            }

            if desc.is_data() {
                attr.set_data();
                let data = DataDescriptor { parent: *desc };

                if !data.is_value_absent() {
                    self.value = data.value();
                }
                if !data.is_writable_absent() {
                    attr.set_writable(data.is_writable());
                }
            } else {
                attr.set_accessor();
                let accs = AccessorDescriptor { parent: *desc };

                let mut ac = if self.attributes().is_accessor() {
                    self.accessor()
                } else {
                    let ac = Accessor::new(
                        vm,
                        Value::encode_undefined_value(),
                        Value::encode_undefined_value(),
                    );
                    self.value = Value::encode_object_value(ac.as_dyn());
                    ac
                };
                if !accs.is_getter_absent() {
                    ac.set_getter(accs.get());
                }
                if !accs.is_setter_absent() {
                    ac.set_setter(accs.set());
                }
            }
            self.attributes = AttrSafe::un_safe(attr);
        }

        pub fn new(vm: &mut VM, desc: &PropertyDescriptor) -> Self {
            let mut this = Self {
                value: Value::encode_undefined_value(),
                attributes: AttrSafe::not_found(),
            };
            let mut attributes = AttrExternal::new(None);
            attributes.set_configurable(desc.is_configurable());
            attributes.set_enumerable(desc.is_enumerable());
            if desc.is_data() {
                let data = DataDescriptor { parent: *desc };
                if !data.is_value_absent() {
                    this.value = data.value();
                }
                attributes.set_writable(data.is_writable());
                this.attributes = create_data(attributes);
            } else if desc.is_accessor() {
                let ac = AccessorDescriptor { parent: *desc };
                let accessor = Accessor::new(vm, ac.get(), ac.set());
                this.value = Value::encode_object_value(accessor.as_dyn());
                this.attributes = create_accessor(attributes);
            } else {
                this.attributes = create_data(attributes);
            }

            this
        }
    }

    #[repr(C)]
    pub struct AccessorDescriptor {
        pub parent: PropertyDescriptor,
    }

    impl Deref for AccessorDescriptor {
        type Target = PropertyDescriptor;
        fn deref(&self) -> &Self::Target {
            &self.parent
        }
    }

    impl DerefMut for AccessorDescriptor {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.parent
        }
    }

    impl AccessorDescriptor {
        pub fn new(get: Value, set: Value, attrs: u32) -> Self {
            Self {
                parent: PropertyDescriptor::accessor_descriptor(get, set, attrs),
            }
        }

        pub fn get(&self) -> Value {
            unsafe { self.value.accessors.0 }
        }

        pub fn set(&self) -> Value {
            unsafe { self.value.accessors.1 }
        }

        pub fn set_get(&mut self, get: Value) {
            self.value.accessors.0 = get;
        }

        pub fn set_set(&mut self, set: Value) {
            self.value.accessors.1 = set;
        }
    }

    pub struct DataDescriptor {
        pub parent: PropertyDescriptor,
    }
    impl Deref for DataDescriptor {
        type Target = PropertyDescriptor;
        fn deref(&self) -> &Self::Target {
            &self.parent
        }
    }

    impl DerefMut for DataDescriptor {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.parent
        }
    }

    impl DataDescriptor {
        pub fn set_value(&mut self, val: Value) {
            self.value.data = val;
        }

        pub fn value(&self) -> Value {
            unsafe { self.value.data }
        }

        pub fn new(val: Value, attrs: u32) -> Self {
            Self {
                parent: PropertyDescriptor::data_descriptor(val, attrs),
            }
        }
    }

    pub struct GenericDescriptor {
        pub parent: PropertyDescriptor,
    }
    impl Deref for GenericDescriptor {
        type Target = PropertyDescriptor;
        fn deref(&self) -> &Self::Target {
            &self.parent
        }
    }

    impl DerefMut for GenericDescriptor {
        fn deref_mut(&mut self) -> &mut Self::Target {
            &mut self.parent
        }
    }

    impl GenericDescriptor {
        pub fn new(attrs: u32) -> Self {
            Self {
                parent: PropertyDescriptor::generic(attrs),
            }
        }
    }

    pub struct Accessor {
        pub(crate) getter: Value,
        pub(crate) setter: Value,
    }

    impl Accessor {
        pub fn getter(&self) -> Value {
            self.getter
        }

        pub fn set_getter(&mut self, val: Value) {
            self.getter = val;
        }

        pub fn set_setter(&mut self, val: Value) {
            self.setter = val;
        }

        pub fn setter(&self) -> Value {
            self.setter
        }
        pub fn new(vm: &mut VM, getter: Value, setter: Value) -> Gc<Self> {
            let this = Self { getter, setter };
            vm.gc().fixed(this)
        }

        pub fn invoke_getter(&self, ctx: &mut VM, this_binding: Value) -> Value {
            let _ = ctx;
            let _ = this_binding;
            /*if self.getter().is_callable() {
                let stack = ctx.shadowstack();
                crate::letroot!(args = stack, Arguments::new(this_binding, &mut []));

                self.getter()
                    .get_object()
                    .downcast::<JsObject>()
                    .unwrap()
                    .as_function_mut()
                    .call(ctx, &mut args, self.getter())
            } else {
                Ok(Value::encode_undefined_value())
            }*/
            todo!()
        }
    }

    unsafe impl Trace for Accessor {
        fn trace(&mut self, visitor: &mut dyn Visitor) {
            self.getter.trace(visitor);
            self.setter.trace(visitor);
        }
    }

    unsafe impl Finalize for Accessor {}
    impl Managed for Accessor {}
}

pub struct Slot {
    pub parent: StoredSlot,
    pub(crate) base: Option<Gc<dyn Managed>>,
    pub(crate) offset: u32,
    pub(crate) flags: u32,
}

impl Slot {
    pub const FLAG_USED: u32 = 1;
    pub const FLAG_CACHEABLE: u32 = 2;
    pub const FLAG_PUT_CACHEABLE: u32 = 4;
    pub const FLAG_FORCE_PUT_UNCACHEABLE: u32 = 8;
    pub const FLAG_INIT: u32 = Slot::FLAG_CACHEABLE;
    pub const PUT_SHIFT: u32 = 4;
    pub const PUT_MASK: u32 = 3;
    fn is_cacheable(&self) -> bool {
        (self.flags & Self::FLAG_CACHEABLE) != 0
            && self
                .base
                .as_ref()
                .map(|obj| obj.is::<Object>())
                .unwrap_or(false)
    }
    fn is_put_force_unchacheable(&self) -> bool {
        (self.flags & Self::FLAG_FORCE_PUT_UNCACHEABLE) != 0
    }

    fn set_put_result_type(&mut self, ty: PutResultType) {
        self.flags &= !(Self::PUT_MASK << Self::PUT_SHIFT);
        self.flags |= (ty as u32) << Self::PUT_SHIFT
    }

    pub fn put_result_type(&self) -> PutResultType {
        unsafe { std::mem::transmute((self.flags >> Self::PUT_SHIFT) & Self::PUT_MASK) }
    }

    pub fn mark_put_result(&mut self, ty: PutResultType, offset: u32) {
        self.set_put_result_type(ty);
        self.offset = offset;
        self.flags |= Self::FLAG_PUT_CACHEABLE;
    }

    pub fn is_used(&self) -> bool {
        (self.flags & Self::FLAG_USED) != 0
    }

    pub fn clear(&mut self) {
        self.set(Value::encode_empty_value(), AttrSafe::not_found());
        self.flags = Self::FLAG_INIT;
        self.base = None;
        self.offset = u32::MAX;
    }

    pub fn make_used(&mut self) {
        self.flags &= !Self::FLAG_USED;
    }

    pub fn is_put_cacheable(&self) -> bool {
        (self.flags & Self::FLAG_PUT_CACHEABLE) != 0 && !self.is_put_force_unchacheable()
    }

    pub fn make_put_uncacheable(&mut self) {
        self.flags |= Self::FLAG_FORCE_PUT_UNCACHEABLE;
    }

    pub fn make_uncacheable(&mut self) {
        self.flags &= !Self::FLAG_CACHEABLE;
    }

    pub fn offset(&self) -> u32 {
        self.offset
    }

    pub fn base(&self) -> &Option<Gc<dyn Managed>> {
        &self.base
    }

    pub fn set_1(&mut self, value: Value, attributes: AttrSafe, obj: Option<Gc<dyn Managed>>) {
        self.set(value, attributes);
        self.make_used();
        self.make_uncacheable();
        self.base = obj;
        self.offset = u32::MAX;
    }

    pub fn set_woffset(
        &mut self,
        value: Value,
        attributes: AttrSafe,
        obj: Option<Gc<dyn Managed>>,
        offset: u32,
    ) {
        self.set(value, attributes);
        self.make_used();

        self.base = obj;
        self.offset = offset;
    }

    pub fn set_from_slot(&mut self, slot: &StoredSlot, obj: Option<Gc<dyn Managed>>) {
        self.set_1(slot.value(), slot.attributes(), obj);
    }

    pub fn is_load_cacheable(&self) -> bool {
        self.is_cacheable() && self.attributes().is_data()
    }

    pub fn is_store_cacheable(&self) -> bool {
        self.is_cacheable() && self.attributes().is_simple_data()
    }

    pub fn has_offset(&self) -> bool {
        self.offset != u32::MAX
    }

    pub fn is_not_found(&self) -> bool {
        self.attributes().is_not_found()
    }

    pub fn new() -> Self {
        Self {
            parent: StoredSlot::new_raw(Value::encode_undefined_value(), AttrSafe::not_found()),
            offset: u32::MAX,
            flags: Self::FLAG_INIT,
            base: None,
        }
    }
}

impl Default for Slot {
    fn default() -> Self {
        Self::new()
    }
}

impl Managed for Slot {}

unsafe impl Trace for Slot {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.base.trace(visitor);
        self.value.trace(visitor);
    }
}

unsafe impl Finalize for Slot {}

pub static OBJECT_CLASS: &'static Class = &Class {
    name: "Object",
    method_table: js_method_table!(Object),
    previous: None,
};

#[cfg(test)]
mod tests {
    use crate::{gc_frame, value::nanbox::Value, vm::VM};

    use super::Object;

    #[test]
    fn test_object() {
        let mut vm = VM::new(None);

        gc_frame!(vm.gc().roots() => obj = Object::new_empty(&mut vm));
        vm.gc().collect(0, &mut []);
        let x = vm.gc().str("hello!");
        obj.put(&mut vm, "b", Value::Str(x), false);
        vm.gc().write_barrier(*obj);
        obj.put(vm, "a", Value::encode_int32(42), false);
        vm.gc().collect(1, &mut []);
        let prop = obj.get(&mut vm, "a");
        assert_eq!(prop.get_int32(), 42);
        let prop = obj.get(&mut vm, "b");
        assert_eq!(&**prop.str(), "hello!");
    }
}

pub fn array_length(vm: &mut VM, object: &mut Gc<Object>) -> usize {
    if Object::is(object, ARRAY_CLASS) {
        return object.indexed().length() as _;
    }
    let id = vm.id(Id::Length);

    let length = object.get(vm, id);

    length.to_length(vm)
}
