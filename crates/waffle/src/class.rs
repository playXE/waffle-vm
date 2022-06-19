use crate::object::property_descriptor::PropertyDescriptor;

use super::memory::gcwrapper::*;
use super::object::*;
use super::{value::*, vm::*};
pub type GetNonIndexedSlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> Value;

pub type GetIndexedSlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, index: u32, slot: &mut Slot) -> Value;
pub type GetNonIndexedPropertySlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool;
pub type GetIndexedPropertySlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, index: u32, slot: &mut Slot) -> bool;
pub type GetOwnNonIndexedPropertySlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, name: Symbol, slot: &mut Slot) -> bool;
pub type GetOwnIndexedPropertySlotType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, index: u32, slot: &mut Slot) -> bool;
pub type PutNonIndexedSlotType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    name: Symbol,
    val: Value,
    slot: &mut Slot,
    throwable: bool,
);
pub type PutIndexedSlotType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    index: u32,
    val: Value,
    slot: &mut Slot,
    throwable: bool,
);
pub type DeleteNonIndexedType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, name: Symbol, throwable: bool) -> bool;
pub type DeleteIndexedType =
    fn(obj: &mut Gc<Object>, ctx: &mut VM, index: u32, throwable: bool) -> bool;

pub type DefineOwnNonIndexedPropertySlotType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    name: Symbol,
    desc: &PropertyDescriptor,
    slot: &mut Slot,
    throwable: bool,
) -> bool;
pub type DefineOwnIndexedPropertySlotType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    index: u32,
    desc: &PropertyDescriptor,
    slot: &mut Slot,
    throwable: bool,
) -> bool;
pub type GetPropertyNamesType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    collector: &mut dyn FnMut(Symbol, u32),
    mode: EnumerationMode,
);
pub type GetOwnPropertyNamesType = fn(
    obj: &mut Gc<Object>,
    ctx: &mut VM,
    collector: &mut dyn FnMut(Symbol, u32),
    mode: EnumerationMode,
);

pub type DefaultValueType = fn(obj: &mut Gc<Object>, ctx: &mut VM, hint: Hint) -> Value;
#[derive(Clone, Copy)]
#[repr(C)]
#[allow(non_snake_case)]
pub struct MethodTable {
    pub GetNonIndexedSlot: GetNonIndexedSlotType,
    pub GetIndexedSlot: GetIndexedSlotType,
    pub GetNonIndexedPropertySlot: GetNonIndexedPropertySlotType,
    pub GetIndexedPropertySlot: GetIndexedPropertySlotType,
    pub GetOwnNonIndexedPropertySlot: GetOwnNonIndexedPropertySlotType,
    pub GetOwnIndexedPropertySlot: GetOwnIndexedPropertySlotType,
    pub PutNonIndexedSlot: PutNonIndexedSlotType,
    pub PutIndexedSlot: PutIndexedSlotType,
    pub DeleteNonIndexed: DeleteNonIndexedType,
    pub DeleteIndexed: DeleteIndexedType,
    pub DefineOwnNonIndexedPropertySlot: DefineOwnNonIndexedPropertySlotType,
    pub DefineOwnIndexedPropertySlot: DefineOwnIndexedPropertySlotType,
    pub GetPropertyNames: GetPropertyNamesType,
    pub GetOwnPropertyNames: GetOwnPropertyNamesType,
    pub DefaultValue: DefaultValueType,
}

pub struct Class {
    pub name: &'static str,
    pub method_table: MethodTable,
    pub previous: Option<&'static Class>,
}

#[macro_export]
macro_rules! js_method_table {
    ($class: ident) => {
        MethodTable {
            GetNonIndexedSlot: $class::GetNonIndexedSlotMethod,
            GetIndexedSlot: $class::GetIndexedSlotMethod,
            GetNonIndexedPropertySlot: $class::GetNonIndexedPropertySlotMethod,
            GetIndexedPropertySlot: $class::GetIndexedPropertySlotMethod,
            GetOwnNonIndexedPropertySlot: $class::GetOwnNonIndexedPropertySlotMethod,
            GetOwnIndexedPropertySlot: $class::GetOwnIndexedPropertySlotMethod,
            PutNonIndexedSlot: $class::PutNonIndexedSlotMethod,
            PutIndexedSlot: $class::PutIndexedSlotMethod,
            DeleteNonIndexed: $class::DeleteNonIndexedMethod,
            DeleteIndexed: $class::DeleteIndexedMethod,
            DefineOwnNonIndexedPropertySlot: $class::DefineOwnNonIndexedPropertySlotMethod,
            DefineOwnIndexedPropertySlot: $class::DefineOwnIndexedPropertySlotMethod,
            GetPropertyNames: $class::GetPropertyNamesMethod,
            GetOwnPropertyNames: $class::GetOwnPropertyNamesMethod,
            DefaultValue: $class::DefaultValueMethod,
        }
    };
}
