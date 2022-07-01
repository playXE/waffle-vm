use super::attributes::*;
use crate::{
    gc_frame,
    memory::{
        gcwrapper::{Array, Gc, Nullable, WeakRef},
        map::Map,
        Finalize, Managed, Trace, Visitor,
    },
    object::{Object, OBJECT_CLASS},
    value::Value,
    vm::Symbol,
    vm::{DUMMY_SYMBOL, VM},
};
use std::{collections::hash_map::RandomState, sync::atomic::AtomicU32};

type Table = Gc<Map<TransitionKey, WeakRef<Structure>>>;

#[derive(Copy, Clone)]
pub struct MapEntry {
    pub offset: u32,
    pub attrs: AttrSafe,
}

unsafe impl Trace for MapEntry {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        let _ = visitor;
    }
}

impl MapEntry {
    pub fn not_found() -> Self {
        Self {
            offset: u32::MAX,
            attrs: AttrSafe::not_found(),
        }
    }

    pub fn is_not_found(&self) -> bool {
        self.attrs.is_not_found()
    }
}

#[derive(Clone, Copy)]
pub enum Transition {
    None,
    Table(Option<Table>),
    Pair(TransitionKey, WeakRef<Structure>),
}
pub struct TransitionsTable {
    pub var: Transition,
    pub enabled: bool,
    pub unique: bool,
    pub indexed: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TransitionKey {
    pub name: Symbol,
    pub attrs: u32,
}
unsafe impl Trace for TransitionKey {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.name.trace(visitor);
    }
}

impl TransitionsTable {
    pub fn new(enabled: bool, indexed: bool) -> Self {
        Self {
            var: Transition::None,
            unique: false,
            indexed,
            enabled,
        }
    }
    pub fn set_indexed(&mut self, indexed: bool) {
        self.indexed = indexed;
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    pub fn is_enabled_unique_transition(&self) -> bool {
        self.unique
    }

    pub fn enable_unique_transition(&mut self) {
        self.unique = true;
    }

    pub fn insert(&mut self, vm: &mut VM, name: Symbol, attrs: AttrSafe, map: Gc<Structure>) {
        gc_frame!(vm.gc().roots() => key = TransitionKey { name, attrs: attrs.raw() });
        match self.var {
            Transition::Pair(x, y) => {
                let mut table = Map::new(vm, 4, RandomState::new());
                table.insert(vm, x, y);
                self.var = Transition::Table(Some(table));
            }
            Transition::Table(Some(ref mut table)) => {
                let weak = vm.gc().weak(map);
                table.insert(vm, *key, weak);
            }
            _ => {
                self.var = Transition::Pair(*key, vm.gc().weak(map));
            }
        }
    }

    pub fn find(&self, name: Symbol, attrs: AttrSafe) -> Option<Gc<Structure>> {
        let key = TransitionKey {
            name,
            attrs: attrs.raw(),
        };
        if let Transition::Table(ref table) = &self.var {
            return table.as_ref().unwrap().get(&key).and_then(|x| x.upgrade());
        } else if let Transition::Pair(key_, map) = &self.var {
            if key == *key_ {
                return map.upgrade();
            }
        }
        None
    }

    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
    pub fn is_indexed(&self) -> bool {
        self.indexed
    }
}

pub type TargetTable = Gc<Map<Symbol, MapEntry>>;

#[derive(Clone)]
pub struct DeletedEntryHolder {
    pub entry: Option<Gc<DeletedEntry>>,
    pub size: u32,
}

impl DeletedEntryHolder {
    pub fn push(&mut self, ctx: &mut VM, offset: u32) {
        let entry = ctx.gc().fixed(DeletedEntry {
            prev: self.entry,
            offset,
        });
        self.size += 1;
        self.entry = Some(entry);
    }
    pub fn pop(&mut self) -> u32 {
        unsafe {
            let res = self.entry.as_ref().unwrap_unchecked().offset;
            self.entry = self.entry.as_ref().unwrap_unchecked().prev;
            self.size -= 1;
            res
        }
    }

    pub fn size(&self) -> u32 {
        self.size
    }

    pub fn empty(&self) -> bool {
        self.size == 0
    }
}

pub struct DeletedEntry {
    pub prev: Option<Gc<DeletedEntry>>,
    pub offset: u32,
}

unsafe impl Trace for DeletedEntry {
    fn trace(&mut self, tracer: &mut dyn Visitor) {
        self.prev.trace(tracer)
    }
}
unsafe impl Trace for DeletedEntryHolder {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.entry.trace(visitor);
    }
}

unsafe impl Finalize for DeletedEntry {}
unsafe impl Finalize for DeletedEntryHolder {}

impl Managed for DeletedEntryHolder {}
impl Managed for DeletedEntry {}

static STRUCTURE_ID: AtomicU32 = AtomicU32::new(0);

fn structure_id() -> u32 {
    STRUCTURE_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed)
}

pub struct Structure {
    pub(crate) id: u32,
    pub(crate) transitions: TransitionsTable,
    pub(crate) table: Option<TargetTable>,
    pub(crate) deleted: DeletedEntryHolder,
    pub(crate) added: (Symbol, MapEntry),
    pub(crate) previous: Nullable<Self>,
    pub(crate) prototype: Nullable<Object>,
    pub(crate) calculated_size: u32,
    pub(crate) transit_count: u32,
    pub(crate) has_been_flattened_before: bool,
    pub(crate) instance_structure: Option<Gc<Structure>>,
    pub(crate) cached_prototype_chain: Nullable<StructureChain>,
    pub(crate) instantiate: Option<Instantiate>,
}

pub type StructureID = u32;

unsafe impl Trace for Structure {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        match self.transitions.var {
            Transition::Pair(ref mut x, ref mut y) => {
                x.trace(visitor);
                y.trace(visitor);
            }
            Transition::Table(ref mut table) => table.trace(visitor),
            Transition::None => (),
        }

        self.table.trace(visitor);
        self.deleted.trace(visitor);
        self.previous.trace(visitor);
        self.prototype.trace(visitor);
        self.instance_structure.trace(visitor);
    }
}
unsafe impl Finalize for Structure {}
impl Managed for Structure {}

impl Structure {
    pub fn is_shaped(&self) -> bool {
        // we can use this map id as shape or not
        !self.is_unique() || self.transitions.is_enabled()
    }

    pub fn prototype(&self) -> &Nullable<Object> {
        &self.prototype
    }
    pub fn is_unique(&self) -> bool {
        !self.transitions.is_enabled()
    }
    pub unsafe fn prototype_mut(&mut self) -> &mut Nullable<Object> {
        &mut self.prototype
    }
    pub fn has_mono_proto(&self) -> bool {
        self.prototype.is_not_null()
    }

    pub fn has_poly_proto(&self) -> bool {
        self.prototype.is_null()
    }

    pub fn id(&self) -> StructureID {
        self.id
    }
    /// Set structure ID.
    ///
    /// # Safety
    ///
    /// It is unsafe to change structure id since it may change program behaviour.
    pub unsafe fn set_id(&mut self, id: StructureID) {
        self.id = id;
    }

    pub fn is_adding_map(&self) -> bool {
        self.added.0 != DUMMY_SYMBOL
    }

    pub fn has_table(&self) -> bool {
        self.table.is_some()
    }
}

impl Gc<Structure> {
    pub fn prototype_chain(&mut self, vm: &mut VM, base: Gc<Object>) -> Gc<StructureChain> {
        if !Structure::is_valid(self.cached_prototype_chain, base) {
            let prototype = *base.prototype();
            self.cached_prototype_chain = StructureChain::create(
                vm,
                if prototype.is_null() {
                    Nullable::NULL
                } else {
                    prototype
                },
            )
            .nullable();
            vm.gc().write_barrier(*self);
        }
        self.cached_prototype_chain.as_gc()
    }

    /// Returns structure for a new instance of some class. Structure is created lazily
    pub fn constructor_structure(
        &mut self,
        vm: &mut VM,
        mut prototype: Nullable<Object>,
    ) -> Gc<Structure> {
        self.instance_structure.unwrap_or_else(|| {
            gc_frame!(vm.gc().roots() => prototype: Nullable<Object>);
            let proto = if prototype.is_not_null() {
                prototype.as_gc()
            } else {
                vm.global.object_prototype.as_gc()
            };
            self.instance_structure = Some(Structure::new_indexed(vm, Some(proto), false));
            self.instance_structure.unwrap()
        })
    }

    pub fn allocate_table(&mut self, vm: &mut VM) {
        gc_frame!(vm.gc().roots() => stack = Vec::with_capacity(4), current = self.previous);

        if self.is_adding_map() {
            stack.push(*self);
        }

        loop {
            if current.is_not_null() {
                if current.has_table() {
                    self.table = Some(Map::new(
                        vm,
                        current.table.as_ref().unwrap().len(),
                        RandomState::new(),
                    ));
                    vm.gc().write_barrier(*self);
                    self.table
                        .as_mut()
                        .unwrap()
                        .copy(vm, current.table.unwrap());
                    break;
                } else if current.is_adding_map() {
                    stack.push(current.as_gc());
                }
                current.set(current.previous);
            } else {
                self.table = Some(Map::new(vm, 4, RandomState::new()));
                vm.gc().write_barrier(*self);
                break;
            }
        }

        let table = self.table.as_mut().unwrap();
        for it in stack.iter() {
            table.insert(vm, it.added.0, it.added.1);
        }
        // = Nullable::NULL;
    }

    pub fn get(&mut self, vm: &mut VM, name: Symbol) -> MapEntry {
        if !self.has_table() {
            if self.previous.is_null() {
                return MapEntry::not_found();
            }
            if self.is_adding_map() && self.added.0 == name {
                return self.added.1;
            }

            self.allocate_table(vm);
        }

        let it = self.table.as_ref().unwrap().get(&name);

        it.copied().unwrap_or_else(|| MapEntry::not_found())
    }

    pub fn add_property_transition(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        attributes: AttrSafe,
        offset: &mut u32,
    ) -> Gc<Structure> {
        let mut entry = MapEntry {
            offset: 0,
            attrs: attributes,
        };

        if self.is_unique() {
            if !self.has_table() {
                self.allocate_table(ctx);
            }

            let mut map = if self.transitions.is_enabled_unique_transition() {
                Structure::new_unique(ctx, *self)
            } else {
                *self
            };
            if !map.deleted.empty() {
                entry.offset = map.deleted.pop();
            } else {
                entry.offset = self.get_slots_size() as _;
            }
            unsafe {
                map.table
                    .as_mut()
                    .unwrap_unchecked()
                    .insert(ctx, name, entry);
            }
            *offset = entry.offset;
            return map;
        }

        // existing transition check
        if let Some(map) = self.transitions.find(name, attributes) {
            *offset = map.added.1.offset;

            return map;
        }
        if self.transit_count > 64 {
            // stop transition
            let mut map = Structure::new_unique(ctx, *self);
            // go to above unique path
            return map.add_property_transition(ctx, name, attributes, offset);
        }
        let mut map = Structure::new(ctx, *self);

        if !map.deleted.empty() {
            let slot = map.deleted.pop();
            map.added = (
                name,
                MapEntry {
                    offset: slot,
                    attrs: attributes,
                },
            );
            map.calculated_size = self.get_slots_size() as _;
        } else {
            map.added = (
                name,
                MapEntry {
                    offset: self.get_slots_size() as _,
                    attrs: attributes,
                },
            );
            map.calculated_size = self.get_slots_size() as u32 + 1;
        }
        map.transit_count += 1;
        self.transitions.insert(ctx, name, attributes, map);
        ctx.gc().write_barrier(*self);
        *offset = map.added.1.offset;
        assert!(map.get_slots_size() as u32 > map.added.1.offset);

        map
    }

    pub fn delete_property_transition(&mut self, ctx: &mut VM, name: Symbol) -> Gc<Structure> {
        let mut map = Structure::new_unique(ctx, *self);
        if !map.has_table() {
            map.allocate_table(ctx);
        }
        map.delete(ctx, name);
        map
    }

    pub fn delete(&mut self, ctx: &mut VM, name: Symbol) {
        let it = self.table.as_mut().unwrap().remove(&name).unwrap();
        self.deleted.push(ctx, it.value().offset);
    }

    pub fn change_indexed_transition(&mut self, ctx: &mut VM) -> Gc<Structure> {
        if self.is_unique() {
            let mut map = if self.transitions.is_enabled_unique_transition() {
                Structure::new_unique(ctx, *self)
            } else {
                *self
            };
            map.transitions.set_indexed(true);
            map
        } else {
            Structure::new_unique(ctx, *self).change_indexed_transition(ctx)
        }
    }

    pub fn change_prototype_transition(
        &mut self,
        ctx: &mut VM,
        prototype: Option<Gc<Object>>,
    ) -> Gc<Structure> {
        if self.is_unique() {
            let mut map = if self.transitions.is_enabled_unique_transition() {
                Structure::new_unique(ctx, *self)
            } else {
                *self
            };
            map.prototype = prototype.into();
            map
        } else {
            let mut map = Structure::new_unique(ctx, *self);
            map.change_prototype_transition(ctx, prototype)
        }
    }

    pub fn change_extensible_transition(&mut self, ctx: &mut VM) -> Gc<Structure> {
        Structure::new_unique(ctx, *self)
    }
    pub fn change_attributes_transition(
        &mut self,
        ctx: &mut VM,
        name: Symbol,
        attributes: AttrSafe,
    ) -> Gc<Structure> {
        let mut map = Structure::new_unique(ctx, *self);
        if !map.has_table() {
            map.allocate_table(ctx);
        }
        map.change_attributes(name, attributes);
        map
    }
    pub fn change_attributes(&mut self, name: Symbol, attributes: AttrSafe) {
        let it = self.table.as_mut().unwrap().get_mut(&name).unwrap();
        it.attrs = attributes;
    }

    pub fn get_own_property_names(
        &mut self,
        ctx: &mut VM,
        include: bool,
        mut collector: impl FnMut(Symbol, u32),
    ) {
        if self.allocate_table_if_needed(ctx) {
            /*for entry in self.table.as_ref().unwrap().iter() {
                /*if entry.0.is_private() {
                    continue;
                }
                if entry.0.is_public() {
                    continue;
                }*/
                if include || entry.1.attrs.is_enumerable() {
                    collector(*entry.0, entry.1.offset);
                }
            }*/
            self.table.as_ref().unwrap().for_each(|k, v| {
                if include || v.attrs.is_enumerable() {
                    collector(*k, v.offset);
                }
                true
            });
        }
    }

    pub fn allocate_table_if_needed(&mut self, vm: &mut VM) -> bool {
        if !self.has_table() {
            if self.previous.is_null() {
                return false;
            }
            self.allocate_table(vm);
        }
        true
    }

    pub fn storage_capacity(&self) -> usize {
        let sz = self.get_slots_size();
        if sz < 8 {
            8
        } else {
            fn clp2(number: usize) -> usize {
                let x = number - 1;
                let x = x | (x >> 1);
                let x = x | (x >> 2);
                let x = x | (x >> 4);
                let x = x | (x >> 8);
                let x = x | (x >> 16);
                x + 1
            }
            clp2(sz)
        }
    }
    pub fn change_prototype_with_no_transition(&mut self, prototype: Gc<Object>) -> Self {
        self.prototype = prototype.nullable();
        *self
    }

    pub fn flatten(&mut self) {
        if self.is_unique() {
            self.transitions.enable_unique_transition();
            self.has_been_flattened_before = true;
        }
    }

    pub fn get_slots_size(&self) -> usize {
        if let Some(table) = self.table.as_ref() {
            table.len() + self.deleted.size as usize
        } else {
            self.calculated_size as _
        }
    }

    pub fn is_indexed(&self) -> bool {
        self.transitions.is_indexed()
    }

    pub fn is_unique(&self) -> bool {
        !self.transitions.is_enabled()
    }

    pub fn is_shaped(&self) -> bool {
        // we can use this map id as shape or not
        !self.is_unique() || self.transitions.is_enabled()
    }
}

impl Structure {
    pub fn ctor(vm: &mut VM, previous: Gc<Self>, unique: bool) -> Gc<Self> {
        let transitions = TransitionsTable::new(!unique, previous.transitions.is_indexed());
        vm.gc().fixed(Structure {
            id: structure_id(),
            transitions,
            table: if unique && previous.is_unique() {
                previous.table
            } else {
                None
            },
            deleted: previous.deleted.clone(),
            added: (DUMMY_SYMBOL, MapEntry::not_found()),
            previous: previous.into(),
            prototype: previous.prototype,
            calculated_size: previous.get_slots_size() as _,
            transit_count: 0,
            has_been_flattened_before: previous.has_been_flattened_before,
            instance_structure: previous.instance_structure,
            cached_prototype_chain: Nullable::NULL,
            instantiate: previous.instantiate,
        })
    }
    #[allow(dead_code)]
    fn ctor2(
        ctx: &mut VM,
        table: Option<TargetTable>,
        prototype: Option<Gc<Object>>,
        unique: bool,
        indexed: bool,
    ) -> Gc<Self> {
        let mut this = Self::ctor1(ctx, prototype, unique, indexed);
        this.table = table;
        this.calculated_size = this.get_slots_size() as _;
        this
    }

    fn ctor1(ctx: &mut VM, prototype: Option<Gc<Object>>, unique: bool, indexed: bool) -> Gc<Self> {
        ctx.gc().fixed(Structure {
            prototype: prototype.into(),
            previous: Nullable::NULL,
            table: None,
            has_been_flattened_before: false,
            transitions: TransitionsTable::new(!unique, indexed),
            deleted: DeletedEntryHolder {
                entry: None,
                size: 0,
            },
            added: (DUMMY_SYMBOL, MapEntry::not_found()),
            id: structure_id(),
            calculated_size: 0,
            transit_count: 0,
            instance_structure: None,
            cached_prototype_chain: Nullable::NULL,
            instantiate: None,
        })
    }

    fn ctor3(ctx: &mut VM, it: &[(Symbol, MapEntry)]) -> Gc<Self> {
        let mut table = Map::new(ctx, it.len(), RandomState::new());

        for (key, entry) in it.iter().copied() {
            table.insert(ctx, key, entry);
        }
        let mut this = ctx.gc().fixed(Structure {
            prototype: Nullable::NULL,
            previous: Nullable::NULL,
            has_been_flattened_before: false,
            table: Some(table),
            transitions: TransitionsTable::new(true, false),
            deleted: DeletedEntryHolder {
                entry: None,
                size: 0,
            },
            added: (DUMMY_SYMBOL, MapEntry::not_found()),
            id: 0,
            calculated_size: 0,
            transit_count: 0,
            instance_structure: None,
            cached_prototype_chain: Nullable::NULL,
            instantiate: None,
        });
        this.calculated_size = this.get_slots_size() as _;
        this
    }

    pub fn new(ctx: &mut VM, previous: Gc<Structure>) -> Gc<Structure> {
        Self::ctor(ctx, previous, false)
    }

    pub fn new_unique(ctx: &mut VM, previous: Gc<Structure>) -> Gc<Structure> {
        Self::ctor(ctx, previous, true)
    }
    pub fn new_unique_with_proto(
        ctx: &mut VM,
        proto: Option<Gc<Object>>,
        indexed: bool,
    ) -> Gc<Structure> {
        Self::ctor2(ctx, None, proto, true, indexed)
    }
    pub fn new_(ctx: &mut VM, it: &[(Symbol, MapEntry)]) -> Gc<Structure> {
        Self::ctor3(ctx, it)
    }

    pub fn new_indexed(ctx: &mut VM, prototype: Option<Gc<Object>>, indexed: bool) -> Gc<Self> {
        Self::ctor1(ctx, prototype, false, indexed)
    }
    pub fn new_unique_indexed(
        ctx: &mut VM,
        prototype: Option<Gc<Object>>,
        indexed: bool,
    ) -> Gc<Self> {
        Self::ctor1(ctx, prototype, true, indexed)
    }

    pub fn new_from_point(ctx: &mut VM, map: Gc<Structure>) -> Gc<Self> {
        if map.is_unique() {
            return Self::new_unique(ctx, map);
        }
        map
    }

    fn is_valid(cached_prototype_chain: Nullable<StructureChain>, base: Gc<Object>) -> bool {
        if cached_prototype_chain.is_null() {
            return false;
        }

        let mut prototype = *base.prototype();
        let mut cached_structure = cached_prototype_chain.at(0);
        let mut i = 0;
        while cached_structure.is_not_null() && prototype.is_not_null() {
            if !Nullable::ptr_eq(prototype.structure().nullable(), cached_structure) {
                return false;
            }
            i += 1;
            cached_structure = cached_prototype_chain.at(i);
            prototype = prototype.structure().prototype;
        }
        prototype.is_null() && cached_structure.is_null()
    }

    pub fn instantiate(&self) -> Instantiate {
        self.instantiate.unwrap_or_else(|| default_instantiate)
    }

    pub fn set_instantiate(&mut self, f: Instantiate) {
        self.instantiate = Some(f);
    }
}

pub struct StructureChain {
    pub vector: Gc<Array<Nullable<Structure>>>,
}

impl StructureChain {
    pub fn head(&self) -> Nullable<Structure> {
        // structure chain is guaranteed to not be empty
        *self.vector.first().unwrap()
    }

    pub fn at(&self, i: usize) -> Nullable<Structure> {
        self.vector[i]
    }

    pub fn create(vm: &mut VM, head: Nullable<Object>) -> Gc<Self> {
        let mut size = 0;
        let mut current = head;
        while current.is_not_null() {
            size += 1;
            current = current.structure().prototype;
        }
        // sentinel null
        size += 1;

        gc_frame!(vm.gc().roots() => head = head);
        let vector = vm.gc().array(size, Nullable::NULL);
        gc_frame!(vm.gc().roots() => this = vm.gc().fixed(Self { vector }));
        Self::finish_creation(&mut this, vm, &mut head);
        *this
    }

    fn finish_creation(this: &mut Gc<Self>, vm: &mut VM, head: &mut Nullable<Object>) {
        gc_frame!(vm.gc().roots() => current = *head);
        let mut i = 0;
        while current.is_not_null() {
            this.vector[i] = current.structure().nullable();
            i += 1;
            current.set(current.structure().prototype);
            vm.gc().write_barrier(*this);
        }
    }
}

unsafe impl Trace for StructureChain {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.vector.trace(visitor)
    }
}

unsafe impl Finalize for StructureChain {}

impl Managed for StructureChain {}

/// Function to instantiate object from prototype
pub type Instantiate = fn(&mut VM, &Gc<Structure>) -> Gc<Object>;

pub fn default_instantiate(vm: &mut VM, structure: &Gc<Structure>) -> Gc<Object> {
    Object::new(vm, structure, &OBJECT_CLASS, Value::Null)
}
