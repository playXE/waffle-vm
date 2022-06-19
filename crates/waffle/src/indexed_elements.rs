use std::collections::hash_map::RandomState;

use crate::{memory::{array::ArrayStorage, gcwrapper::Gc, map::Map, Visitor, Trace}, object::property_descriptor::StoredSlot, vm::VM, attributes::object_data};
use super::value::*;
const FLAG_DENSE: u8 = 1;
const FLAG_WRITABLE: u8 = 2;
/// 256*n
pub const MAX_VECTOR_SIZE: usize = 1024 << 6;

pub type SparseArrayMap = Map<u32, StoredSlot>;
pub type DenseArrayMap = ArrayStorage;

pub struct IndexedElements {
    pub(crate) map: Option<Gc<SparseArrayMap>>,
    pub(crate) vector: Gc<DenseArrayMap>,
    pub(crate) length: u32,
    pub(crate) flags: u32,
}

impl IndexedElements {
    #[allow(clippy::explicit_counter_loop)]
    pub fn make_sparse(&mut self, vm: &mut VM) {
        self.flags &= !(FLAG_DENSE as u32);
        let mut sparse = self.ensure_map(vm, 8);
        let mut index = 0;
        for i in 0..self.vector.size() {
            if !self.vector.at(i).is_empty() {
                sparse.insert(vm,
                    index,
                    StoredSlot::new_raw(*self.vector.at(i), object_data()),
                );
            }
            index += 1;
        }
        for i in 0..self.vector.size() {
            *self.vector.at_mut(i) = Value::encode_empty_value();
        }
    }

    pub fn make_dense(&mut self) {
        self.flags |= FLAG_DENSE as u32;
        self.map = None;
    }

    pub fn ensure_map(&mut self, vm: &mut VM, capacity: usize) -> Gc<SparseArrayMap> {
        match self.map.as_ref() {
            Some(map) => *map,
            None => {
                let map = Map::new(vm,capacity,RandomState::new());
                self.map = Some(map);
                map
            }
        }
    }

    pub fn length(&self) -> u32 {
        self.length
    }

    pub fn set_length(&mut self, len: u32) {
        self.length = len;
    }

    pub fn dense(&self) -> bool {
        (self.flags & FLAG_DENSE as u32) != 0
    }

    pub fn sparse(&self) -> bool {
        !self.dense()
    }

    pub fn writable(&self) -> bool {
        (self.flags & FLAG_WRITABLE as u32) != 0
    }

    pub fn make_readonly(&mut self) {
        self.flags &= !(FLAG_WRITABLE as u32);
    }

    pub fn new(vm: &mut VM) -> Self {
        Self {
            length: 0,
            flags: FLAG_DENSE as u32 | FLAG_WRITABLE as u32,
            vector: ArrayStorage::new(vm.gc(), 0),
            map: None,
        }
    }
}

unsafe impl Trace for IndexedElements {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.vector.trace(visitor);
        self.map.trace(visitor);
    }
}