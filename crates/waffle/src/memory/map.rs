use std::{
    collections::hash_map::RandomState,
    hash::{BuildHasher, Hash, Hasher},
};

use crate::{gc_frame, vm::VM};

use super::{
    gcwrapper::{Array, Gc},
    Finalize, Managed, Trace,
};

pub struct Map<K: PartialEq + Hash + Trace, V: Trace, S = RandomState> {
    len: usize,
    storage: Gc<Array<Option<Gc<Entry<K, V>>>>>,
    s: S,
}

unsafe impl<K: PartialEq + Hash + Trace, V: Trace, S> Trace for Map<K, V, S> {
    fn trace(&mut self, visitor: &mut dyn super::Visitor) {
        self.storage.trace(visitor);
    }
}

unsafe impl<K: PartialEq + Hash + Trace, V: Trace, S> Finalize for Map<K, V, S> {}
impl<K: PartialEq + Hash + Trace, V: Trace, S> Managed for Map<K, V, S> {}

impl<K: 'static + PartialEq + Hash + Trace, V: 'static + Trace, S: 'static> Map<K, V, S> {
    pub fn new(vm: &mut VM, capacity: usize, hash_builder: S) -> Gc<Self> {
        let storage = vm.gc().array(capacity, None);
        vm.gc().fixed(Self {
            len: 0,
            storage,
            s: hash_builder,
        })
    }
}

impl<K: 'static + PartialEq + Hash + Trace, V: 'static + Trace, S: 'static> Gc<Map<K, V, S>> {
    pub fn for_each(&self, mut f: impl FnMut(&K,&V) -> bool) {
        'l: for i in 0..self.storage.len() {
            let mut node = &self.storage[i];
            while let Some(ref entry) = node {
                if !f(&entry.key,&entry.value) {
                    break 'l;
                }
                node = &entry.next;
            }
        }
    } 
    
    fn resize(&mut self, vm: &mut VM) {
        let size = self.storage.len() * 2;

        let tmp = vm.gc().array(size, None);
        let prev = self.storage;
        self.storage = tmp;
        let mut node;
        let mut next;

        for i in 0..prev.len() {
            node = prev[i];
            while let Some(mut n) = node {
                next = n.next;
                let pos = n.hash % size as u64;
                n.next = self.storage[pos as usize];
                self.storage[pos as usize] = Some(n);
                node = next;
            }
        }
        vm.gc().write_barrier(*self);
    }

    pub fn insert(&mut self, vm: &mut VM, key: K, value: V) -> bool
    where
        S: BuildHasher,
    {
        let hash = hash(&self.s, &key);
        let position = (hash % self.storage.len() as u64) as usize;
        let mut node = self.storage[position as usize];
        while let Some(mut n) = node {
            if n.hash == hash && n.key == key {
                n.value = value;
                vm.gc().write_barrier(*self);
                return false;
            }
            node = n.next;
        }

        self.insert_slow(vm, key, value, hash, position);
        true
    }

    fn insert_slow(&mut self, vm: &mut VM, key: K, value: V, hash: u64, mut pos: usize) {
        gc_frame!(vm.gc().roots() => key = Some(key), value = Some(value));

        if self.len >= (self.storage.len() as f64 * 0.75) as usize {
            self.resize(vm);
            pos = (hash % self.storage.len() as u64) as usize;
        }

        let node = vm.gc().fixed(Entry {
            key: key.take().unwrap(),
            value: value.take().unwrap(),
            hash,
            next: self.storage[pos],
        });
        self.storage[pos] = Some(node);
        self.len += 1;
        vm.gc().write_barrier(*self);
    }

    pub fn get(&self, key: &K) -> Option<&V> where S: BuildHasher {
        let hash = hash(&self.s, key);
        let position = (hash % self.storage.len() as u64) as usize;

        let mut node = &self.storage[position];
        while let Some(entry) = node {
            if &entry.key == key && entry.hash == hash {
                return Some(&entry.value);
            }
            node = &entry.next;
        }
        None 
    }

    pub fn get_mut(&mut self, key: &K) -> Option<&mut V> where S: BuildHasher {
        let hash = hash(&self.s, key);
        let position = (hash % self.storage.len() as u64) as usize;

        let mut node = &mut self.storage[position];
        while let Some(entry) = node {
            if &entry.key == key && entry.hash == hash {
                return Some(&mut entry.value);
            }
            node = &mut entry.next;
        }
        None 
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn capacity(&self) -> usize {
        self.storage.len()
    }

    pub fn remove(&mut self, key: &K) -> Option<EntryWrite<K,V>> where S: BuildHasher {
        let hash = hash(&self.s,key);
        let position = (hash % self.storage.len() as u64) as usize;
        let mut node = self.storage[position];
        let mut prevnode: Option<Gc<Entry<K, V>>> = None;
        while let Some(entry) = node {
            if entry.hash == hash && &entry.key == key {
                if let Some(mut prev) = prevnode {
                    prev.next = entry.next;
                } else {
                    self.storage[position] = entry.next;
                }
                self.len -= 1;
                return Some(EntryWrite {
                    entry
                })
            }
            prevnode = node;
            node = entry.next;
        }

        None
    }

    pub fn copy<S2>(&mut self, vm: &mut VM, from: Gc<Map<K,V, S2>>) where K: Clone, V: Clone, S: BuildHasher {
        gc_frame!(vm.gc().roots() => from = from);

        for i in 0..from.storage.len() {
            gc_frame!(vm.gc().roots() => node = from.storage[i]);
            while let Some(ref entry) = *node {
                self.insert(vm, entry.key.clone(), entry.value.clone());
                node.set(entry.next);
            }
        }
    }
}

pub struct EntryRead<K: Trace + PartialEq + Hash, V: Trace> {
    entry: Gc<Entry<K,V>>
}

impl<K: Trace + PartialEq + Hash, V: Trace> EntryRead<K,V> {
    pub fn key(&self) -> &K {
        &self.entry.key
    }

    pub fn value(&self) -> &V {
        &self.entry.value
    }

    pub fn hash(&self) -> u64 {
        self.entry.hash
    }
}

pub struct EntryWrite<K: Trace + PartialEq + Hash, V: Trace> {
    entry: Gc<Entry<K,V>>
}

impl<K: Trace + PartialEq + Hash, V: Trace> EntryWrite<K,V> {
    pub fn key(&self) -> &K {
        &self.entry.key
    }

    pub fn value(&self) -> &V {
        &self.entry.value
    }

    pub fn set_value(&mut self,vm: &mut VM, val: V) -> V {
        let prev = std::mem::replace(&mut self.entry.value, val);
        vm.gc().write_barrier(self.entry);
        prev 
    } 

    pub fn hash(&self) -> u64 {
        self.entry.hash
    }
}

fn hash<K: Hash, S: BuildHasher>(b: &S, val: &K) -> u64 {
    let mut hasher = b.build_hasher();
    val.hash(&mut hasher);
    hasher.finish()
}

struct Entry<K: PartialEq + Hash + Trace, V: Trace> {
    key: K,
    value: V,
    hash: u64,
    next: Option<Gc<Self>>,
}

unsafe impl<K: PartialEq + Hash + Trace, V: Trace> Trace for Entry<K, V> {
    fn trace(&mut self, visitor: &mut dyn super::Visitor) {
        self.key.trace(visitor);
        self.value.trace(visitor);
        self.next.trace(visitor);
    }
}
unsafe impl<K: PartialEq + Hash + Trace, V: Trace> Finalize for Entry<K, V> {}
impl<K: PartialEq + Hash + Trace, V: Trace> Managed for Entry<K, V> {}


#[cfg(test)]
mod tests {
    use crate::vm::initialize_symbol_table;

    use super::*;
    #[test]
    fn test_map() {
        initialize_symbol_table();
        let mut vm = VM::new(None);

        let map = Map::<i32,_>::new(vm, 4,RandomState::new());
        gc_frame!(vm.gc().roots() => map = map);
        for i in 0..1024 {
            let str = vm.gc().str(i.to_string());
            map.insert(&mut vm, i,str);
            vm.gc().write_barrier(*map);
        }
        println!("map init");
        vm.gc().collect(1,&mut []);

        for i in 0..100 {
            let val = map.get(&i).unwrap();
            assert_eq!(val.to_string(), i.to_string());
        }
    }
}