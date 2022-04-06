use std::{
    hash::{Hash, Hasher},
    ops::Index,
};

use fxhash::FxHasher64;
use memoffset::offset_of;

use crate::{
    gc_frame,
    memory::{
        gcwrapper::{Array, Gc, Nullable, Str},
        Allocation, Finalize, Object, Trace, Visitor,
    },
    vm::VM,
};

#[cfg(feature = "small-float")]
pub type Float = f32;
#[cfg(not(feature = "small-float"))]
pub type Float = f64;

#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(Gc<Str>),
    Array(Gc<Array<Value>>),
    Abstract(Gc<dyn Object>),
    Module(Gc<Module>),
    Function(Gc<Function>),
    Primitive(Gc<Function>),
    Object(Gc<Obj>),
    Table(Gc<Table>),
}

impl Value {
    pub fn int(self) -> i64 {
        match self {
            Self::Int(x) => x,
            _ => panic!("type error: integer expected"),
        }
    }

    pub fn to_int(self) -> Option<i64> {
        match self {
            Self::Int(x) => Some(x),
            Self::Float(x) => Some(x as i64),
            _ => None,
        }
    }
    pub fn float(self) -> Float {
        match self {
            Self::Float(x) => x,
            _ => panic!("type error: float expected"),
        }
    }
    pub fn str(self) -> Gc<Str> {
        match self {
            Self::Str(x) => x,
            _ => panic!("type error: string expected"),
        }
    }

    pub fn array(self) -> Gc<Array<Value>> {
        match self {
            Self::Array(x) => x,
            _ => panic!("type error: array expected"),
        }
    }
    pub fn module(self) -> Gc<Module> {
        match self {
            Self::Module(x) => x,
            _ => panic!("type error: module expected"),
        }
    }

    pub fn bool(self) -> bool {
        match self {
            Self::Bool(x) => x,
            Self::Null => false,
            Self::Int(x) => x != 0,
            _ => true,
        }
    }
}

unsafe impl Trace for Value {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        match self {
            Self::Str(x) => x.trace(vis),
            Self::Array(x) => x.trace(vis),
            Self::Abstract(x) => x.trace(vis),
            Self::Table(x) => x.trace(vis),
            Self::Function(x) | Self::Primitive(x) => x.trace(vis),
            Self::Object(x) => x.trace(vis),

            _ => (),
        }
    }
}

unsafe impl Finalize for Value {}
impl Object for Value {}

pub struct Function {
    pub nargs: u32,
    pub varsize: bool,
    pub env: Value,
    pub addr: usize,
    pub module: Value,
}

unsafe impl Trace for Function {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.env.trace(vis);
        self.module.trace(vis);
    }
}

unsafe impl Finalize for Function {}
impl Object for Function {}

#[repr(C)]
pub struct Module {
    pub name: Value,
    pub globals: Gc<Array<Value>>,
    pub exports: Value,
    pub loader: Value,
    pub code_size: usize,
    pub code: [u8; 0],
}
unsafe impl Allocation for Module {
    const LIGHT_FINALIZER: bool = false;
    const FINALIZE: bool = false;
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Module, code_size);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Module, code);
    const VARSIZE_ITEM_SIZE: usize = 1;
}
unsafe impl Trace for Module {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.name.trace(vis);
        self.globals.trace(vis);
        self.exports.trace(vis);
        self.loader.trace(vis);
    }
}
unsafe impl Finalize for Module {}
impl Object for Module {}

/// key-value pair
pub struct Table {
    count: usize,
    cells: Gc<Array<Nullable<Cell>>>,
}

impl Gc<Table> {
    fn resize(&mut self, vm: &mut VM) {
        let size = self.cells.len() * 2;
        let prev = self.cells;
        self.cells = vm.gc().array(size, Nullable::NULL);

        let mut node;
        let mut next;

        for i in 0..prev.len() {
            node = prev[i];
            while node.is_not_null() {
                next = node.next;
                let pos = node.hash % size as u64;
                node.next = self.cells[pos as usize];
                self.cells[pos as usize] = node;
                node = next;
            }
        }
        vm.gc().write_barrier(*self);
    }

    pub fn insert(&mut self, vm: &mut VM, key: &Value, value: &Value) -> bool {
        let hash = value_hash(vm, key).int() as u64;

        let position = (hash % self.cells.len() as u64) as usize;
        let mut node = self.cells[position];
        while node.is_not_null() {
            if node.hash == hash && value_eq(vm, &node.key, key).bool() {
                node.value = *value;
                vm.gc().write_barrier(*self);
                return false;
            }
            node = node.next;
        }
        self.insert_slow(vm, *key, *value, hash, position);
        true
    }
    fn insert_slow(
        &mut self,
        vm: &mut VM,
        mut key: Value,
        mut value: Value,
        hash: u64,
        mut pos: usize,
    ) {
        gc_frame!(vm.gc().roots() => key,value);
        if self.count >= (self.cells.len() as f64 * 0.75) as usize {
            self.resize(vm);
            pos = (hash % self.cells.len() as u64) as usize;
        }
        unsafe {
            let node = vm.gc().fixed(Cell {
                key: *key.as_ref(),
                value: *value.as_ref(),
                hash,
                next: *self.cells.get_unchecked(pos),
            });
            *self.cells.get_unchecked_mut(pos) = node.nullable();
            vm.gc().write_barrier(*self);
        }
    }
    pub fn remove(&mut self, vm: &mut VM, key: &Value) -> bool {
        let hash = value_hash(vm, key).int() as u64;

        let position = (hash % self.cells.len() as u64) as usize;
        let mut node = self.cells[position];
        let mut prevnode = Nullable::<Cell>::NULL;
        while node.is_not_null() {
            if node.hash == hash && value_eq(vm, &node.key, key).bool() {
                if prevnode.is_not_null() {
                    prevnode.next = node.next;
                } else {
                    self.cells[position] = node.next;
                }
                self.count -= 1;
                return true;
            }
            prevnode = node;
            node = node.next;
        }
        false
    }

    pub fn lookup(&self, vm: &mut VM, key: &Value, found: &mut bool) -> Value {
        let hash = value_hash(vm, key).int() as u64;

        let position = (hash % self.cells.len() as u64) as usize;
        let mut node = self.cells[position];

        while node.is_not_null() {
            if node.hash == hash && value_eq(vm, &node.key, key).bool() {
                *found = true;
                return node.value;
            }

            node = node.next;
        }
        *found = false;
        Value::Null
    }
}

pub fn value_eq(vm: &mut VM, a: &Value, b: &Value) -> Value {
    let res = value_cmp(vm, a, b);
    if res.int() == 0 {
        Value::Bool(true)
    } else {
        Value::Bool(false)
    }
}

unsafe impl Trace for Table {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.cells.trace(vis);
    }
}
unsafe impl Finalize for Table {}
impl Object for Table {}

struct Cell {
    key: Value,
    value: Value,
    hash: u64,
    next: Nullable<Cell>,
}

unsafe impl Trace for Cell {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.key.trace(vis);
        self.value.trace(vis);
        self.next.trace(vis);
    }
}

unsafe impl Finalize for Cell {}
impl Object for Cell {}

pub fn value_hash(vm: &mut VM, value: &Value) -> Value {
    let mut hasher = FxHasher64::default();
    match value {
        Value::Null => {
            0i64.hash(&mut hasher);
        }
        Value::Int(x) => {
            1i64.hash(&mut hasher);
            x.hash(&mut hasher);
        }
        Value::Float(x) => {
            2i64.hash(&mut hasher);
            x.to_bits().hash(&mut hasher);
        }
        Value::Abstract(obj) => {
            3i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Array(obj) => {
            4i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Module(obj) => {
            5i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Function(obj) => {
            6i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Str(x) => {
            7i64.hash(&mut hasher);
            (**x).hash(&mut hasher);
        }
        Value::Table(obj) => {
            8i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Object(obj) => {
            9i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }

        Value::Primitive(obj) => {
            11i64.hash(&mut hasher);
            let identity = vm.gc().identity(*obj);
            identity.hash(&mut hasher);
        }
        Value::Bool(x) => {
            12i64.hash(&mut hasher);
            x.hash(&mut hasher);
        }
    }

    Value::Int(hasher.finish() as _)
}

fn icmp(x: i64, y: i64) -> i64 {
    if x == y {
        0
    } else if x < y {
        -1
    } else {
        1
    }
}

pub const INVALID_CMP: i64 = 0xfe;

fn fcmp(x: Float, y: Float) -> i64 {
    if x != x || y != y {
        INVALID_CMP
    } else {
        if x == y {
            0
        } else if x < y {
            -1
        } else {
            1
        }
    }
}

fn scmp(x: &str, y: &str) -> i64 {
    unsafe {
        let r = libc::memcmp(
            x.as_ptr().cast(),
            y.as_ptr().cast(),
            if x.len() < y.len() { x.len() } else { y.len() },
        );
        if r != 0 {
            r as _
        } else {
            icmp(x.len() as _, y.len() as _)
        }
    }
}

pub fn value_cmp(vm: &mut VM, a: &Value, b: &Value) -> Value {
    Value::Int(match (*a, *b) {
        (Value::Int(x), Value::Int(y)) => icmp(x, y),
        (Value::Float(x), Value::Float(y)) => fcmp(x, y),
        (Value::Int(x), Value::Float(y)) => fcmp(x as f64, y),
        (Value::Float(x), Value::Int(y)) => fcmp(x, y as f64),
        (Value::Str(x), Value::Str(y)) => scmp(&**x, &**y),
        (Value::Bool(x), Value::Bool(y)) => {
            if x == y {
                0
            } else if x {
                1
            } else {
                -1
            }
        }
        (Value::Null, Value::Null) => 0,
        (Value::Primitive(a), Value::Primitive(b)) => vm.identity_cmp(a, b),
        (Value::Function(x), Value::Function(y)) => vm.identity_cmp(x, y),
        (Value::Object(a), Value::Object(b)) => {
            if vm.identity_cmp(a, b) == 0 {
                0
            } else {
                // todo: invoke __eq method
                INVALID_CMP
            }
        }

        (Value::Table(a), Value::Table(b)) => vm.identity_cmp(a, b),
        (Value::Array(a), Value::Array(b)) => vm.identity_cmp(a, b),
        (Value::Abstract(a), Value::Abstract(b)) => vm.identity_cmp(a, b),

        (Value::Module(a), Value::Module(b)) => vm.identity_cmp(a, b),

        _ => INVALID_CMP,
    })
}

pub struct Obj {
    pub(crate) table: Gc<Table>,
    proto: Option<Gc<Self>>,
}

impl Gc<Obj> {
    pub fn field(&self, vm: &mut VM, field: &Value) -> Value {
        let mut o = Some(*self);

        gc_frame!(vm.gc().roots() => o);
        while let Some(mut obj) = o.get() {
            let mut found = false;
            gc_frame!(vm.gc().roots() => obj);
            let f = obj.as_ref().table.lookup(vm, field, &mut found);
            if found {
                return f;
            }

            o.set(obj.as_ref().proto);
        }
        Value::Null
    }
}

unsafe impl Trace for Obj {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.table.trace(vis);
        self.proto.trace(vis);
    }
}
unsafe impl Finalize for Obj {}
impl Object for Obj {}

impl Index<usize> for Module {
    type Output = u8;
    fn index(&self, ix: usize) -> &Self::Output {
        unsafe { &*self.code.as_ptr().add(ix) }
    }
}
