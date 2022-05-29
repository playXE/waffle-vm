use core::fmt;
use std::{
    hash::{Hash, Hasher},
    ops::{Deref, DerefMut, Index, IndexMut},
};

use fxhash::FxHasher64;
use memoffset::offset_of;

use crate::{
    gc_frame,
    memory::{
        gcwrapper::{Array, Gc, Nullable, Str},
        Allocation, Finalize, Object, Trace, Visitor,
    },
    opcode::Op,
    vm::VM,
};

#[cfg(feature = "small-float")]
pub type Float = f32;
#[cfg(not(feature = "small-float"))]
pub type Float = f64;

/*
#[derive(Clone, Copy)]
#[repr(C, u8)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(Float),
    Str(Gc<Str>),
    Symbol(Gc<Sym>),
    Array(Gc<Array<Value>>),
    Abstract(Gc<dyn Object>),
    Module(Gc<Module>),
    Function(Gc<Function>),
    Primitive(Gc<Function>),
    Object(Gc<Obj>),
    Table(Gc<Table>),
}*/
pub use nanbox::*;

pub struct Sym {
    pub(crate) next: Option<Gc<Sym>>,
    pub(crate) name: Gc<Str>,
}

impl Object for Sym {}
unsafe impl Trace for Sym {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.next.trace(visitor);
        self.name.trace(visitor);
    }
}
unsafe impl Finalize for Sym {}
unsafe impl Allocation for Sym {
    const CELL_TYPE: crate::CellType = crate::CellType::Symbol;
}

impl Value {
    pub fn tag(&self) -> usize {
        /*  match self {
            Null => 0,
            Bool(_) => 1,
            Int(_) => 2,
            Float(_) => 3,
            Str(_) => 4,
            Array(_) => 5,
            Abstract(_) => 6,
            Module(_) => 7,
            Function(_) => 8,
            Primitive(_) => 9,
            Object(_) => 10,
            Table(_) => 11,
            Symbol(_) => 12,
        }*/
        if self.is_null() {
            0
        } else if self.is_bool() {
            1
        } else if self.is_int32() {
            2
        } else if self.is_double() {
            3
        } else if self.is_str() {
            4
        } else if self.is_array() {
            5
        } else if self.is_module() {
            6
        } else if self.is_function() {
            7
        } else if self.is_primitive() {
            8
        } else if self.is_obj() {
            9
        } else if self.is_table() {
            10
        } else if self.is_symbol() {
            11
        } else {
            12
        }
    }
    pub fn field(&self, vm: &mut VM, key: &Value) -> Value {
        if let Some(obj) = self.downcast_ref::<Obj>() {
            obj.field(vm, key)
        } else {
            vm.throw_str("not an object");
        }
    }
    pub fn set_field(&mut self, vm: &mut VM, key: &Value, value: &Value) {
        if let Some(mut obj) = self.downcast_ref::<Obj>() {
            gc_frame!(vm.gc().roots() => obj: Gc<Obj>);

            obj.insert(vm, key, value)
        } else {
            vm.throw_str("not an object");
        }
    }
    pub fn int(self) -> i32 {
        self.get_int32()
    }

    pub fn to_int(self) -> Option<i32> {
        if self.is_int32() {
            Some(self.get_int32())
        } else if self.is_double() {
            Some(self.get_double() as i32)
        } else {
            None
        }
    }
    pub fn float(self) -> Float {
        self.get_double()
    }
    pub fn str(self) -> Gc<Str> {
        self.downcast_ref().unwrap()
    }

    pub fn array(self) -> Gc<Array<Value>> {
        self.downcast_ref().unwrap()
    }
    pub fn module(self) -> Nullable<Module> {
        /*self.downcast_ref::<Module>()
        .map(|x| x.nullable())
        .unwrap_or(Nullable::NULL)*/
        if self.is_null() {
            Nullable::NULL
        } else if let Some(m) = self.downcast_ref::<Module>() {
            m.nullable()
        } else {
            if self.is_object() {
                panic!("not an module: {:p} {}", self.get_object(), unsafe {
                    self.get_object().header.as_ref().tid().as_vtable.type_name
                });
            } else {
                unsafe {
                    panic!("not an module: {:x}", self.0.as_int64);
                }
            }
        }
    }

    pub fn prim_or_func(self) -> Gc<Function> {
        self.downcast_ref().expect("function expected")
    }

    pub fn bool(self) -> bool {
        /*match self {
            Self::Bool(x) => x,
            Self::Null => false,
            Self::Int(x) => x != 0,
            _ => true,
        }*/
        if self.is_bool() {
            self.get_bool()
        } else if self.is_null() {
            false
        } else if self.is_int32() {
            self.get_int32() != 0
        } else {
            true
        }
    }

    pub fn is_numeric(self) -> bool {
        self.is_number()
    }
}

unsafe impl Finalize for Value {}
impl Object for Value {}

pub struct Function {
    pub nargs: u32,
    pub varsize: bool,
    pub env: Nullable<Array<Nullable<Upvalue>>>,
    pub addr: usize,
    pub prim: bool,
    pub module: Nullable<Module>,
}

pub struct Upvalue {
    pub next: Nullable<Self>,
    pub closed: bool,
    pub state: UpvalState,
}

pub union UpvalState {
    pub slot: *mut Value,
    pub local: Value,
}

unsafe impl Trace for Upvalue {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.next.trace(visitor);
        if self.closed {
            unsafe {
                self.state.local.trace(visitor);
            }
        }
    }
}

unsafe impl Finalize for Upvalue {}
unsafe impl Allocation for Upvalue {}
impl Object for Upvalue {}
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
    pub globals: Nullable<Array<Value>>,
    pub exports: Value,
    pub loader: Value,
    pub code_size: usize,
    pub code: [Op; 0],
}
unsafe impl Allocation for Module {
    const LIGHT_FINALIZER: bool = false;
    const FINALIZE: bool = false;
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Module, code_size);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Module, code);
    const VARSIZE_ITEM_SIZE: usize = std::mem::size_of::<Op>();
}
unsafe impl Trace for Module {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.name.trace(vis);
        self.globals.trace(vis);
        self.exports.trace(vis);
        self.loader.trace(vis);

        for i in 0..self.code_size {
            if let Op::AccBuiltinResolved(ref mut val) = self[i] {
                val.trace(vis);
            }
        }
    }
}
unsafe impl Finalize for Module {}
impl Object for Module {}

/// key-value pair
pub struct Table {
    pub(crate) count: usize,
    pub(crate) cells: Gc<Array<Nullable<Cell>>>,
}

impl Table {
    pub fn new(vm: &mut VM, cap: usize) -> Gc<Self> {
        let table = vm.gc().array(cap, Nullable::NULL);
        vm.gc().fixed(Self {
            count: 0,
            cells: table,
        })
    }
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
        gc_frame!(vm.gc().roots() => key: Value,value: Value);
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
            self.count += 1;
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
            //     println!("try {:x} {:x} {} {}", hash, node.hash, node.key, key);
            if node.hash == hash && value_eq(vm, &node.key, key).bool() {
                //    println!("found?");
                *found = true;
                return node.value;
            }

            node = node.next;
        }
        *found = false;
        Value::Null
    }

    pub fn copy(&mut self, vm: &mut VM, mut from: Gc<Table>) {
        gc_frame!(vm.gc().roots() => from: Gc<Table>);

        for i in 0..from.cells.len() {
            let mut node = from.cells[i];
            gc_frame!(vm.gc().roots() => node: Nullable<Cell>);
            while node.is_not_null() {
                self.insert(vm, &node.key, &(**node).value);
                *node = node.next;
            }
        }
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

pub struct Cell {
    pub(crate) key: Value,
    pub(crate) value: Value,
    pub(crate) hash: u64,
    pub(crate) next: Nullable<Cell>,
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
    /*match value {
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
        Value::Symbol(x) => {
            13i64.hash(&mut hasher);
            vm.gc().identity(*x).hash(&mut hasher);
        }
    }*/

    if value.is_null() {
        0i64.hash(&mut hasher);
    } else if value.is_int32() {
        1i64.hash(&mut hasher);
        value.get_int32().hash(&mut hasher);
    } else if value.is_double() {
        2i64.hash(&mut hasher);
        value.get_double().to_bits().hash(&mut hasher);
    } else if value.is_bool() {
        3i64.hash(&mut hasher);
        value.get_bool().hash(&mut hasher);
    } else if let Some(str) = value.downcast_ref::<Str>() {
        4i64.hash(&mut hasher);
        (**str).hash(&mut hasher);
    } else {
        let x = value.get_object();
        let id = vm.gc().identity(x);
        5i64.hash(&mut hasher);
        id.hash(&mut hasher);
    }

    Value::Int(hasher.finish() as _)
}

fn icmp(x: i32, y: i32) -> i32 {
    if x == y {
        0
    } else if x < y {
        -1
    } else {
        1
    }
}

pub const INVALID_CMP: i32 = 0xfe;

fn fcmp(x: Float, y: Float) -> i32 {
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

fn scmp(x: &str, y: &str) -> i32 {
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
    /*Value::Int(match (*a, *b) {
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
                let s = vm.intern("__compare");
                let tmp = a.field(vm, &Value::Symbol(s));
                if !matches!(tmp, Value::Function(_) | Value::Primitive(_)) {
                    INVALID_CMP
                } else {
                    let a = vm.call2(tmp, Value::Object(a), Value::Object(b));
                    match a {
                        Value::Int(x) => x,
                        _ => INVALID_CMP,
                    }
                }
            }
        }
        (Value::Object(a), b) => {
            let s = vm.intern("__compare");
            let tmp = a.field(vm, &Value::Symbol(s));
            if !matches!(tmp, Value::Function(_) | Value::Primitive(_)) {
                INVALID_CMP
            } else {
                let a = vm.call2(tmp, Value::Object(a), b);
                match a {
                    Value::Int(x) => x,
                    _ => INVALID_CMP,
                }
            }
        }

        (Value::Table(a), Value::Table(b)) => vm.identity_cmp(a, b),
        (Value::Array(a), Value::Array(b)) => vm.identity_cmp(a, b),
        (Value::Abstract(a), Value::Abstract(b)) => vm.identity_cmp(a, b),
        (Value::Symbol(x), Value::Symbol(y)) => vm.identity_cmp(x, y),
        (Value::Module(a), Value::Module(b)) => vm.identity_cmp(a, b),

        _ => INVALID_CMP,
    })*/
    Value::encode_int32(if a.is_int32() && b.is_int32() {
        icmp(a.get_int32(), b.get_int32())
    } else if a.is_double() && b.is_double() {
        fcmp(a.get_double(), b.get_double())
    } else if a.is_int32() && b.is_double() {
        fcmp(a.get_int32() as _, b.get_double())
    } else if a.is_double() && b.is_int32() {
        fcmp(a.get_double(), b.get_int32() as _)
    } else if let (Some(a), Some(b)) = (a.downcast_ref::<Str>(), b.downcast_ref::<Str>()) {
        scmp(&**a, &**b)
    } else if let (Some(a), Some(b)) = (a.downcast_ref::<Obj>(), b.downcast_ref::<Obj>()) {
        if vm.identity_cmp(a, b) == 0 {
            0
        } else {
            let s = vm.intern("__compare");
            let tmp = a.field(vm, &Value::Symbol(s));
            if !tmp.is_function() {
                INVALID_CMP
            } else {
                let a = vm.call2(tmp, Value::Object(a), Value::Object(b));
                if a.is_int32() {
                    a.get_int32()
                } else {
                    INVALID_CMP
                }
            }
        }
    } else if let Some(a) = a.downcast_ref::<Obj>() {
        let s = vm.intern("__compare");
        let tmp = a.field(vm, &Value::Symbol(s));
        if !tmp.is_function() {
            INVALID_CMP
        } else {
            let a = vm.call2(tmp, Value::Object(a), *b);
            if a.is_int32() {
                a.get_int32()
            } else {
                INVALID_CMP
            }
        }
    } else if a.is_object() && b.is_object() {
        vm.identity_cmp(a.get_object(), b.get_object())
    } else {
        INVALID_CMP
    })
}

pub struct Obj {
    pub(crate) table: Gc<Table>,
    proto: Option<Gc<Self>>,
}

impl Obj {
    pub fn with_proto(vm: &mut VM, mut proto: Gc<Obj>) -> Gc<Obj> {
        let mut this = Nullable::NULL;
        gc_frame!(vm.gc().roots() => proto: Gc<Obj>,this: Nullable<Obj>);
        *this = Obj::with_capacity(vm, proto.table.count).nullable();

        this.proto = Some(proto.get());
        this.table.copy(vm, proto.table);

        this.as_gc()
    }
    pub fn new(vm: &mut VM) -> Gc<Obj> {
        let table = Table::new(vm, 2);
        vm.gc().fixed(Obj { table, proto: None })
    }
    pub fn with_capacity(vm: &mut VM, cap: usize) -> Gc<Obj> {
        let table = Table::new(vm, cap);
        vm.gc().fixed(Obj { table, proto: None })
    }
}

impl Gc<Obj> {
    pub fn field(&self, vm: &mut VM, field: &Value) -> Value {
        let mut o = Some(*self);

        gc_frame!(vm.gc().roots() => o: Option<Gc<Obj>>);
        while let Some(mut obj) = o.get() {
            let mut found = false;
            gc_frame!(vm.gc().roots() => obj: Gc<Obj>);
            let f = obj.as_ref().table.lookup(vm, field, &mut found);
            if found {
                return f;
            }

            o.set(obj.as_ref().proto);
        }
        Value::Null
    }

    pub fn insert(&mut self, vm: &mut VM, key: &Value, val: &Value) {
        self.table.insert(vm, key, val);
        vm.gc().write_barrier(*self);
    }
}

unsafe impl Trace for Obj {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        self.table.trace(vis);
        self.proto.trace(vis);
    }
}
unsafe impl Finalize for Obj {}
unsafe impl Allocation for Obj {
    const CELL_TYPE: crate::CellType = crate::CellType::Object;
}
impl Object for Obj {}

impl Index<usize> for Module {
    type Output = Op;
    fn index(&self, ix: usize) -> &Self::Output {
        unsafe { &*self.code.as_ptr().add(ix) }
    }
}
impl IndexMut<usize> for Module {
    fn index_mut(&mut self, ix: usize) -> &mut Self::Output {
        unsafe { &mut *self.code.as_mut_ptr().add(ix) }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_int32() {
            write!(f, "{}", self.get_int32())
        } else if self.is_double() {
            write!(f, "{}", self.get_double())
        } else if self.is_null() {
            write!(f, "null")
        } else if self.is_bool() {
            write!(f, "{}", self.get_bool())
        } else if let Some(x) = self.downcast_ref::<Str>() {
            write!(f, "{}", &**x)
        } else if let Some(x) = self.downcast_ref::<Sym>() {
            write!(f, "\"{}\"", &**x.name)
        } else if let Some(x) = self.downcast_ref::<Function>() {
            if x.prim {
                write!(f, "<primitive {:p}>", x)
            } else {
                write!(f, "<function {:p}>", x)
            }
        } else if let Some(x) = self.downcast_ref::<Obj>() {
            write!(f, "<object {:p}>", x)
        } else if let Some(x) = self.downcast_ref::<Array<Value>>() {
            write!(f, "<array {:p}: {}>", x, x.len())
        } else {
            assert!(self.is_object());
            write!(f, "<abstract {:p}>", self.get_object())
        }
        /*match self {
            Self::Null => write!(f, "null"),
            Self::Int(x) => write!(f, "{}", x),
            Self::Float(x) => write!(f, "{}", x),
            Self::Abstract(x) => write!(f, "<abstract {:p}>", *x),
            Self::Object(x) => write!(f, "<object {:p}>", *x),
            Self::Array(x) => write!(f, "<array {:p}: {}>", *x, x.len()),
            Self::Bool(x) => write!(f, "{}", x),
            Self::Str(x) => write!(f, "{}", &***x),
            Self::Table(x) => write!(f, "<table {:p}: {}>", *x, x.count),
            Self::Module(x) => write!(f, "<module {:p}>", *x),
            Self::Function(x) | Self::Primitive(x) => write!(f, "<func {:p}>", *x),
            Self::Symbol(x) => write!(f, "{}", &**x.name),
        }*/
    }
}
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.is_int32() {
            write!(f, "{}", self.get_int32())
        } else if self.is_double() {
            write!(f, "{}", self.get_double())
        } else if self.is_null() {
            write!(f, "null")
        } else if self.is_bool() {
            write!(f, "{}", self.get_bool())
        } else if let Some(x) = self.downcast_ref::<Str>() {
            write!(f, "{}", &**x)
        } else if let Some(x) = self.downcast_ref::<Sym>() {
            write!(f, "\"{}\"", &**x.name)
        } else if let Some(x) = self.downcast_ref::<Function>() {
            if x.prim {
                write!(f, "<primitive {:p}>", x)
            } else {
                write!(f, "<function {:p}>", x)
            }
        } else if let Some(x) = self.downcast_ref::<Obj>() {
            write!(f, "<object {:p}>", x)
        } else if let Some(x) = self.downcast_ref::<Array<Value>>() {
            write!(f, "<array {:p}: {}>", x, x.len())
        } else {
            assert!(self.is_object());
            write!(f, "<abstract {:p}>", self.get_object())
        }
    }
}

pub mod nanbox {
    use crate::memory::{
        gcwrapper::{Array, Gc, Str},
        Object, Trace,
    };

    use super::{purenan::*, Function, Module, Obj, Sym, Table};
    #[derive(Copy, Clone)]
    #[repr(C)]
    pub struct Value(pub(crate) EncodedValueDescriptor);

    impl std::fmt::Pointer for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            unsafe { write!(f, "Value(0x{:x})", self.0.as_int64) }
        }
    }
    #[derive(Clone, Copy)]
    #[repr(C)]
    pub(crate) union EncodedValueDescriptor {
        pub as_int64: i64,
        #[cfg(target_pointer_width = "32")]
        pub as_double: f64,

        pub ptr: usize,
        #[cfg(target_pointer_width = "32")]
        pub as_bits: AsBits,
    }
    impl PartialEq for Value {
        fn eq(&self, other: &Self) -> bool {
            unsafe { self.0.as_int64 == other.0.as_int64 }
        }
    }
    impl Eq for Value {}

    impl std::fmt::Debug for EncodedValueDescriptor {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_fmt(format_args!("EncodedValueDescriptor {}", unsafe {
                self.ptr
            }))
        }
    }
    impl Value {
        pub fn raw(self) -> u64 {
            unsafe { std::mem::transmute(self) }
        }
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    #[cfg(target_endian = "little")]
    #[repr(C)]
    pub struct AsBits {
        pub payload: i32,
        pub tag: i32,
    }
    #[derive(Clone, Copy, PactxialEq, Eq)]
    #[cfg(target_endian = "big")]
    #[repr(C)]
    pub struct AsBits {
        pub tag: i32,
        pub payload: i32,
    }

    #[cfg(target_pointer_width = "64")]
    impl Value {
        /*
         * On 64-bit platforms USE(JSVALUE64) should be defined, and we use a NaN-encoded
         * form for immediates.
         *
         * The encoding makes use of unused NaN space in the IEEE754 representation.  Any value
         * with the top 13 bits set represents a QNaN (with the sign bit set).  QNaN values
         * can encode a 51-bit payload.  Hardware produced and C-library payloads typically
         * have a payload of zero.  We assume that non-zero payloads are available to encode
         * pointer and integer values.  Since any 64-bit bit pattern where the top 15 bits are
         * all set represents a NaN with a non-zero payload, we can use this space in the NaN
         * ranges to encode other values (however there are also other ranges of NaN space that
         * could have been selected).
         *
         * This range of NaN space is represented by 64-bit numbers begining with the 15-bit
         * hex patterns 0xFFFC and 0xFFFE - we rely on the fact that no valid double-precision
         * numbers will fall in these ranges.
         *
         * The top 15-bits denote the type of the encoded JSValue:
         *
         *     Pointer {  0000:PPPP:PPPP:PPPP
         *              / 0002:****:****:****
         *     Double  {         ...
         *              \ FFFC:****:****:****
         *     Integer {  FFFE:0000:IIII:IIII
         *
         * The scheme we have implemented encodes double precision values by performing a
         * 64-bit integer addition of the value 2^49 to the number. After this manipulation
         * no encoded double-precision value will begin with the pattern 0x0000 or 0xFFFE.
         * Values must be decoded by reversing this operation before subsequent floating point
         * operations may be peformed.
         *
         * 32-bit signed integers are marked with the 16-bit tag 0xFFFE.
         *
         * The tag 0x0000 denotes a pointer, or another form of tagged immediate. Boolean,
         * null and undefined values are represented by specific, invalid pointer values:
         *
         *     False:     0x06
         *     True:      0x07
         *     Undefined: 0x0a
         *     Null:      0x02
         *
         * These values have the following properties:
         * - Bit 1 (Othectxag) is set for all four values, allowing real pointers to be
         *   quickly distinguished from all immediate values, including these invalid pointers.
         * - With bit 3 masked out (UndefinedTag), Undefined and Null share the
         *   same value, allowing null & undefined to be quickly detected.
         *
         * No valid JSValue will have the bit pattern 0x0, this is used to represent array
         * holes, and as a C++ 'no value' result (e.g. JSValue() has an internal value of 0).
         *
         * When USE(BIGINT32), we have a special representation for BigInts that are small (32-bit at most):
         *      0000:XXXX:XXXX:0012
         * This representation works because of the following things:
         * - It cannot be confused with a Double or Integer thanks to the top bits
         * - It cannot be confused with a pointer to a Cell, thanks to bit 1 which is set to true
         * - It cannot be confused with a pointer to wasm thanks to bit 0 which is set to false
         * - It cannot be confused with true/false because bit 2 is set to false
         * - It cannot be confused for null/undefined because bit 4 is set to true
         */

        pub const DOUBLE_ENCODE_OFFSET_BIT: usize = 49;
        pub const DOUBLE_ENCODE_OFFSET: i64 = 1 << Self::DOUBLE_ENCODE_OFFSET_BIT as i64;
        pub const NUMBER_TAG: i64 = 0xfffe000000000000u64 as i64;
        pub const LOWEST_OF_HIGH_BITS: i64 = 1 << 49;

        pub const OTHER_TAG: i32 = 0x2;
        pub const BOOL_TAG: i32 = 0x4;
        pub const UNDEFINED_TAG: i32 = 0x8;
        pub const NATIVE32_TAG: i32 = 0x12;
        pub const NATIVE32_MASK: i64 = Self::NUMBER_TAG | Self::NATIVE32_TAG as i64;

        pub const VALUE_FALSE: i32 = Self::OTHER_TAG | Self::BOOL_TAG | false as i32;
        pub const VALUE_TRUE: i32 = Self::OTHER_TAG | Self::BOOL_TAG | true as i32;
        pub const VALUE_UNDEFINED: i32 = Self::OTHER_TAG | Self::UNDEFINED_TAG;
        pub const VALUE_NULL: i32 = Self::OTHER_TAG;

        pub const MISC_TAG: i64 =
            Self::OTHER_TAG as i64 | Self::BOOL_TAG as i64 | Self::UNDEFINED_TAG as i64;
        pub const NOT_CELL_MASK: i64 = Self::NUMBER_TAG as i64 | Self::OTHER_TAG as i64;

        pub const VALUE_EMPTY: i64 = 0x0;
        pub const VALUE_DELETED: i64 = 0x4;

        pub const UNDEFINED: Value = Self::encode_undefined_value();

        #[inline]
        pub fn encode_empty_value() -> Self {
            Self(EncodedValueDescriptor {
                as_int64: Self::VALUE_EMPTY,
            })
        }
        #[inline]
        pub fn encode_object_value<T: Object + ?Sized>(gc: Gc<T>) -> Self {
            Self(EncodedValueDescriptor {
                ptr: unsafe { std::mem::transmute(gc) },
            })
        }

        #[inline]
        pub const fn encode_undefined_value() -> Self {
            Self(EncodedValueDescriptor {
                as_int64: Self::VALUE_UNDEFINED as _,
            })
        }

        #[inline]
        pub const fn encode_null_value() -> Self {
            Self(EncodedValueDescriptor {
                as_int64: Self::VALUE_NULL as _,
            })
        }

        #[inline]
        pub fn encode_bool_value(x: bool) -> Self {
            if x {
                Self(EncodedValueDescriptor {
                    as_int64: Self::VALUE_TRUE as _,
                })
            } else {
                Self(EncodedValueDescriptor {
                    as_int64: Self::VALUE_FALSE as _,
                })
            }
        }
        #[inline]
        pub fn is_empty(self) -> bool {
            unsafe { self.0.as_int64 == Self::VALUE_EMPTY }
        }

        #[inline]
        pub fn is_undefined(self) -> bool {
            self == Self::encode_undefined_value()
        }
        #[inline]
        pub fn is_null(self) -> bool {
            self == Self::encode_null_value()
        }

        #[inline]
        pub fn is_true(self) -> bool {
            self == Self::encode_bool_value(true)
        }

        #[inline]
        pub fn is_false(self) -> bool {
            self == Self::encode_bool_value(false)
        }

        #[inline]
        pub fn is_boolean(self) -> bool {
            unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
        }

        #[inline]
        pub fn is_pointer(self) -> bool {
            unsafe { (self.0.as_int64 & Self::NOT_CELL_MASK) == 0 }
        }

        #[inline]
        pub fn is_int32(self) -> bool {
            unsafe { (self.0.as_int64 & Self::NUMBER_TAG) == Self::NUMBER_TAG }
        }

        #[inline]
        pub fn is_number(self) -> bool {
            unsafe { (self.0.as_int64 & Self::NUMBER_TAG) != 0 }
        }

        #[inline]
        pub fn get_object(self) -> Gc<dyn Object> {
            assert!(self.is_object());

            unsafe { std::mem::transmute(self.0.ptr) }
        }

        #[inline]
        pub fn is_object(self) -> bool {
            self.is_pointer() && !self.is_empty()
        }
        #[inline]
        pub fn get_int32(self) -> i32 {
            unsafe { self.0.as_int64 as i32 }
        }

        #[inline]
        pub fn get_number(self) -> f64 {
            if self.is_int32() {
                return self.get_int32() as _;
            }
            self.get_double()
        }
        #[inline]
        pub fn get_double(self) -> f64 {
            assert!(self.is_double());
            f64::from_bits((unsafe { self.0.as_int64 - Self::DOUBLE_ENCODE_OFFSET }) as u64)
        }
        #[inline]
        pub fn is_double(self) -> bool {
            self.is_number() && !self.is_int32()
        }

        #[inline]
        pub fn is_bool(self) -> bool {
            unsafe { (self.0.as_int64 & !1) == Self::VALUE_FALSE as i64 }
        }

        #[inline]
        pub fn encode_f64_value(x: f64) -> Self {
            Self(EncodedValueDescriptor {
                as_int64: x.to_bits() as i64 + Self::DOUBLE_ENCODE_OFFSET,
            })
        }

        #[inline]
        pub fn encode_untrusted_f64_value(x: f64) -> Self {
            Self::encode_f64_value(purify_nan(x))
        }

        #[inline]
        pub fn encode_nan_value() -> Self {
            Self::encode_f64_value(pure_nan())
        }

        #[inline]
        pub const fn encode_int32(x: i32) -> Self {
            Self(EncodedValueDescriptor {
                as_int64: Self::NUMBER_TAG | x as u32 as u64 as i64,
            })
        }

        #[inline]
        pub fn get_raw(self) -> i64 {
            unsafe { self.0.as_int64 }
        }

        #[inline]
        pub fn get_native_u32(self) -> u32 {
            unsafe { (self.0.as_int64 >> 16) as u32 }
        }

        #[inline]
        pub fn encode_native_u32(x: u32) -> Self {
            Self(EncodedValueDescriptor {
                as_int64: (((x as u64) << 16) | Self::NATIVE32_TAG as u64) as i64,
            })
        }
        #[inline]
        pub fn is_native_value(self) -> bool {
            unsafe { (self.0.as_int64 & Self::NATIVE32_MASK) == Self::NATIVE32_TAG as i64 }
        }

        #[inline]
        pub fn get_bool(self) -> bool {
            assert!(self.is_bool());
            self == Self::encode_bool_value(true)
        }
    }

    #[allow(non_snake_case, non_upper_case_globals)]
    impl Value {
        pub const fn Int(x: i32) -> Self {
            Self::encode_int32(x)
        }

        pub fn Float(x: f64) -> Self {
            Self::encode_f64_value(x)
        }
        pub fn Object(x: Gc<Obj>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Array(x: Gc<Array<Self>>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Symbol(x: Gc<Sym>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Str(x: Gc<Str>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Abstract(x: Gc<dyn Object>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Table(x: Gc<Table>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Bool(x: bool) -> Self {
            Self::encode_bool_value(x)
        }

        pub fn Primitive(x: Gc<Function>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn Function(x: Gc<Function>) -> Self {
            Self::encode_object_value(x)
        }

        pub fn downcast_ref<T: 'static + Object>(self) -> Option<Gc<T>> {
            if self.is_object() {
                self.get_object().downcast()
            } else {
                None
            }
        }

        pub fn is_function(self) -> bool {
            self.downcast_ref::<Function>().is_some()
        }

        pub fn is_primitive(self) -> bool {
            match self.downcast_ref::<Function>() {
                Some(x) => x.prim,
                _ => false,
            }
        }
        pub fn is_obj(self) -> bool {
            self.downcast_ref::<Obj>().is_some()
        }
        pub fn is_table(self) -> bool {
            self.downcast_ref::<Table>().is_some()
        }

        pub fn is_array(self) -> bool {
            self.downcast_ref::<Array<Value>>().is_some()
        }
        pub fn is_symbol(self) -> bool {
            self.downcast_ref::<Sym>().is_some()
        }

        pub fn is_str(self) -> bool {
            self.downcast_ref::<Str>().is_some()
        }

        pub fn is_module(self) -> bool {
            self.downcast_ref::<Module>().is_some()
        }

        pub const Null: Self = Self::encode_null_value();
    }

    unsafe impl Trace for Value {
        fn trace(&mut self, visitor: &mut dyn crate::memory::Visitor) {
            if self.is_object() {
                let mut obj = self.get_object();
                obj.trace(visitor);
                *self = Self::encode_object_value(obj);
            }
        }
    }
}

pub mod purenan {
    //! NaN (not-a-number) double values are central to how JavaScriptCore encodes JavaScript
    //! values (Values).  All values, including integers and non-numeric values, are always
    //! encoded using the IEEE 754 binary double format.  Non-double values are encoded using
    //! a NaN with the sign bit set.  The 51-bit payload is then used for encoding the actual
    //! value - be it an integer or a pointer to an object, or something else. But we only
    //! make use of the low 49 bits and the top 15 bits being all set to 1 is the indicator
    //! that a value is not a double. Top 15 bits being set to 1 also indicate a signed
    //! signaling NaN with some additional NaN payload bits.
    //!
    //! Our use of NaN encoding means that we have to be careful with how we use NaNs for
    //! ordinary doubles. For example, it would be wrong to ever use a NaN that has the top
    //! 15 bits set, as that would look like a non-double value to JSC.
    //!
    //! We can trust that on all of the hardware/OS combinations that we care about,
    //! NaN-producing math operations never produce a NaN that looks like a tagged value. But
    //! if we're ever in a situation where we worry about it, we can use purifyNaN() to get a
    //! NaN that doesn't look like a tagged non-double value. The JavaScript language doesn't
    //! distinguish between different flavors of NaN and there is no way to detect what kind
    //! of NaN you have - hence so long as all double NaNs are purified then our tagging
    //! scheme remains sound.
    //!
    //! It's worth noting that there are cases, like sin(), that will almost produce a NaN
    //! that breaks us. sin(-inf) returns 0xfff8000000000000. This doesn't break us because
    //! not all of the top 15 bits are set. But it's very close. Hence our assumptions about
    //! NaN are just about the most aggressive assumptions we could possibly make without
    //! having to call purifyNaN() in surprising places.
    //!
    //! For naming purposes, we say that a NaN is "pure" if it is safe to tag, in the sense
    //! that doing so would result in a tagged value that would pass the "are you a double"
    //! test. We say that a NaN is "impure" if attempting to tag it would result in a value
    //! that would look like something other than a double.

    /// Returns some kind of pure NaN.
    #[inline(always)]
    pub fn pure_nan() -> f64 {
        f64::from_bits(0x7ff8000000000000)
    }

    #[inline]
    pub fn is_impure_nan(value: f64) -> bool {
        // Tests if the double value would break Value64 encoding, which is the most
        // aggressive kind of encoding that we currently use.
        value.to_bits() >= 0xfffe000000000000
    }
    /// If the given value is NaN then return a NaN that is known to be pure.
    pub fn purify_nan(value: f64) -> f64 {
        if value != value {
            return pure_nan();
        }
        value
    }
}

#[repr(C)]
pub struct ByteBuffer {
    pub(crate) len: usize,
    pub(crate) data: [u8; 0],
}

unsafe impl Trace for ByteBuffer {}
unsafe impl Finalize for ByteBuffer {}
unsafe impl Allocation for ByteBuffer {
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(Self, len);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(Self, data);
}

impl Object for ByteBuffer {}

impl Index<usize> for ByteBuffer {
    type Output = u8;
    fn index(&self, index: usize) -> &Self::Output {
        assert!(index < self.len);
        unsafe { &*self.data.as_ptr().add(index as _) }
    }
}

impl IndexMut<usize> for ByteBuffer {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        assert!(index < self.len);
        unsafe { &mut *self.data.as_mut_ptr().add(index as _) }
    }
}

impl Deref for ByteBuffer {
    type Target = [u8];
    fn deref(&self) -> &Self::Target {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.len) }
    }
}

impl DerefMut for ByteBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { std::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.len) }
    }
}
