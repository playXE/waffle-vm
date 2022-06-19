use crate::interpreter::*;
use crate::{
    gc_frame,
    memory::{
        gcwrapper::{Array, GCWrapper, Gc, Nullable, Str},
        Allocation, Finalize, Managed, Trace, Visitor,
    },
    object::Object,
    opcode::Op,
    structure::Structure,
    value::{ByteBuffer, Function, Module, Upvalue, Value, INVALID_CMP},
};
use std::{
    hash::Hash,
    mem::size_of,
    panic::{self, catch_unwind, resume_unwind, AssertUnwindSafe},
    ptr::null_mut,
    sync::atomic::AtomicBool,
};

pub struct VM {
    gc: GCWrapper,

    pub(crate) sp: *mut Value,
    pub(crate) csp: *mut Value,
    pub(crate) vthis: Value,
    pub(crate) env: Nullable<Array<Nullable<Upvalue>>>,

    pub(crate) spmin: *mut Value,
    pub(crate) spmax: *mut Value,
    pub(crate) trap: isize,
    pub(crate) exc_stack: Value,

    pub(crate) callback_ret: Nullable<Function>,
    pub(crate) callback_mod: Nullable<Module>,
    trace_hook: u32,

    pub(crate) builtins: Value,

    ids: [Symbol; Id::Last as u8 as usize],
    pub(crate) open_upvalues: Nullable<Upvalue>,
    pub(crate) global: GlobalData,
}

#[derive(Default)]
pub struct GlobalData {
    pub empty_object_struct: Nullable<Structure>,
    pub object_prototype: Nullable<Object>,
    pub int_structure: Nullable<Structure>,
    pub float_structure: Nullable<Structure>,
    pub string_structure: Nullable<Structure>,
    pub array_prototype: Nullable<Object>,
    pub array_structure: Nullable<Structure>,
    pub number_structure: Nullable<Structure>,
    pub number_prototype: Nullable<Object>,
}

unsafe impl Trace for GlobalData {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.empty_object_struct.trace(visitor);
        self.object_prototype.trace(visitor);
        self.int_structure.trace(visitor);
        self.float_structure.trace(visitor);
        self.string_structure.trace(visitor);
        self.array_prototype.trace(visitor);
        self.array_structure.trace(visitor);
        self.number_structure.trace(visitor);
        self.number_prototype.trace(visitor);
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Id {
    Loader,
    Exports,
    Cache,
    Path,
    Libs,
    Length,
    Constructor,
    Last,
}

static IDS: &'static [&'static str] = &[
    "loader",
    "exports",
    "cache",
    "path",
    "libs",
    "length",
    "constructor",
];

unsafe impl Trace for VM {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        unsafe {
            let mut cursor = self.spmin;
            let end = self.spmax;
            while cursor < end {
                (&mut *cursor).trace(vis);
                cursor = cursor.add(1);
            }

            self.vthis.trace(vis);
            self.env.trace(vis);
            self.callback_mod.trace(vis);
            self.callback_ret.trace(vis);
            self.exc_stack.trace(vis);
            self.builtins.trace(vis);

            self.open_upvalues.trace(vis);

            self.global.trace(vis);
        }
    }
}

impl VM {
    pub const STACK_SIZE: usize = 256;

    pub fn this(&self) -> &Value {
        &self.vthis
    }

    pub fn intern(&mut self, x: impl Internable) -> Gc<Symbol> {
        self.gc().fixed(x.intern())
    }
    pub fn expect_str(&mut self, val: &Value) -> Gc<Str> {
        if let Some(str) = val.downcast_ref::<Str>() {
            return str;
        }
        self.throw_str("string expected")
    }

    pub fn expect_int(&mut self, val: &Value) -> i32 {
        if val.is_int32() {
            return val.get_int32();
        }
        self.throw_str("integer expected")
    }

    pub fn expect_float(&mut self, val: &Value) -> f64 {
        if val.is_double() {
            return val.get_double();
        }
        self.throw_str("float expected")
    }

    pub fn expect_bytebuffer(&mut self, val: &Value) -> Gc<ByteBuffer> {
        if let Some(buf) = val.downcast_ref::<ByteBuffer>() {
            return buf;
        }
        self.throw_str("bytebuffer expected")
    }

    pub fn expect<T: 'static + Managed, F: AsRef<str>>(&mut self, val: &Value, msg: F) -> Gc<T> {
        if let Some(obj) = val.downcast_ref::<T>() {
            return obj;
        }
        self.throw_str(msg)
    }

    pub fn new(stack_size: Option<usize>) -> &'static mut VM {
        initialize_symbol_table();
        //let symtab_size = read_uint_from_env("WAFFLE_SYMTAB_SIZE").unwrap_or_else(|| 128);
        let mut this = Box::leak(Box::new(VM {
            gc: GCWrapper::new(),
            env: Nullable::NULL,
            vthis: Value::Null,
            exc_stack: Value::Null,
            callback_mod: Nullable::NULL,
            callback_ret: Nullable::NULL,
            sp: null_mut(),
            spmax: null_mut(),
            spmin: null_mut(),
            csp: null_mut(),
            trace_hook: u32::MAX,
            trap: 0,
            builtins: Value::Null,

            ids: [DUMMY_SYMBOL; Id::Last as usize],
            open_upvalues: Nullable::NULL,
            global: Default::default(),
        }));

        let stack_size = stack_size.unwrap_or_else(|| VM::STACK_SIZE);

        unsafe {
            let mem = libc::malloc(stack_size * size_of::<Value>()).cast::<Value>();
            for i in 0..stack_size {
                mem.add(i).write(Value::Null);
            }
            this.spmin = mem;
            this.spmax = mem.add(stack_size);
            this.sp = this.spmax;
            this.csp = this.spmin;

            let p = this as *mut Self;

            this.trace_hook = this.gc.add_trace_callback(move |visitor| {
                let vm = &mut *p;

                vm.trace(visitor);
            });
        }

        let (f, m) = callback_return(this);
        this.callback_ret = f.nullable();
        this.callback_mod = m.nullable();
        for (i, id) in IDS.iter().enumerate() {
            let s = id.intern();
            this.ids[i] = s;
        }
        super::runtime::object::init(&mut this);
        super::builtin::init_builtin(&mut this);
        gc_frame!(this.gc().roots() => builtins = this.builtins.downcast_ref::<Object>().unwrap());
        let proto = this.global.object_prototype;
        builtins.put(
            &mut this,
            "object".intern(),
            Value::Object(proto.as_gc()),
            false,
        );
        super::runtime::number::init(&mut this);
        super::runtime::int::init(&mut this);
        super::runtime::float::init(&mut this);
        super::runtime::array::init(&mut this);
        this.gc().collect(0, &mut []);
        this
    }

    pub fn id(&mut self, id: Id) -> Symbol {
        self.ids[id as usize]
    }

    pub fn gc(&mut self) -> &mut GCWrapper {
        &mut self.gc
    }

    pub fn identity_cmp<T: Managed + ?Sized>(&mut self, a: Gc<T>, b: Gc<T>) -> i32 {
        let id1 = self.gc.identity(a);
        let id2 = self.gc.identity(b);

        if id1 == id2 {
            0
        } else {
            INVALID_CMP
        }
    }

    pub fn throw(&mut self, value: Value) -> ! {
        self.exc_stack = Value::Array(self.gc().array(0, Value::Null));
        self.vthis = value;

        panic::resume_unwind(Box::new(VMTrap));
    }

    pub fn rethrow(&mut self, value: Value) -> ! {
        self.vthis = value;
        panic::resume_unwind(Box::new(VMTrap));
    }

    unsafe fn interp(
        &mut self,
        mut module: Nullable<Module>,
        mut acc: Value,
        mut ip: usize,
    ) -> Value {
        let init_sp = (self.spmax as usize - self.sp as usize) as *mut Value;
        let mut csp;
        let mut sp;
        let mut trap;
        gc_frame!(self.gc().roots() => acc: Value, module: Nullable<Module>);
        loop {
            match panic::catch_unwind(AssertUnwindSafe(|| {
                // Execute code
                acc.set(interp_loop(self, module.get_copy(), acc.get_copy(), ip));
            })) {
                // if no error thrown just return accumulator register
                Ok(_) => return acc.get_copy(),
                // VM trap occured
                Err(x) if x.is::<VMTrap>() => {
                    acc.set(self.vthis);

                    if self.trap == 0 || self.trap <= init_sp as isize {
                        // uncaught or outside init stack, reraise
                        panic::resume_unwind(Box::new(VMTrap));
                    }

                    trap = self.spmax.cast::<u8>().sub(self.trap as _).cast::<Value>();

                    if trap < self.sp {
                        // trap outside stack
                        self.trap = 0;
                        let msg = Value::Str(self.gc().str("Invalid Trap"));
                        self.throw(msg);
                    }

                    // pop csp
                    csp = self.spmin.add(trap.read().int() as _);
                    self.csp = csp;
                    // restore state
                    self.vthis = trap.add(1).read();
                    self.env = match trap.add(2).read() {
                        x if x.is_null() => Nullable::NULL,
                        x => x
                            .downcast_ref::<Array<Nullable<Upvalue>>>()
                            .unwrap()
                            .nullable(),
                    };

                    ip = trap.add(3).read().int() as usize;
                    // if thrown from native code `module` is allowed to be null
                    module.set(trap.add(4).read().module());
                    // pop sp
                    sp = trap.add(6);

                    self.trap = trap.add(5).read().int() as _;
                    while self.sp < sp {
                        self.sp.write(Value::Null);
                        self.sp = self.sp.add(1);
                    }
                }
                Err(x) => panic::resume_unwind(x),
            }
        }
    }

    pub fn throw_str(&mut self, x: impl AsRef<str>) -> ! {
        let x = Value::Str(self.gc().str(x.as_ref()));
        self.throw(x)
    }

    unsafe fn setup_trap(&mut self) {
        self.sp = self.sp.sub(6);

        if self.sp <= self.csp {
            self.throw_str("Stack Overflow");
        }

        self.sp
            .write(Value::Int((self.csp as isize - self.spmin as isize) as _));
        self.sp.add(1).write(self.vthis);
        self.sp.add(2).write(if self.env.is_not_null() {
            Value::Abstract(self.env.as_gc().as_dyn())
        } else {
            Value::Null
        });
        self.sp.add(3).write(Value::Int(0));
        self.sp.add(4).write(Value::Null);
        self.sp.add(5).write(Value::Int(self.trap as _));
        self.trap = self.spmax as isize - self.sp as isize;
    }
    pub fn calln(&mut self, f: Value, args: &[Value]) -> Value {
        unsafe { self.callex(Value::Null, f, args, &mut None) }
    }

    pub fn ocalln(&mut self, this: Value, f: Value, args: &[Value]) -> Value {
        unsafe { self.callex(this, f, args, &mut None) }
    }

    pub fn call0(&mut self, f: Value) -> Value {
        unsafe { self.callex(Value::Null, f, &[], &mut None) }
    }

    pub fn call1(&mut self, f: Value, a0: Value) -> Value {
        let mut args = [a0];
        gc_frame!(self.gc.roots() => args: [Value;1]);
        unsafe { self.callex(Value::Null, f, args.as_ref(), &mut None) }
    }
    pub fn call2(&mut self, f: Value, a0: Value, a1: Value) -> Value {
        let mut args = [a0, a1];
        gc_frame!(self.gc.roots() => args: [Value;2]);
        unsafe { self.callex(Value::Null, f, args.as_ref(), &mut None) }
    }

    pub fn call3(&mut self, f: Value, a0: Value, a1: Value, a2: Value) -> Value {
        let mut args = [a0, a1, a2];
        gc_frame!(self.gc.roots() => args: [Value;3]);
        unsafe { self.callex(Value::Null, f, args.as_ref(), &mut None) }
    }

    pub fn ocall0(&mut self, this: Value, f: Value) -> Value {
        unsafe { self.callex(this, f, &[], &mut None) }
    }

    pub fn ocall1(&mut self, this: Value, f: Value, a0: Value) -> Value {
        let mut args = [a0];
        gc_frame!(self.gc.roots() => args: [Value;1]);
        unsafe { self.callex(this, f, args.as_ref(), &mut None) }
    }
    pub fn ocall2(&mut self, this: Value, f: Value, a0: Value, a1: Value) -> Value {
        let mut args = [a0, a1];
        gc_frame!(self.gc.roots() => args: [Value;2]);
        unsafe { self.callex(this, f, args.as_ref(), &mut None) }
    }

    pub fn ocall3(&mut self, this: Value, f: Value, a0: Value, a1: Value, a2: Value) -> Value {
        let mut args = [a0, a1, a2];
        gc_frame!(self.gc.roots() => args: [Value;3]);
        unsafe { self.callex(this, f, args.as_ref(), &mut None) }
    }
    /// Implements function call
    pub unsafe fn callex(
        &mut self,
        mut vthis: Value,
        mut f: Value,
        args: &[Value],
        exc: &mut Option<Value>,
    ) -> Value {
        let mut old_this = self.vthis;
        let mut old_env = self.env;
        gc_frame!(self.gc().roots() => old_this: Value,old_env: Value,vthis: Value,f: Value);
        let mut ret = Value::Null;

        if !vthis.is_null() {
            self.vthis = vthis.get_copy();
        }
        loop {
            self.setup_trap();
            let result = panic::catch_unwind(AssertUnwindSafe(|| match f.get_copy() {
                x if x.is_function() => {
                    let x = x.prim_or_func();
                    check_arguments(self, args.len() as _, x.nargs as _, x.varsize);
                    if x.prim {
                        self.env = x.env;
                        ret = dispatch_func2(
                            self,
                            args.as_ptr() as _,
                            x.addr,
                            args.len() as _,
                            x.varsize,
                        )
                    } else {
                        if self.csp.add(5) >= self.sp.sub(args.len()) {
                            if let Some(_) = exc {
                                self.process_trap();
                            }
                            self.throw_str("Stack Overflow");
                        }
                        for i in 0..args.len() {
                            self.sp = self.sp.sub(1);
                            self.sp.write(args[i]);
                        }

                        self.env = x.env;

                        self.csp = self.csp.add(1);
                        self.csp.write(Value::Function(self.callback_ret.as_gc()));

                        self.csp = self.csp.add(1);
                        self.csp.write(Value::Int(0));
                        self.csp = self.csp.add(1);
                        self.csp.write(Value::Int(0));
                        self.csp = self.csp.add(1);
                        self.csp
                            .write(Value::encode_object_value(self.callback_mod.as_gc()));
                        self.csp = self.csp.add(1);
                        self.csp.write(Value::Bool(false));

                        ret = self.interp(x.module, Value::Null, x.addr);
                    }
                }
                x => panic!("{}", x), //self.throw_str(format!("invalid call {}",x)),
            }));

            match result {
                Ok(_) => break,
                Err(x) => {
                    if let Some(_) = x.downcast_ref::<VMTrap>() {
                        *exc = Some(self.vthis);

                        self.process_trap();
                        self.vthis = old_this.get_copy();
                        self.env = old_env.get_copy();

                        return Value::Null;
                    } else {
                        resume_unwind(x);
                    }
                }
            }
        }

        self.vthis = old_this.get_copy();
        self.env = old_env.get_copy();
        ret
    }

    unsafe fn process_trap(&mut self) {
        let mut sp;

        if self.trap == 0 {
            return;
        }

        let trap = (self.spmax as isize - self.trap) as *mut Value;

        sp = self.spmin.offset(trap.read().int() as _);

        self.csp = sp;
        self.vthis = trap.add(1).read();
        self.env = match trap.add(2).read() {
            x if x.is_null() => Nullable::NULL,
            x => x
                .downcast_ref::<Array<Nullable<Upvalue>>>()
                .unwrap()
                .nullable(),
        };

        sp = trap.add(6);
        self.trap = trap.add(5).read().int() as _;
        while self.sp < sp {
            self.sp.write(Value::Null);
            self.sp = self.sp.add(1);
        }
    }
    pub(crate) unsafe fn close_upvalues(&mut self, slot: *mut Value) {
        let mut prev = Nullable::<Upvalue>::NULL;
        let mut ls = self.open_upvalues;
        while ls.is_not_null() {
            if ls.state.slot == slot {
                ls.closed = true;
                ls.state.local = slot.read();
                if prev.is_not_null() {
                    prev.next = ls.next;
                } else {
                    self.open_upvalues = ls.next;
                }
            } else {
                prev = ls;
                ls = ls.next;
            }
        }
    }
    /// Executes the given module code and returns the value stored in accumulator in the end.
    pub fn execute(&mut self, mut m: Gc<Module>) -> Result<Value, Value> {
        let mut old_env = self.env;
        let mut old_vthis = self.vthis;
        gc_frame!(self.gc().roots()=>m: Gc<Module>,old_env: Value,old_vthis: Value);
        self.env = Nullable::NULL;
        self.vthis = Value::Null;
        let ret = match catch_unwind(AssertUnwindSafe(|| unsafe {
            self.interp(m.nullable(), Value::Null, 0)
        })) {
            // No traps or panics, return accumulator value
            Ok(val) => Ok(val),
            // catch trap thrown by user or VM
            Err(x) if x.is::<VMTrap>() => Err(self.vthis),
            // if this is not an VM error just resume unwinding
            Err(x) => resume_unwind(x),
        };
        self.vthis = old_vthis.get_copy();
        self.env = old_env.get_copy();

        ret
    }
}

use libloading_mini::Library;

pub struct VMTrap;

/// Generates `return` function. This function actually returns control from bytecode interpreter
/// to native code that entered bytecode interpreter.
///
///
/// When native code invokes bytecode function `return` function is always appended to call stack.
fn callback_return(vm: &mut VM) -> (Gc<Function>, Gc<Module>) {
    unsafe {
        let module = vm.gc().malloc_varsize::<Module>(1, &mut []);

        let module_ptr = module.as_mut_ptr();
        module_ptr.write(Module {
            name: Value::Null,
            globals: Nullable::NULL,
            exports: Value::Null,
            loader: Value::Null,
            feedback: Nullable::NULL,
            code_size: 1,
            code: [],
        });
        let mut module = module.assume_init();
        module.code.as_mut_ptr().write(Op::Leave as _);

        let func = vm.gc().malloc_fixedsize::<Function>(&mut [&mut module]);
        func.as_mut_ptr().write(Function {
            nargs: 0,
            varsize: false,
            env: Nullable::NULL,
            addr: 0,
            module: module.nullable(),
            prim: false,
            construct_struct: Nullable::NULL,
        });

        (func.assume_init(), module)
    }
}

pub struct LibList {
    pub v: Vec<Lib>,
}

pub struct Lib {
    pub handle: Library,
    pub name: String,
}

unsafe impl Finalize for LibList {}
unsafe impl Trace for LibList {}
unsafe impl Allocation for LibList {
    const FINALIZE: bool = true;
    const LIGHT_FINALIZER: bool = true;
}
impl Managed for LibList {}

pub const WAFFLE_TYPEOF: [Value; 17] = [
    Value::Int(0),
    Value::Int(1),
    Value::Int(2),
    Value::Int(3),
    Value::Int(4),
    Value::Int(5),
    Value::Int(6),
    Value::Int(7),
    Value::Int(8),
    Value::Int(9),
    Value::Int(10),
    Value::Int(11),
    Value::Int(12),
    Value::Int(13),
    Value::Int(14),
    Value::Int(15),
    Value::Int(16),
];

macro_rules! builtin_symbols {
    ($m: ident) => {
        $m! {
            /*PROTOTYPE prototype 0,
            TO_STRING toString 1,
            CONSTRUCTOR constructor 2,
            LENGTH length 3,
            BYTE_LENGTH byteLength 4,
            GET get 5,
            SET set 6,
            CALL call 7,
            APPLY apply 8*/

        }
    };
}

macro_rules! def_sid {
    ($($id: ident $val: ident $ix: expr),*) => {
        $(pub const $id: SymbolID = SymbolID($ix);)*
    };
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
pub struct SymbolID(pub(crate) u32);

impl SymbolID {
    builtin_symbols! {
        def_sid
    }

    pub const PUBLIC_START: SymbolID = Self(128);
}
/// VirtualMachine symbol type.
///
///
/// This type is used as property names and inside Symbol.
#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub enum Symbol {
    /// Interned string.
    Key(SymbolID),
    /// Private symbol. You can't create it in JS world.
    Private(SymbolID),
    /// Represents index value, this variant is used when you can definetely put array
    /// index inside u32 so it does not take space in interner.
    Index(u32),
}

macro_rules! def_sym {
    ($($id: ident $val: ident $ix: expr),*) => {
        $(
            pub const $id: Symbol = Symbol::Key(SymbolID::$id);
        )*
    };
}

impl Symbol {
    builtin_symbols! {
        def_sym
    }
    pub fn private(self) -> Self {
        match self {
            Self::Key(x) => Self::Private(x),
            _ => unreachable!(),
        }
    }
    pub fn get_id(self) -> SymbolID {
        match self {
            Self::Key(x) => x,
            Self::Private(x) => x,
            _ => unreachable!(),
        }
    }
    pub fn is_index(self) -> bool {
        /*match self {
            Self::Index(_) => true,
            _ => false,
        }*/
        matches!(self, Self::Index(_))
    }
    pub fn get_index(self) -> u32 {
        match self {
            Self::Index(x) => x,
            _ => unreachable!(),
        }
    }
    pub fn is_key(self) -> bool {
        !self.is_index()
    }

    pub fn description(self, table: &SymbolTable) -> String {
        match self {
            Symbol::Index(ix) => ix.to_string(),
            Symbol::Key(key) => table.description(key).to_string(),
            Symbol::Private(key) => format!("#{}", table.description(key)),
        }
    }
}

unsafe impl Trace for Symbol {}
unsafe impl Finalize for Symbol {}
impl Managed for Symbol {}

pub const DUMMY_SYMBOL: Symbol = Symbol::Key(SymbolID(0));

use dashmap::DashMap;
use std::sync::atomic::AtomicU32;
use std::sync::atomic::Ordering;
pub struct SymbolTable {
    pub(crate) symbols: DashMap<&'static str, u32>,
    pub(crate) ids: DashMap<u32, &'static str>,
    key: AtomicU32,
}
impl Drop for SymbolTable {
    fn drop(&mut self) {
        for entry in self.ids.iter_mut() {
            let key = entry.value();
            unsafe {
                let _ = Box::from_raw((*key) as *const _ as *mut str);
            }
        }
        self.symbols.clear();
        self.ids.clear();
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}
impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: DashMap::with_capacity(0),
            ids: DashMap::with_capacity(0),
            key: AtomicU32::new(128),
        }
    }

    pub fn description(&self, symbol: SymbolID) -> &'static str {
        *self.ids.get(&symbol.0).unwrap()
    }
    pub fn intern(&self, val: impl AsRef<str>) -> SymbolID {
        let string = val.as_ref();
        if let Some(key) = self.symbols.get(string) {
            return SymbolID(*key.value());
        }

        let string = Box::leak(string.to_string().into_boxed_str());
        let make_new_key = || self.key.fetch_add(1, Ordering::Relaxed);
        let key = *self
            .symbols
            .entry(string)
            .or_insert_with(make_new_key)
            .value();
        self.ids.insert(key, string);
        SymbolID(key)
    }
}

#[no_mangle]
#[doc(hidden)]
pub static mut SYMBOL_TABLE: std::mem::MaybeUninit<SymbolTable> = std::mem::MaybeUninit::uninit();
static mut LENGTH: Symbol = Symbol::Key(SymbolID(0));

macro_rules! globals {
    ($($id: ident $val: ident $ix: expr),*) => {
       $( pub static $id: &'static str = stringify!($val);)*
    };
}
builtin_symbols!(globals);
macro_rules! intern_builtins {
    ($($id: ident $val: ident $ix: expr),*) => {
        let mut _symtab = symbol_table();
        $(
            _symtab.ids.insert($ix,$id);
            _symtab.symbols.insert($id,$ix);
        )*
    };
}
static INIT: AtomicBool = AtomicBool::new(false);

pub fn initialize_symbol_table() {
    if INIT.compare_exchange(false, true, Ordering::AcqRel, Ordering::Relaxed) == Ok(false) {
        unsafe {
            SYMBOL_TABLE.as_mut_ptr().write(SymbolTable::new());
            LENGTH = "length".intern();
        }
        builtin_symbols!(intern_builtins);
    }
}

pub fn length_id() -> Symbol {
    unsafe { LENGTH }
}
pub fn symbol_table() -> &'static SymbolTable {
    unsafe { &*SYMBOL_TABLE.as_ptr() }
}
pub trait Internable {
    fn intern(&self) -> Symbol;
}

impl Internable for str {
    fn intern(&self) -> Symbol {
        Symbol::Key(symbol_table().intern(self))
    }
}

impl Internable for &str {
    fn intern(&self) -> Symbol {
        (*self).intern()
    }
}

impl Internable for String {
    fn intern(&self) -> Symbol {
        Symbol::Key(symbol_table().intern(self))
    }
}

impl Internable for u32 {
    fn intern(&self) -> Symbol {
        Symbol::Index(*self)
    }
}

impl Internable for usize {
    fn intern(&self) -> Symbol {
        if *self as u32 as usize == *self {
            return (*self as u32).intern();
        }
        self.to_string().intern()
    }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self {
        s.intern()
    }
}
impl From<&str> for Symbol {
    fn from(s: &str) -> Self {
        s.intern()
    }
}

pub fn catch_trap<R>(vm: &mut VM, clos: impl FnOnce(&mut VM) -> R) -> Result<R, Value> {
    let result = std::panic::catch_unwind(AssertUnwindSafe(|| clos(vm)));
    match result {
        Ok(r) => Ok(r),
        Err(x) if x.is::<VMTrap>() => {
            let error = vm.vthis;

            Err(error)
        }
        Err(e) => std::panic::resume_unwind(e),
    }
}
