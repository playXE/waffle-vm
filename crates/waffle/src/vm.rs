use crate::{
    gc_frame,
    memory::{
        gcwrapper::{Array, GCWrapper, Gc, Nullable},
        minimark::read_uint_from_env,
        Allocation, Finalize, Object, Trace, Visitor,
    },
    opcode::Op,
    value::{value_cmp, value_hash, Function, Module, Obj, Sym, Upvalue, Value, INVALID_CMP},
};
use std::{
    hash::{Hash, Hasher},
    mem::size_of,
    panic::{self, catch_unwind, resume_unwind, AssertUnwindSafe},
    ptr::null_mut,
};

pub struct VM {
    gc: GCWrapper,

    sp: *mut Value,
    csp: *mut Value,
    pub(crate) vthis: Value,
    env: Nullable<Array<Nullable<Upvalue>>>,

    spmin: *mut Value,
    spmax: *mut Value,
    trap: isize,
    exc_stack: Value,

    callback_ret: Nullable<Function>,
    callback_mod: Nullable<Module>,
    trace_hook: u32,

    pub(crate) builtins: Value,
    symtab: Box<[Option<Gc<Sym>>]>,
    ids: [Option<Gc<Sym>>; Id::Last as u8 as usize],
    open_upvalues: Nullable<Upvalue>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug, Hash)]
#[repr(u8)]
pub enum Id {
    Loader,
    Exports,
    Cache,
    Path,
    Libs,
    Last,
}

static IDS: &'static [&'static str] = &["loader", "exports", "cache", "path", "libs"];

unsafe impl Trace for VM {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        unsafe {
            let mut cursor = self.sp;
            let end = self.spmax;

            while cursor < end {
                (&mut *cursor).trace(vis);
                cursor = cursor.add(1);
            }

            let mut cursor = self.spmin;
            let end = self.csp;

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
            for sym in self.symtab.iter_mut() {
                sym.trace(vis);
            }
            for id in self.ids.iter_mut() {
                id.trace(vis);
            }

            self.open_upvalues.trace(vis);
        }
    }
}

impl VM {
    pub const STACK_SIZE: usize = 256;

    pub fn intern(&mut self, name: impl AsRef<str>) -> Gc<Sym> {
        let name = name.as_ref();
        let mut hasher = FxHasher64::default();
        name.hash(&mut hasher);
        let hash = hasher.finish();
        let pos = (hash % self.symtab.len() as u64) as usize;
        let mut node = self.symtab[pos];
        while let Some(sym) = node {
            if &**sym.name == name {
                return sym;
            }
            node = sym.next;
        }

        let str = self.gc().str(name);
        let mut sym = self.gc().fixed(Sym {
            next: None,
            name: str,
        });
        sym.next = self.symtab[pos];
        self.symtab[pos] = Some(sym);

        sym
    }

    pub fn new(stack_size: Option<usize>) -> &'static mut VM {
        let symtab_size = read_uint_from_env("WAFFLE_SYMTAB_SIZE").unwrap_or_else(|| 128);
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
            symtab: vec![None; symtab_size].into_boxed_slice(),
            ids: [None; Id::Last as usize],
            open_upvalues: Nullable::NULL,
        }));

        let stack_size = stack_size.unwrap_or_else(|| VM::STACK_SIZE);

        unsafe {
            let mem = libc::malloc(stack_size * size_of::<Value>()).cast::<Value>();
            core::ptr::copy_nonoverlapping(&Value::Null, mem, stack_size);
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
            let s = this.intern(id);
            this.ids[i] = Some(s);
        }
        super::builtin::init_builtin(&mut this);
        this.gc().collect(0, &mut []);
        this
    }

    pub fn id(&mut self, id: Id) -> Gc<Sym> {
        self.ids[id as usize].unwrap()
    }

    pub fn gc(&mut self) -> &mut GCWrapper {
        &mut self.gc
    }

    pub fn identity_cmp<T: Object + ?Sized>(&mut self, a: Gc<T>, b: Gc<T>) -> i64 {
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
                acc.set(interp_loop(self, module.get(), acc.get(), ip));
            })) {
                // if no error thrown just return accumulator register
                Ok(_) => return acc.get(),
                // VM trap occured
                Err(x) if x.is::<VMTrap>() => {
                    acc.set(self.vthis);

                    if self.trap == 0 || self.trap <= init_sp as isize {
                        // uncaught or outside init stack, reraise
                        panic::resume_unwind(Box::new(VMTrap));
                    }

                    trap = self.spmax.sub(self.trap as _);
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
                        Value::Null => Nullable::NULL,
                        Value::Abstract(x) => {
                            x.downcast::<Array<Nullable<Upvalue>>>().unwrap().nullable()
                        }
                        _ => unreachable!(),
                    };

                    ip = trap.add(3).read().int() as usize;
                    // if thrown from native code `module` is allowed to be null
                    module.set(match trap.add(4).read() {
                        Value::Module(x) => x.nullable(),
                        Value::Null => Nullable::NULL,
                        _ => unreachable!(),
                    });
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

        if !matches!(vthis.get(), Value::Null) {
            self.vthis = vthis.get();
        }
        loop {
            self.setup_trap();
            let result = panic::catch_unwind(AssertUnwindSafe(|| match f.get() {
                Value::Primitive(x) => {
                    self.env = x.env;
                    if args.len() as u32 != x.nargs && !x.varsize {
                        self.throw_str(&format!(
                            "argument count does not match: {} != {}",
                            args.len(),
                            x.nargs
                        ));
                    }
                    ret =
                        dispatch_func2(self, args.as_ptr() as _, x.addr, args.len() as _, x.varsize)
                }
                Value::Function(x) => {
                    if args.len() == x.nargs as usize {
                        if self.csp.add(4) >= self.sp.sub(args.len()) {
                            if exc.is_some() {
                                self.process_trap();
                            }
                            self.throw_str("stack overflow");
                        }
                    } else {
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
                        self.csp.write(Value::Module(self.callback_mod.as_gc()));

                        ret = self.interp(x.module, Value::Null, x.addr);
                    }
                }
                _ => {
                    self.throw_str("Invalid call");
                }
            }));

            match result {
                Ok(_) => break,
                Err(x) => {
                    if let Some(_) = x.downcast_ref::<VMTrap>() {
                        *exc = Some(self.vthis);

                        self.process_trap();
                        self.vthis = old_this.get();
                        self.env = old_env.get();

                        return Value::Null;
                    } else {
                        resume_unwind(x);
                    }
                }
            }
        }

        self.vthis = old_this.get();
        self.env = old_env.get();
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
            Value::Null => Nullable::NULL,
            Value::Abstract(x) => x.downcast::<Array<Nullable<Upvalue>>>().unwrap().nullable(),
            _ => unreachable!(),
        };

        sp = trap.add(6);
        self.trap = trap.add(5).read().int() as _;
        while self.sp < sp {
            self.sp.write(Value::Null);
            self.sp = self.sp.add(1);
        }
    }
    unsafe fn close_upvalues(&mut self, slot: *mut Value) {
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
        self.vthis = old_vthis.get();
        self.env = old_env.get();

        ret
    }
}
use fxhash::FxHasher64;
use libloading_mini::Library;
use std::mem::transmute;
use Op::*;
#[inline(never)]
unsafe fn dispatch_func(
    vm: &mut VM,
    mut sp: *mut Value,
    f: usize,
    nargs: u32,
    varsize: bool,
) -> Value {
    if varsize {
        let f = transmute::<_, fn(&mut VM, &[Value]) -> Value>(f);
        let mut args = vm.gc().array(nargs as _, Value::Null);
        let mut i = nargs as usize;
        sp = sp.add(nargs as _);
        let mut j = 0;
        while i > 0 {
            sp = sp.sub(1);
            args[j] = sp.read();
            j += 1;
            i -= 1;
        }

        vm.gc().write_barrier(args);
        return f(vm, &args);
    } else {
        match nargs {
            0 => (transmute::<_, extern "C" fn(&mut VM) -> Value>(f))(vm),
            1 => (transmute::<_, extern "C" fn(&mut VM, &Value) -> Value>(f))(vm, &*sp),
            2 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value) -> Value>(f))(
                vm,
                &*sp.add(1),
                &*sp,
            ),
            3 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value, &Value) -> Value>(f))(
                vm,
                &*sp.add(2),
                &*sp.add(1),
                &*sp,
            ),
            4 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value, &Value, &Value) -> Value>(
                f,
            ))(vm, &*sp.add(3), &*sp.add(2), &*sp.add(1), &*sp),
            5 => (transmute::<
                _,
                extern "C" fn(&mut VM, &Value, &Value, &Value, &Value, &Value) -> Value,
            >(f))(vm, &*sp.add(4), &*sp.add(3), &*sp.add(2), &*sp.add(1), &*sp),

            _ => todo!(),
        }
    }
}

#[inline(never)]
unsafe fn dispatch_func2(
    vm: &mut VM,
    mut sp: *mut Value,
    f: usize,
    nargs: u32,
    varsize: bool,
) -> Value {
    if varsize {
        let f = transmute::<_, fn(&mut VM, &[Value]) -> Value>(f);
        let mut args = vm.gc().array(nargs as _, Value::Null);

        sp = sp.add(nargs as _);
        for i in 0..nargs as usize {
            args[i] = sp.add(i).read();
        }

        vm.gc().write_barrier(args);
        return f(vm, &args);
    } else {
        match nargs {
            0 => (transmute::<_, extern "C" fn(&mut VM) -> Value>(f))(vm),
            1 => (transmute::<_, extern "C" fn(&mut VM, &Value) -> Value>(f))(vm, &*sp),
            2 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value) -> Value>(f))(
                vm,
                &*sp,
                &*sp.add(1),
            ),
            3 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value, &Value) -> Value>(f))(
                vm,
                &*sp,
                &*sp.add(1),
                &*sp.add(2),
            ),
            4 => (transmute::<_, extern "C" fn(&mut VM, &Value, &Value, &Value, &Value) -> Value>(
                f,
            ))(vm, &*sp.add(1), &*sp.add(2), &*sp.add(3), &*sp),
            5 => (transmute::<
                _,
                extern "C" fn(&mut VM, &Value, &Value, &Value, &Value, &Value) -> Value,
            >(f))(vm, &*sp, &*sp.add(1), &*sp.add(2), &*sp.add(3), &*sp.add(4)),

            _ => todo!(),
        }
    }
}

fn interp_loop(vm: &mut VM, mut m: Nullable<Module>, mut acc: Value, mut ip: usize) -> Value {
    gc_frame!(vm.gc().roots() => m: Nullable<Module>,acc: Value);
    let mut sp = vm.sp;
    let mut csp = vm.csp;

    macro_rules! pop {
        ($n: expr) => {{
            let mut tmp = $n;
            while tmp > 0 {
                sp.write(Value::Null);
                sp = sp.add(1);
                tmp -= 1;
            }
        }};
    }

    macro_rules! pop_infos {
        ($restpc: expr) => {{
            m.set(csp.read().module());
            csp.write(Value::Null);
            csp = csp.sub(1);
            vm.vthis = csp.read();
            csp.write(Value::Null);
            csp = csp.sub(1);

            vm.env = match csp.read() {
                Value::Null => Nullable::NULL,
                Value::Abstract(x) => x.downcast::<Array<Nullable<Upvalue>>>().unwrap().nullable(),
                _ => unreachable!(),
            };
            csp.write(Value::Null);
            csp = csp.sub(1);

            if $restpc {
                ip = csp.read().int() as usize;
            }
            csp.write(Value::Null);
            csp = csp.sub(1);
        }};
    }

    macro_rules! push_infos {
        () => {
            csp = csp.add(1);
            csp.write(Value::Int(ip as _));

            csp = csp.add(1);
            csp.write(if vm.env.is_not_null() {
                Value::Abstract(vm.env.as_gc().as_dyn())
            } else {
                Value::Null
            });

            csp = csp.add(1);
            csp.write(vm.vthis);

            csp = csp.add(1);
            csp.write(if m.get().is_null() {
                Value::Null
            } else {
                Value::Module(m.get().as_gc())
            });
        };
    }

    macro_rules! save {
        () => {
            vm.csp = csp;
            vm.sp = sp;
        };
    }

    macro_rules! restore {
        () => {
            sp = vm.sp;
            csp = vm.csp;
        };
    }

    macro_rules! object_op {
        ($obj: expr,$param: expr,$id: expr,$err: expr) => {
            let id = Value::Symbol(vm.intern(stringify!($id)));
            let f = $obj.field(vm, &id);
            match f {
                Value::Null => $err,
                _ => {
                    push_infos!();
                    save!();
                    let mut p = [*$param];
                    gc_frame!(vm.gc().roots() => p: [Value;1]);
                    acc.set(vm.callex(Value::Object(*$obj), f, p.as_ref(),&mut None));
                    restore!();
                    pop_infos!(false);
                }
            }
        };
        ($obj: expr,$params: expr,$id: expr) => {
            object_op!($obj, $params, $id, vm.throw_str("Unsupported operation"))
        };
    }

    macro_rules! setup_before_call {
        ($this: expr) => {
            let f = acc.get().prim_or_func();
            push_infos!();
            vm.vthis = $this;
            vm.env = f.env;
            save!();
        };
    }

    macro_rules! restore_after_call {
        () => {
            restore!();
            pop_infos!(false);
        };
    }
    macro_rules! do_call {
        ($cur_this: expr,$nargs: expr) => {
            match acc.get() {
                Value::Function(func) => {
                    push_infos!();
                    m.set(func.module);
                    ip = func.addr as _;
                    vm.vthis = $cur_this;
                    vm.env = func.env;
                    // store number of arguments in accumulator register
                    acc.set(Value::Int($nargs as _));
                }
                Value::Primitive(prim) => {
                    if $nargs as u32 != prim.nargs && !prim.varsize {
                        vm.throw_str(&format!(
                            "argument count does not match: {} != {}",
                            $nargs, prim.nargs
                        ));
                    }
                    setup_before_call!($cur_this);

                    acc.set(dispatch_func(vm, sp, prim.addr, $nargs as _, prim.varsize));
                    restore_after_call!();
                    pop!($nargs);
                }
                _ => vm.throw_str(&format!("call failure: {}", acc.get())),
            }
        };
    }
    macro_rules! test {
        ($op: tt) => {{
            save!();
            let tmp = crate::value::value_cmp(vm, &*sp, acc.as_ref());
            restore!();
            sp.write(Value::Null);
            sp = sp.add(1);
            match tmp {
                Value::Int(x) => {
                    acc.set(Value::Bool(x $op 0 && x != INVALID_CMP as i64));
                }
                _ => unreachable!()
            }
        }};
    }

    macro_rules! intop {
        ($op: tt) => {{
            match (acc.get(),sp.read()) {
                (Value::Int(x),Value::Int(y)) => { acc.set(Value::Int(x $op y)); },
                _ => vm.throw_str(concat!("incompatible types for '", stringify!($op), "'"))
            }
            sp.write(Value::Null);
            sp = sp.add(1);
        }}
    }
    loop {
        unsafe {
            let op = m.get()[ip];
            let op = std::mem::transmute::<_, Op>(op);
            println!("{} {:?}", ip, op);
            ip += 1;
            match op {
                AccNull => {
                    acc.set(Value::Null);
                }
                AccTrue => {
                    acc.set(Value::Bool(true));
                }
                AccFalse => {
                    acc.set(Value::Bool(false));
                }

                AccThis => {
                    acc.set(vm.vthis);
                }
                AccInt(int) => {
                    acc.set(Value::Int(int as i64));
                }
                AccStack(ix) => {
                    acc.set(sp.offset(ix as _).read());
                }
                AccGlobal(ix) => {
                    println!("{:p} {} {}", m.as_gc(), ix, 0);
                    acc.set(m.globals[ix as usize]);
                }
                AccField(ix) => {
                    let mut name = m.globals[ix as usize];
                    let object = acc.get();

                    match object {
                        Value::Object(mut object) => {
                            gc_frame!(vm.gc().roots() => object: Gc<Obj>,name: Value);
                            let tmp = object.field(vm, &name);

                            *acc = tmp;
                        }

                        _ => vm.throw_str("invalid field access"),
                    }
                }
                AccArray => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Array(arr)) => {
                        let k = x;
                        if k < 0 || k >= arr.len() as i64 {
                            acc.set(Value::Null);
                        } else {
                            acc.set(arr[k as usize]);
                        }

                        sp = sp.add(1);
                    }

                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __get);
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    }
                    _ => vm.throw_str("invalid array access"),
                },
                AccIndex(ix) => match acc.get() {
                    Value::Array(arr) => {
                        if ix < 0 || ix >= arr.len() as i32 {
                            acc.set(Value::Null);
                        } else {
                            acc.set(arr[ix as usize]);
                        }
                    }
                    Value::Object(mut obj) => {
                        gc_frame!(vm.gc.roots() => obj: Gc<Obj>);
                        object_op!(obj.as_mut(), acc.as_ref(), __get);
                    }
                    _ => vm.throw_str("invalid array access"),
                },
                AccBuiltinResolved(val) => {
                    acc.set(val);
                }
                AccBuiltin(ix) => {
                    let f = &m.globals[ix as usize];
                    match vm.builtins {
                        Value::Object(obj) => {
                            let field = obj.field(vm, f);
                            *acc = field;
                            // patch code so next builtin load is O(1)
                            m[ip - 1] = Op::AccBuiltinResolved(field);
                        }
                        _ => unreachable!(),
                    }
                }

                AccEnv(ix) => {
                    let upval = vm.env[ix as usize];
                    if upval.closed {
                        acc.set(upval.state.local);
                    } else {
                        acc.set(upval.state.slot.read());
                    }
                }
                Push => {
                    sp = sp.sub(1);
                    sp.write(acc.get());
                }
                Pop(n) => {
                    pop!(n);
                }
                SetStack(ix) => {
                    sp.offset(ix as _).write(acc.get());
                }
                SetField(ix) => {
                    let field = &m.globals[ix as usize];
                    match &mut *sp {
                        Value::Object(object) => object.insert(vm, field, acc.as_ref()),
                        _ => vm.throw_str("invalid field access"),
                    };

                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                SetArray => {
                    match (&mut *sp, &*sp.add(1)) {
                        (Value::Array(arr), Value::Int(k)) => {
                            let k = *k;
                            if k < arr.len() as i64 && k >= 0 {
                                arr[k as usize] = acc.get();
                                vm.gc().write_barrier(*arr);
                            }
                        }
                        (Value::Object(obj), arg0) => {
                            let string = vm.intern("__set");
                            let mut f = obj.field(vm, &Value::Symbol(string));
                            if matches!(f, Value::Null) {
                                vm.throw_str("unsupported operation");
                            }
                            let mut args = [*arg0, acc.get()];
                            gc_frame!(vm.gc().roots() => f: Value,args: [Value;2]);
                            push_infos!();
                            save!();
                            vm.callex(sp.read(), f.get(), args.as_ref(), &mut None);
                            restore!();
                            pop_infos!(false);
                        }
                        _ => vm.throw_str("invalid array access"),
                    }
                    sp.write(Value::Null);
                    sp = sp.add(1);
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                SetIndex(ix) => {
                    match &mut *sp {
                        Value::Array(arr) => {
                            if ix >= 0 && ix < arr.len() as i32 {
                                arr[ix as usize] = acc.get();
                                vm.gc().write_barrier(*arr);
                            }
                        }
                        Value::Object(obj) => {
                            let string = vm.gc().str("__set");
                            let mut f = obj.field(vm, &Value::Str(string));
                            if matches!(f, Value::Null) {
                                vm.throw_str("unsupported operation");
                            }
                            let mut args = [Value::Int(ix as _), acc.get()];
                            gc_frame!(vm.gc().roots() => f: Value,args: [Value;2]);
                            push_infos!();
                            save!();
                            vm.callex(sp.read(), f.get(), args.as_ref(), &mut None);
                            restore!();
                            pop_infos!(false);
                        }
                        _ => vm.throw_str("Invalid array access"),
                    }
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }

                SetEnv(ix) => {
                    /*vm.env.array()[ix as usize] = acc.get();
                    let arr = vm.env.array();
                    vm.gc().write_barrier(arr);*/
                    let mut upval = vm.env[ix as usize];
                    if upval.closed {
                        upval.state.local = acc.get();
                    } else {
                        upval.state.slot.write(acc.get());
                    }
                    vm.gc().write_barrier(upval.as_gc());
                }
                SetGlobal(ix) => {
                    let mut globals = m.globals;

                    globals[ix as usize] = acc.get();
                    vm.gc().write_barrier(globals.as_gc());
                }

                SetThis => {
                    vm.vthis = acc.get();
                }

                TailCall(mut stack, nargs) => {
                    let mut i = nargs as i32;
                    let cur_this = vm.vthis;
                    stack -= nargs;
                    sp = sp.add(nargs as _);
                    while i > 0 {
                        sp = sp.sub(1);
                        sp.add(stack as _).write(sp.read());
                        i -= 1;
                    }

                    while stack > 0 {
                        stack -= 1;
                        vm.close_upvalues(sp);
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    }

                    pop_infos!(true);
                    do_call!(cur_this, nargs);
                }
                Call(nargs) => {
                    do_call!(vm.vthis, nargs);
                }

                ObjCall(nargs) => {
                    let vtmp = sp.read();
                    sp.write(Value::Null);
                    sp = sp.add(1);
                    do_call!(vtmp, nargs);
                }

                Jump(ix) => {
                    ip -= 1;
                    ip = (ip as isize + ix as isize) as usize;
                }
                JumpIf(ix) => {
                    if acc.get().bool() {
                        ip -= 1;
                        ip = (ip as isize + ix as isize) as usize;
                    }
                }
                JumpIfNot(ix) => {
                    if !acc.get().bool() {
                        ip -= 1;
                        ip = (ip as isize + ix as isize) as usize;
                    }
                }

                Trap(ix) => {
                    sp = sp.sub(6);
                    sp.write(Value::Int(vm.csp as i64 - vm.spmin as i64));
                    sp.add(1).write(vm.vthis);
                    sp.add(2).write(if vm.env.is_not_null() {
                        Value::Abstract(vm.env.as_gc().as_dyn())
                    } else {
                        Value::Null
                    });
                    sp.add(3).write(Value::Int(ix as _));
                    sp.add(4).write(if m.is_null() {
                        Value::Null
                    } else {
                        Value::Module(m.as_gc())
                    });
                    sp.add(5).write(Value::Int(vm.trap as _));
                    vm.trap = vm.spmax as isize - sp as isize;
                }
                EndTrap => {
                    vm.trap = sp.add(5).read().int() as _;
                    pop!(6);
                }
                Ret(nargs) => {
                    for i in 0..nargs {
                        vm.close_upvalues(sp.add(i as _));
                    }
                    pop!(nargs);
                    pop_infos!(true);
                }
                CloseUpvalue => {
                    vm.close_upvalues(sp);
                    pop!(1);
                }
                MakeEnv(n) => {
                    let mut func = acc.get().prim_or_func();
                    let mut upvals = func.module.globals[n as usize].array();
                    let mut real_upvals = vm.gc().array(upvals.len(), Nullable::NULL);
                    gc_frame!(vm.gc().roots() => func: Gc<Function>, upvals: Gc<Array<Value>>, real_upvals: Gc<Array<Nullable<Upvalue>>>);

                    for i in 0..upvals.len() {
                        let [is_local, index] = std::mem::transmute::<_, [i32; 2]>(upvals[i].int());
                        if is_local == 0 {
                            real_upvals[i] = vm.env[index as usize];
                        } else {
                            let slot = sp.add(index as usize);
                            //    println!("upval at {}: {}", index, sp.read());
                            let upval = {
                                let mut upval = Nullable::NULL;
                                let mut open = vm.open_upvalues;
                                while open.is_not_null() {
                                    if open.state.slot == slot {
                                        upval = open;
                                    }
                                    open = open.next;
                                }
                                if upval.is_null() {
                                    let tmp = vm.open_upvalues;
                                    upval = vm
                                        .gc()
                                        .fixed(Upvalue {
                                            next: tmp,
                                            closed: false,
                                            state: crate::value::UpvalState { slot },
                                        })
                                        .nullable();
                                    vm.open_upvalues = upval;
                                }
                                upval
                            };
                            real_upvals[i] = upval;
                        }
                        vm.gc().write_barrier(*real_upvals);
                    }
                    acc.set(Value::Function(vm.gc().fixed(Function {
                        module: func.module,
                        env: real_upvals.nullable(),
                        addr: func.addr,
                        nargs: func.nargs,
                        varsize: func.varsize,
                    })));
                }

                MakeArray(mut n) => {
                    let mut arr = vm.gc().array(n as usize + 1, Value::Null);
                    while n != 0 {
                        arr[n as usize] = sp.read();
                        sp.write(Value::Null);
                        sp = sp.add(1);
                        n -= 1;
                    }
                    arr[0] = acc.get();
                    vm.gc().write_barrier(arr);
                    acc.set(Value::Array(arr));
                }

                Bool => {
                    let v = acc.get();
                    if matches!(v, Value::Bool(true)) || matches!(v, Value::Bool(false)) {
                        acc.set(Value::Bool(false));
                    } else {
                        acc.set(Value::Bool(true));
                    }
                }

                Not => {
                    let v = acc.get();
                    if matches!(v, Value::Bool(false) | Value::Null) {
                        acc.set(Value::Bool(true));
                    } else {
                        acc.set(Value::Bool(false));
                    }
                }
                Add => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_add(x)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let y = y.get_number();
                        let x = x.get_number();
                        acc.set(Value::Float(x + y));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __add);
                        pop!(1);
                    }
                    _ => vm.throw_str(&format!("wrong operands for +: {} {}", acc.get(), *sp)),
                },
                Sub => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_sub(x)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let y = y.get_number();
                        let x = x.get_number();
                        acc.set(Value::Float(x - y));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __sub);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for -"),
                },

                Div => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_div(x)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number();
                        let y = y.get_number();
                        acc.set(Value::Float(y / x));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __div);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for /"),
                },
                Mul => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_mul(x)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number();
                        let y = y.get_number();
                        acc.set(Value::Float(y * x));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __mul);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for *"),
                },
                Mod => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_rem(x)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number();
                        let y = y.get_number();
                        acc.set(Value::Float(y % x));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __mod);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for *"),
                },
                Shl => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_shl(x as _)));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number() as i64;
                        let y = y.get_number() as i64;
                        acc.set(Value::Int(y.wrapping_shl(x as _)));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __shl);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for <<"),
                },
                Shr => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int(y.wrapping_shr(x as _)));
                        pop!(1);
                    }
                    (y, x) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number() as i64;
                        let y = y.get_number() as i64;
                        acc.set(Value::Int(y.wrapping_shr(x as _)));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __shr);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for <<"),
                },
                UShr => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Int(y)) => {
                        acc.set(Value::Int((x as u64).wrapping_shr(*y as _) as i64));
                        pop!(1);
                    }
                    (x, y) if x.is_numeric() && y.is_numeric() => {
                        let x = x.get_number() as u32;
                        let y = y.get_number() as u64;
                        acc.set(Value::Int(y.wrapping_shr(x) as _));
                        pop!(1);
                    }
                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __ushr);
                        pop!(1);
                    }
                    _ => vm.throw_str("wrong operands for <<"),
                },
                Eq => test!(==),
                Neq => test!(!=),
                Gt => test!(>),
                Gte => test!(>=),
                Lt => test!(<),
                Lte => test!(<=),
                Compare => {
                    save!();
                    let c = value_cmp(vm, &*sp, &*acc);
                    restore!();
                    acc.set(if matches!(c, Value::Int(INVALID_CMP)) {
                        Value::Null
                    } else {
                        c
                    });
                }
                IsNull => {
                    acc.set(Value::Bool(matches!(acc.get(), Value::Null)));
                }
                IsNotNull => {
                    acc.set(Value::Bool(!matches!(acc.get(), Value::Null)));
                }
                Or => intop!(|),
                And => intop!(&),
                Xor => intop!(^),
                TypeOf => {
                    let tag = acc.tag();
                    acc.set(WAFFLE_TYPEOF[tag as usize]);
                }
                New => {
                    save!();
                    acc.set(match acc.get() {
                        Value::Object(x) => Value::Object(Obj::with_proto(vm, x)),
                        _ => Value::Object(Obj::new(vm)),
                    });
                }
                Hash => {
                    save!();
                    let hash = value_hash(vm, &*acc);
                    acc.set(hash);
                }
                Apply(_) => todo!(),
                JumpTable(_) => todo!(),

                Leave | Last => break,
                x => todo!("{:?}", x),
            }
        }
    }

    vm.sp = sp;
    vm.csp = csp;
    acc.get()
}

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
    const LIGHT_FINALIZER: bool = false;
}
impl Object for LibList {}

pub const WAFFLE_TYPEOF: [Value; 13] = [
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
];
