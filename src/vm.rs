use crate::{
    gc_frame,
    memory::{
        gcwrapper::{GCWrapper, Gc, Nullable},
        Object, Trace, Visitor,
    },
    opcode::Op,
    value::{Module, Value, INVALID_CMP},
};
use std::panic::{self, AssertUnwindSafe};

pub struct VM {
    gc: GCWrapper,

    sp: *mut Value,
    csp: *mut Value,
    vthis: Value,
    env: Value,

    spmin: *mut Value,
    spmax: *mut Value,
    trap: isize,
    exc_stack: Value,
}

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
        }
    }
}

impl VM {
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

    pub fn interp(&mut self, mut module: Nullable<Module>, mut acc: Value, mut ip: usize) -> Value {
        let init_sp = (self.spmax as usize - self.sp as usize) as *mut Value;
        let mut csp;
        let mut sp;
        let mut trap;
        gc_frame!(self.gc().roots() => acc, module);
        loop {
            match panic::catch_unwind(AssertUnwindSafe(|| {
                // Execute code
                acc.set(interp_loop(self, module.get().as_gc(), acc.get(), ip));
            })) {
                // if no error thrown just return accumulator register
                Ok(_) => return acc.get(),
                // VM trap occured
                Err(x) if x.is::<VMTrap>() => unsafe {
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
                    self.env = trap.add(2).read();

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
                },
                Err(x) => panic::resume_unwind(x),
            }
        }
    }

    pub fn throw_str(&mut self, x: impl AsRef<str>) -> ! {
        let x = Value::Str(self.gc().str(x.as_ref()));
        self.throw(x)
    }

    pub fn callex(&mut self, mut vthis: Value, mut f: Value, args: &[Value]) -> Value {
        let mut old_this = self.vthis;
        let mut old_env = self.env;
        gc_frame!(self.gc().roots() => old_this,old_env,vthis,f);
        let ret = Value::Null;
        match f.get() {
            Value::Primitive(x) => {
                todo!()
            }
            Value::Function(x) => {
                todo!()
            }
            _ => {
                self.throw_str("Invalid call");
            }
        }

        self.vthis = old_this.get();
        self.env = old_env.get();
        ret
    }
}
use Op::*;
pub fn interp_loop(vm: &mut VM, mut m: Gc<Module>, mut acc: Value, mut ip: usize) -> Value {
    gc_frame!(vm.gc().roots() => m,acc);
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

            vm.env = csp.read();
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
            csp.write(Value::Int(ip as _));
            csp = csp.add(1);
            csp.write(vm.env);
            csp = csp.add(1);
            csp.write(vm.vthis);
            csp = csp.add(1);
            csp.write(Value::Module(m.get()));
            csp = csp.add(1);
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
            let id = Value::Str(vm.gc().str(stringify!($id)));
            let f = $obj.field(vm, &id);
            match f {
                Value::Null => $err,
                _ => {
                    push_infos!();
                    save!();
                    let mut p = [*$param];
                    gc_frame!(vm.gc().roots() => p);
                    acc.set(vm.callex(Value::Object(*$obj), f, p.as_ref()));
                    restore!();
                    pop_infos!(false);
                }
            }
        };
        ($obj: expr,$params: expr,$id: expr) => {
            object_op!($obj, $params, $id, vm.throw_str("Unsupported operation"))
        };
    }

    loop {
        unsafe {
            let op = m.get()[ip];
            let op = std::mem::transmute::<_, Op>(op);
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
                AccInt => {
                    let m = m.get();
                    let x = [
                        m[0],
                        m[ip + 1],
                        m[ip + 2],
                        m[ip + 3],
                        m[ip + 4],
                        m[ip + 5],
                        m[ip + 6],
                        m[ip + 7],
                    ];
                    ip += 8;
                    let i = i64::from_le_bytes(x);
                    acc.set(Value::Int(i));
                }
                AccStack => {
                    let m = m.get();
                    let ix = i16::from_le_bytes([m[0], m[1]]);
                    ip += 2;
                    acc.set(sp.offset(ix as _).read());
                }
                AccGlobal => {
                    let m = m.get();
                    let ix = u32::from_le_bytes([m[ip], m[ip + 1], m[ip + 2], m[ip + 3]]);
                    ip += 4;
                    acc.set(m.globals[ix as usize]);
                }
                AccField => {
                    let m = m.get();
                    let ix = u32::from_le_bytes([m[ip], m[ip + 1], m[ip + 2], m[ip + 3]]);
                    ip += 4;

                    let mut name = m.globals[ix as usize];
                    let object = acc.get();
                    match object {
                        Value::Object(mut object) => {
                            gc_frame!(vm.gc().roots() => object,name);
                            acc.set(object.as_ref().table.lookup(vm, name.as_ref(), &mut false));
                        }

                        _ => vm.throw_str("invalid field access"),
                    }
                    acc.set(m.globals[ix as usize]);
                }
                AccArray => match (acc.get(), &mut *sp) {
                    (Value::Int(x), Value::Array(arr)) => {
                        let k = x;
                        if k < 0 || k >= arr.len() as i64 {
                            acc.set(Value::Null);
                        } else {
                            acc.set(arr[k as usize]);
                        }
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    }

                    (_, Value::Object(obj)) => {
                        object_op!(obj, acc.as_ref(), __get);
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    }
                    _ => vm.throw_str("invalid array access"),
                },
                AccIndex => {
                    let m = m.get();
                    let ix = i32::from_le_bytes([m[ip], m[ip + 1], m[ip + 2], m[ip + 3]]);
                    ip += 4;
                    match acc.get() {
                        Value::Array(arr) => {
                            if ix < 0 || ix >= arr.len() as i32 {
                                acc.set(Value::Null);
                            } else {
                                acc.set(arr[ix as usize]);
                            }
                        }
                        Value::Object(mut obj) => {
                            gc_frame!(vm.gc.roots() => obj);
                            object_op!(obj.as_mut(), acc.as_ref(), __get);
                        }
                        _ => vm.throw_str("invalid array access"),
                    }
                }
                AccBuiltin => {
                    todo!()
                }

                AccEnv => {
                    let m = m.get();
                    let ix = u16::from_le_bytes([m[ip], m[ip + 1]]);
                    ip += 2;

                    acc.set(vm.env.array()[ix as usize]);
                }
                Push => {
                    sp = sp.sub(1);
                    sp.write(acc.get());
                }
                Pop => {
                    let m = m.get();
                    let ix = u16::from_le_bytes([m[ip], m[ip + 1]]);
                    ip += 2;
                    pop!(ix);
                }
                SetStack => {
                    let m = m.get();
                    let ix = i16::from_le_bytes([m[0], m[1]]);
                    ip += 2;
                    sp.offset(ix as _).write(acc.get());
                }
                SetField => {
                    let m = m.get();
                    let ix = u32::from_le_bytes([m[ip], m[ip + 1], m[ip + 2], m[ip + 3]]);
                    ip += 4;
                    let field = &m.globals[ix as usize];
                    match &mut *sp {
                        Value::Object(object) => object.table.insert(vm, field, acc.as_ref()),
                        _ => vm.throw_str("invalid field access"),
                    };

                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                SetEnv => {
                    let m = m.get();
                    let ix = u16::from_le_bytes([m[ip], m[ip + 1]]);
                    ip += 2;
                    vm.env.array()[ix as usize] = acc.get();
                    let arr = vm.env.array();
                    vm.gc().write_barrier(arr);
                }
                SetGlobal => {
                    let m = m.get();
                    let ix = u32::from_le_bytes([m[ip], m[ip + 1], m[ip + 2], m[ip + 3]]);
                    ip += 4;
                    let mut globals = m.globals;
                    globals[ix as usize] = acc.get();
                    vm.gc().write_barrier(globals);
                }
                _ => todo!(),
            }
        }
    }

    vm.sp = sp;
    vm.csp = csp;
    acc.get()
}

pub struct VMTrap;
