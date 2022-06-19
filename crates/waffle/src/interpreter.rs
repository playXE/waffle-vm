use crate::opcode::*;
use crate::{
    gc_frame,
    memory::{gcwrapper::*, roots::*},
    object::*,
    runtime::array::ARRAY_CLASS,
    value::*,
    vm::*,
};
use std::mem::transmute;
use Op::*;
mod slow_paths;
#[inline(never)]
pub(crate) unsafe fn dispatch_func(
    vm: &mut VM,
    mut sp: *mut Value,
    f: usize,
    nargs: u32,
    varsize: bool,
) -> Value {
    if varsize {
        let f = transmute::<_, extern "C" fn(&mut VM, &Gc<Array<Value>>) -> Value>(f);
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
pub(crate) unsafe fn dispatch_func2(
    vm: &mut VM,
    mut sp: *mut Value,
    f: usize,
    nargs: u32,
    varsize: bool,
) -> Value {
    if varsize {
        let f = transmute::<_, extern "C" fn(&mut VM, &Gc<Array<Value>>) -> Value>(f);
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

pub(crate) fn check_arguments(vm: &mut VM, argc: usize, expected: usize, variable: bool) {
    if argc < expected && !variable {
        vm.throw_str(format!(
            "function expected at least {} arguments but found {}",
            expected, argc
        ));
    } else if argc > expected && !variable {
        vm.throw_str(format!(
            "function expected at most {} arguments but found {}",
            expected, argc
        ))
    }
}

pub(crate) fn interp_loop(
    vm: &mut VM,
    mut m: Nullable<Module>,
    mut acc: Value,
    mut ip: usize,
) -> Value {
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
            let ctor = csp.read().bool();
            csp.write(Value::Null);
            csp = csp.sub(1);
            /*m.set(csp.read().module());*/
            let m_ = csp.read().module();
            // println!("ASSIGN {:p} to {:p} ({:p})", m_, m, *m);
            m.set(m_);
            csp.write(Value::Null);
            csp = csp.sub(1);
            vm.vthis = csp.read();
            csp.write(Value::Null);
            csp = csp.sub(1);

            vm.env = match csp.read() {
                x if x.is_null() => Nullable::NULL,
                x => x
                    .downcast_ref::<Array<Nullable<Upvalue>>>()
                    .unwrap()
                    .nullable(),
            };
            csp.write(Value::Null);
            csp = csp.sub(1);

            if $restpc {
                ip = csp.read().int() as usize;
            }
            csp.write(Value::Null);
            csp = csp.sub(1);
            ctor
        }};
    }

    macro_rules! push_infos {
        ($ctor: expr) => {
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
            csp.write(if m.get_copy().is_null() {
                Value::Null
            } else {
                Value::encode_object_value(m.get_copy().as_gc())
            });

            csp = csp.add(1);
            csp.write(Value::Bool($ctor));
        };
        () => {
            push_infos!(false);
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
            let id = stringify!($id).intern();
            let f = $obj.get(vm, id);
            match f {
                x if !x.is_function() => $err,
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
            let f = acc.get_copy().prim_or_func();
            push_infos!();
            vm.vthis = $this;
            vm.env = f.env;
            save!();
        };
    }
    macro_rules! stack_valid {
        () => {
            debug_assert!(sp <= vm.spmax && sp > vm.csp && sp >= vm.spmin);
        };
    }
    macro_rules! restore_after_call {
        () => {
            restore!();
            pop_infos!(false);
        };
    }
    macro_rules! do_call {
        ($cur_this: expr,$nargs: expr, $ctor: expr) => {
            //    println!("CALL {} {}", acc.get_copy(), $nargs);
            match acc.get_copy() {
                x if x.is_function() => {
                    let func = x.prim_or_func();
                    check_arguments(vm, $nargs as _, func.nargs as _, func.varsize);
                    if !func.prim {
                        push_infos!($ctor);
                        m.set(func.module);
                        ip = func.addr as _;
                        vm.vthis = $cur_this;
                        vm.env = func.env;
                        // store number of arguments in accumulator register
                        acc.set(Value::Int($nargs as _));
                    } else {
                        let prim = func;

                        setup_before_call!($cur_this);

                        acc.set(dispatch_func(vm, sp, prim.addr, $nargs as _, prim.varsize));
                        restore_after_call!();
                        pop!($nargs);
                    }
                }
                _ => vm.throw_str(&format!("call failure: {}", acc.get_copy())),
            }
        };
    }
    macro_rules!  test {
        ($op: tt) => {{
            save!();
            let tmp = crate::value::value_cmp(vm, &*sp, acc.as_ref());
            restore!();
            sp.write(Value::Null);
            sp = sp.add(1);
            let x = tmp.get_int32();
            acc.set(Value::Bool(x $op 0 && x != INVALID_CMP));


        }};
    }

    macro_rules! intop {
        ($op: tt) => {{
            match (acc.get_copy(),sp.read()) {
                (x,y) if x.is_int32() && y.is_int32() => { acc.set(Value::Int(x.get_int32() $op y.get_int32())); },
                _ => vm.throw_str(concat!("incompatible types for '", stringify!($op), "'"))
            }
            sp.write(Value::Null);
            sp = sp.add(1);
        }}
    }
    'interp: loop {
        unsafe {
            let op = m.get_copy()[ip];
            let op = std::mem::transmute::<_, Op>(op);
            let mut stack = vec![];

            let mut tmp = sp;
            while tmp < vm.spmax {
                stack.push(tmp.read());
                tmp = tmp.add(1);
            }
            ip += 1;
            stack_valid!();
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
                    acc.set(Value::Int(int as i32));
                }
                AccStack(ix) => {
                    acc.set(sp.offset(ix as _).read());
                }
                AccGlobal(ix) => {
                    acc.set(*m.globals.get_unchecked(ix as usize));
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
                    sp.write(acc.get_copy());
                }
                Pop(n) => {
                    pop!(n);
                }
                SetStack(ix) => {
                    sp.offset(ix as _).write(acc.get_copy());
                }
                SetEnv(ix) => {
                    let mut upval = vm.env[ix as usize];
                    if upval.closed {
                        upval.state.local = acc.get_copy();
                    } else {
                        upval.state.slot.write(acc.get_copy());
                    }
                    vm.gc().write_barrier(upval.as_gc());
                }
                SetGlobal(ix) => {
                    let mut globals = m.globals;

                    globals[ix as usize] = acc.get_copy();
                    vm.gc().write_barrier(globals.as_gc());
                }

                SetThis => {
                    vm.vthis = acc.get_copy();
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

                    let pthis = vm.vthis;

                    let ctor = pop_infos!(true);
                    if ctor {
                        acc.set(pthis);
                    }

                    do_call!(cur_this, nargs, false);
                }
                Call(nargs) => {
                    do_call!(vm.vthis, nargs, false);
                }

                ObjCall(nargs) => {
                    let vtmp = sp.read();
                    sp.write(Value::Null);
                    sp = sp.add(1);
                    do_call!(vtmp, nargs, false);
                }

                Jump(ix) => {
                    ip -= 1;
                    ip = (ip as isize + ix as isize) as usize;
                }
                JumpIf(ix) => {
                    if acc.get_copy().bool() {
                        ip -= 1;
                        ip = (ip as isize + ix as isize) as usize;
                    }
                }
                JumpIfNot(ix) => {
                    if !acc.get_copy().bool() {
                        ip -= 1;
                        ip = (ip as isize + ix as isize) as usize;
                    }
                }
                Lt => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() < rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() < rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(<);
                    }
                }

                Eq => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() == rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() == rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(==);
                    }
                }
                Neq => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() != rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() != rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(!=);
                    }
                }
                Gt => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() > rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() > rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(>);
                    }
                }
                Gte => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() >= rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() >= rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(>=);
                    }
                }

                Lte => {
                    let lhs = *sp;
                    let rhs = acc.get_copy();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Bool(lhs.get_int32() >= rhs.get_int32()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Bool(lhs.get_number() >= rhs.get_number()));
                        sp.write(Value::Null);
                        sp = sp.add(1);
                    } else {
                        test!(>=);
                    }
                }
                Add => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(lhs.get_int32().wrapping_add(rhs.get_int32())));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Float(lhs.get_number() + rhs.get_number()));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __add);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!("wrong operands for +: {} {}", acc.get_copy(), *sp));
                    }
                }

                Bool => {
                    let v = acc.get_copy();
                    if v.is_true() || v.is_false() {
                        acc.set(Value::Bool(false));
                    } else {
                        acc.set(Value::Bool(true));
                    }
                }

                Not => {
                    let v = acc.get_copy();
                    if v.is_null() || v.is_false() {
                        acc.set(Value::Bool(true));
                    } else {
                        acc.set(Value::Bool(false));
                    }
                }

                Sub => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(lhs.get_int32().wrapping_sub(rhs.get_int32())));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Float(lhs.get_number() - rhs.get_number()));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __sub);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!("wrong operands for -: {} {}", acc.get_copy(), *sp));
                    }
                }

                Div => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(lhs.get_int32().wrapping_div(rhs.get_int32())));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Float(lhs.get_number() / rhs.get_number()));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __div);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!("wrong operands for /: {} {}", acc.get_copy(), *sp));
                    }
                }

                Mul => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(lhs.get_int32().wrapping_mul(rhs.get_int32())));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Float(lhs.get_number() * rhs.get_number()));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __mul);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!("wrong operands for *: {} {}", acc.get_copy(), *sp));
                    }
                }

                Mod => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(lhs.get_int32().wrapping_rem(rhs.get_int32())));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Float(lhs.get_number() % rhs.get_number()));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __mod);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!("wrong operands for %: {} {}", acc.get_copy(), *sp));
                    }
                }
                Shl => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(
                            lhs.get_int32().wrapping_shl(rhs.get_int32() as _),
                        ));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Int(
                            (lhs.get_number() as i32).wrapping_shl(rhs.get_number() as _),
                        ));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __shl);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!(
                            "wrong operands for <<: {} {}",
                            acc.get_copy(),
                            *sp
                        ));
                    }
                }

                Shr => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(
                            lhs.get_int32().wrapping_shr(rhs.get_int32() as _),
                        ));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Int(
                            (lhs.get_number() as i32).wrapping_shr(rhs.get_number() as _),
                        ));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __shr);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!(
                            "wrong operands for >>: {} {}",
                            acc.get_copy(),
                            *sp
                        ));
                    }
                }
                UShr => {
                    let rhs = acc.get_copy();
                    let lhs = sp.read();
                    if lhs.is_int32() && rhs.is_int32() {
                        acc.set(Value::Int(
                            ((lhs.get_int32() as u32).wrapping_shl(rhs.get_int32() as _)) as i32,
                        ));
                        pop!(1);
                    } else if lhs.is_number() && rhs.is_number() {
                        acc.set(Value::Int(
                            (lhs.get_number() as u32).wrapping_shl(rhs.get_number() as _) as i32,
                        ));
                        pop!(1);
                    } else if let Some(mut obj) = lhs.downcast_ref::<Object>() {
                        gc_frame!(vm.gc().roots() => obj: Gc<Object>);
                        object_op!(obj, acc.as_ref(), __ushr);
                        pop!(1);
                    } else {
                        vm.throw_str(&format!(
                            "wrong operands for <<: {} {}",
                            acc.get_copy(),
                            *sp
                        ));
                    }
                }

                Compare => {
                    save!();
                    let c = value_cmp(vm, &*sp, &*acc);
                    restore!();
                    acc.set(if c.get_int32() == INVALID_CMP {
                        Value::Null
                    } else {
                        c
                    });
                }
                IsNull => {
                    acc.set(Value::Bool(acc.get_copy().is_null()));
                }
                IsNotNull => {
                    acc.set(Value::Bool(!acc.is_null()));
                }
                Or => {
                    match (acc.get_copy(), sp.read()) {
                        (x, y) if x.is_int32() && y.is_int32() => {
                            acc.set(Value::Int(x.get_int32() | y.get_int32()));
                        }
                        (x, y) if x.is_bool() && y.is_bool() => {
                            acc.set(Value::Bool(x.get_bool() | y.get_bool()));
                        }
                        _ => {
                            vm.throw_str(concat!("incompatible types for '", stringify!($op), "'"))
                        }
                    }
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                And => {
                    match (acc.get_copy(), sp.read()) {
                        (x, y) if x.is_int32() && y.is_int32() => {
                            acc.set(Value::Int(x.get_int32() & y.get_int32()));
                        }
                        (x, y) if x.is_bool() && y.is_bool() => {
                            acc.set(Value::Bool(x.get_bool() & y.get_bool()));
                        }
                        _ => {
                            vm.throw_str(concat!("incompatible types for '", stringify!($op), "'"))
                        }
                    }
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                Xor => intop!(^),

                AccArray => {
                    gc_frame!(vm.gc().roots() => object = sp.read());
                    let sym = acc.to_symbol(vm);
                    let field = object.field(vm, sym);
                    acc.set(field);
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                AccIndex(ix) => {
                    let field = acc.field(vm, Symbol::Index(ix as _));
                    acc.set(field);
                }
                AccBuiltinResolved(val) => {
                    acc.set(val);
                }
                AccBuiltin(ix) => {
                    let f = m.globals[ix as usize].to_symbol(vm);
                    gc_frame!(vm.gc().roots() => obj = vm.builtins.downcast_ref::<Object>().unwrap_unchecked());

                    let field = obj.get(vm, f);
                    *acc = field;
                    // patch code so next builtin load is O(1)
                    m[ip - 1] = Op::AccBuiltinResolved(field);
                }
                AccField(ix, fdbk) => {
                    if let Some(object) = acc.downcast_ref::<Object>() {
                        if let Feedback::PropertyCache {
                            structure,
                            offset,
                            mode,
                        } = m.feedback[fdbk as usize]
                        {
                            if let Some(structure) = structure.upgrade() {
                                if object.structure().id() == structure.id() {
                                    match mode {
                                        GetByIdMode::Default => {
                                            acc.set(*object.direct(offset as _));
                                        }
                                        GetByIdMode::ArrayLength => {
                                            acc.set(Value::Int(object.indexed().length() as _));
                                        }
                                    }
                                    continue;
                                }
                            }
                        } else if let Feedback::ProtoLoad {
                            structure,
                            cached_offset,
                            cached_slot,
                            proto_structure,
                        } = m.feedback[fdbk as usize]
                        {
                            if let (Some(structure), Some(cached_slot), Some(proto_structure)) = (
                                structure.upgrade(),
                                cached_slot.upgrade(),
                                proto_structure.upgrade(),
                            ) {
                                if object.structure().id() == structure.id()
                                    && proto_structure.id() == cached_slot.structure().id()
                                {
                                    acc.set(*cached_slot.direct(cached_offset as _));
                                    continue;
                                }
                            }
                        }
                    }

                    let name = m.globals[ix as usize].to_symbol(vm);
                    //save!();
                    let value = slow_paths::slow_acc_field(vm, &acc, &mut m, name, fdbk as _);
                    acc.set(value);
                }

                SetField(ix, fdbk) => {
                    let obj = *sp;

                    'slow: loop {
                        if obj.is_obj() {
                            let mut obj = obj.downcast_ref::<Object>().unwrap_unchecked();
                            if let Feedback::PutNewCache {
                                new_structure,
                                old_structure,
                                offset,
                                chain,
                            } = m.feedback[fdbk as usize]
                            {
                                if let (Some(new_structure), Some(old_structure), Some(chain)) = (
                                    new_structure.upgrade(),
                                    old_structure.upgrade(),
                                    chain.upgrade(),
                                ) {
                                    if Gc::ptr_eq(*obj.structure(), old_structure) {
                                        let mut i = 0;
                                        let mut t2 = old_structure.prototype;
                                        let vector = chain.vector;
                                        while t2.is_not_null() {
                                            let t2_1 = t2.structure();
                                            if !Nullable::ptr_eq(t2_1.nullable(), vector[i]) {
                                                break 'slow;
                                            }
                                            i += 1;
                                            t2 = t2_1.prototype;
                                        }
                                        *obj.structure_mut() = new_structure;

                                        *obj.direct_mut(offset as _) = acc.get_copy();
                                        vm.gc().write_barrier(obj);
                                        sp.write(Value::Null);
                                        sp = sp.add(1);
                                        println!("PutNewCache success Structure({}) -> Structure({}): {}",old_structure.id(), new_structure.id(), offset);
                                        continue 'interp;
                                    } else {
                                        break 'slow;
                                    }
                                }
                            } else if let Feedback::PutReplaceCache { structure, offset } =
                                m.feedback[fdbk as usize]
                            {
                                if let Some(structure) = structure.upgrade() {
                                    if Gc::ptr_eq(*obj.structure(), structure) {
                                        *obj.direct_mut(offset as _) = acc.get_copy();
                                        vm.gc().write_barrier(obj);
                                        sp.write(Value::Null);
                                        sp = sp.add(1);

                                        continue 'interp;
                                    }
                                }
                            }
                        }
                        break 'slow;
                    }

                    let field = m.globals[ix as usize].to_symbol(vm);

                    slow_paths::slow_set_field(vm, &*sp, &mut m, field, fdbk as _, &acc);

                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                SetArray => {
                    gc_frame!(vm.gc().roots() => object = sp.read(), k = sp.add(1).read());
                    let k = k.to_symbol(vm);
                    object.set_field(vm, k, acc.get_copy());
                    sp.write(Value::Null);
                    sp = sp.add(1);
                    sp.write(Value::Null);
                    sp = sp.add(1);
                }
                SetIndex(k) => {
                    let ix = Symbol::Index(k as _);
                    gc_frame!(vm.gc().roots() => object = sp.read());
                    object.set_field(vm, ix, acc.get_copy());

                    sp.write(Value::Null);
                    sp = sp.add(1);
                }

                Trap(ix) => {
                    sp = sp.sub(6);
                    sp.write(Value::Int(vm.csp as i32 - vm.spmin as i32));
                    sp.add(1).write(vm.vthis);
                    sp.add(2).write(if vm.env.is_not_null() {
                        Value::Abstract(vm.env.as_gc().as_dyn())
                    } else {
                        Value::Null
                    });
                    sp.add(3).write(Value::Int(ip as i32 + ix as i32));
                    sp.add(4).write(if m.is_null() {
                        Value::Null
                    } else {
                        Value::encode_object_value(m.as_gc())
                    });
                    sp.add(5).write(Value::Int(vm.trap as _));
                    vm.trap = vm.spmax as isize - sp as isize;
                    //       println!("SET TRAP {:x} {:p}", vm.trap, sp);
                }
                EndTrap => {
                    vm.trap = sp.add(5).read().int() as _;
                    pop!(6);
                }
                Ret(nargs) => {
                    for i in 0..nargs {
                        vm.close_upvalues(sp.add(i as _));
                    }
                    let pthis = vm.vthis;
                    pop!(nargs);
                    let ctor = pop_infos!(true);
                    if ctor {
                        acc.set(pthis);
                    }
                }
                Swap => {
                    let tmp = acc.get_copy();
                    acc.set(sp.read());
                    sp.write(tmp);
                }
                CloseUpvalue => {
                    vm.close_upvalues(sp);
                    pop!(1);
                }
                MakeEnv(n) => {
                    let mut func = acc.get_copy().prim_or_func();
                    let mut upvals = func.module.globals[n as usize].array();
                    let mut real_upvals = vm.gc().array(upvals.len(), Nullable::NULL);
                    gc_frame!(vm.gc().roots() => func: Gc<Function>, upvals: Gc<Array<Value>>, real_upvals: Gc<Array<Nullable<Upvalue>>>);

                    for i in 0..upvals.len() {
                        let [is_local, index] = std::mem::transmute::<_, [i16; 2]>(upvals[i].int());
                        if is_local == 0 {
                            real_upvals[i] = vm.env[index as usize];
                        } else {
                            let slot = sp.add(index as usize);

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
                        prim: func.prim,
                        construct_struct: Nullable::NULL,
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
                    arr[0] = acc.get_copy();
                    vm.gc().write_barrier(arr);

                    acc.set(Value::Array(arr));
                }

                TypeOf => {
                    let tag = acc.tag();
                    acc.set(WAFFLE_TYPEOF[tag as usize]);
                }
                New(argc, fdbk) => {
                    //save!();
                    let (class, constructor);
                    'l: loop {
                        if acc.is_obj() {
                            if let Feedback::PropertyCache {
                                structure,
                                offset,
                                mode: _,
                            } = m.feedback[fdbk as usize]
                            {
                                let object = acc.get_object().downcast_unchecked::<Object>();
                                if let Some(structure) = structure.upgrade() {
                                    if structure.id() == object.structure().id() {
                                        constructor = *object.direct(offset as _);

                                        if !constructor.is_function() {
                                            vm.throw_str(format!(
                                                "constructor must be a function: '{}'",
                                                constructor
                                            ))
                                        }

                                        class = object;
                                        break 'l;
                                    }
                                }
                            }
                        }

                        if !acc.is_obj() {
                            vm.throw_str("trying to create new instance of non object");
                        }

                        gc_frame!(vm.gc().roots() => object = acc.downcast_ref::<Object>().unwrap(), slot = Slot::new());
                        let id = vm.id(Id::Constructor);

                        let found = object.get_own_property_slot(vm, id, &mut slot);
                        if found
                            && slot.is_load_cacheable()
                            && Nullable::ptr_eq(
                                slot.base
                                    .map(|x| x.as_dyn().nullable())
                                    .unwrap_or_else(|| Nullable::NULL),
                                object.as_dyn().nullable(),
                            )
                        {
                            let weak = vm.gc().weak(*object.structure());
                            m.feedback[fdbk as usize] = Feedback::PropertyCache {
                                structure: weak,
                                offset: slot.offset(),
                                mode: GetByIdMode::Default,
                            };
                            vm.gc().write_barrier(m.feedback.as_gc());
                        } else {
                            m.feedback[fdbk as usize] = Feedback::None;
                        }

                        let value = slot.get(vm, acc.get_copy());
                        if !value.is_function() {
                            vm.throw_str(format!("constructor must be a function: '{}'", value))
                        }
                        constructor = value;
                        class = *object;
                        break 'l;
                    }
                    gc_frame!(vm.gc().roots() => structure = *class.structure(), class = class, constructor = constructor);

                    gc_frame!(vm.gc().roots() => structure = structure.constructor_structure(vm, class.nullable()));

                    gc_frame!(vm.gc().roots() => base = Object::new(vm,&structure,OBJECT_CLASS,Value::Null));

                    acc.set(constructor.get_copy());

                    do_call!(Value::Object(base.get_copy()), argc, true);
                }
                Super(argc, fdbk) => {
                    if !acc.is_obj() {
                        vm.throw_str("trying to invoke super constructor of non object");
                    }
                    gc_frame!(vm.gc().roots() => tmp = acc.downcast_ref::<Object>().unwrap_unchecked());
                    gc_frame!(vm.gc().roots() => super_class = if tmp.prototype().is_null() {
                        vm.throw_str("object does not have an prototype");
                    } else {
                        let proto = tmp.prototype();
                        let proto2 = proto.prototype();
                        if proto2.is_null() {
                            vm.throw_str("object does not inherit any class");
                        }
                        proto2.as_gc()
                    });

                    let constructor;
                    'l: loop {
                        if acc.is_obj() {
                            if let Feedback::PropertyCache {
                                structure,
                                offset,
                                mode: _,
                            } = m.feedback[fdbk as usize]
                            {
                                let object = super_class.get_copy();
                                if let Some(structure) = structure.upgrade() {
                                    if structure.id() == object.structure().id() {
                                        constructor = *object.direct(offset as _);

                                        if !constructor.is_function() {
                                            vm.throw_str(format!(
                                                "constructor must be a function: '{}'",
                                                constructor
                                            ))
                                        }

                                        break 'l;
                                    }
                                }
                            }
                        }

                        if !acc.is_obj() {
                            vm.throw_str("trying to create new instance of non object");
                        }

                        gc_frame!(vm.gc().roots() => slot = Slot::new());
                        let id = vm.id(Id::Constructor);

                        let found = super_class.get_own_property_slot(vm, id, &mut slot);
                        if found
                            && slot.is_load_cacheable()
                            && Nullable::ptr_eq(
                                slot.base
                                    .map(|x| x.as_dyn().nullable())
                                    .unwrap_or_else(|| Nullable::NULL),
                                super_class.as_dyn().nullable(),
                            )
                        {
                            let weak = vm.gc().weak(*super_class.structure());
                            m.feedback[fdbk as usize] = Feedback::PropertyCache {
                                structure: weak,
                                offset: slot.offset(),
                                mode: GetByIdMode::Default,
                            };
                            vm.gc().write_barrier(m.feedback.as_gc());
                        } else {
                            m.feedback[fdbk as usize] = Feedback::None;
                        }

                        let value = slot.get(vm, acc.get_copy());
                        if !value.is_function() {
                            vm.throw_str(format!("constructor must be a function: '{}'", value))
                        }
                        constructor = value;

                        break 'l;
                    }

                    acc.set(constructor);

                    do_call!(Value::Object(tmp.get_copy()), argc, true);
                }
                Hash => {
                    save!();
                    let hash = value_hash(vm, &*acc);
                    acc.set(hash);
                }
                Apply(_) => todo!(),
                JumpTable(_) => todo!(),

                Leave | Last => break,
                _ => todo!("{:p} {}", *m, m.name),
            }
            stack_valid!();
        }
    }

    vm.sp = sp;
    vm.csp = csp;
    acc.get_copy()
}
