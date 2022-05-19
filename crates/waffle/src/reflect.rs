#![allow(dead_code)]
use crate::gc_frame;
use crate::memory::gcwrapper::{Gc, Nullable};
use crate::value::{Function, Module, Value};
use crate::vm::VM;

use super::opcode::*;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::mem::replace;
use std::rc::Rc;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Global {
    Str(Box<str>),
    Symbol(Box<str>),
    Float(u64),
    Int(i64),
    Var(Box<str>),
    Func(isize, isize),
    Upval(Vec<(bool, u16)>),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Access {
    Env(isize),
    Stack(isize),
    Global(isize),
    Field(isize),
    Index(u32),
    Array,
    This,
}

pub struct Label {
    lname: String,
    ltraps: Vec<isize>,
    lstack: isize,
    lpos: Option<isize>,
    lwait: Vec<Box<dyn FnOnce(&mut Context)>>,
}

#[derive(Default)]
pub struct Globals {
    pub globals: HashMap<Rc<Global>, isize>,
    pub gobjects: HashMap<Vec<String>, isize>,
    pub functions: Vec<(Box<[Op]>, Vec<(isize, isize)>, isize, isize)>,
    pub gtable: Vec<Rc<Global>>,
    pub labels: HashMap<String, Label>,
}

pub struct Scope {
    locals: Vec<Local>,
    save_stack: isize,
}

pub struct Context<'a> {
    g: &'a mut Globals,
    pub ops: Vec<Op>,
    pub locals: &'a mut HashMap<String, isize>,
    pub env: HashMap<String, isize>,
    pub nenv: isize,
    pub stack: isize,
    pub loop_limit: isize,
    pub loop_traps: isize,
    pub limit: isize,
    pub traps: Vec<isize>,
    pub breaks: Vec<Box<dyn FnOnce(&mut Self)>>,
    pub continues: Vec<Box<dyn FnOnce(&mut Self)>>,
    pub pos: Vec<(isize, isize)>,
    pub scope: &'a mut Vec<Scope>,
}

use Op::*;

pub fn stack_delta(op: Op) -> isize {
    match op {
        Add | Sub | Mul | Div | Mod | Shl | Shr | UShr | Or | And | Xor | Eq | Neq | Gt | Gte
        | Lt | Lte | PhysCompare | AccArray | SetField(_) | SetIndex(_) | Compare => -1,
        SetArray => -2,
        CloseUpvalue => -1,
        Push => 1,
        Pop(x) => -(x as isize),
        Apply(nargs) | Call(nargs) => -(nargs as isize),
        TailCall(_, nargs) => -(nargs as isize),
        ObjCall(nargs) => -(nargs as isize + 1),
        MakeEnv(_) => 0,
        MakeArray(n) => -(n as isize),
        Trap(_) => 6,
        EndTrap => -6,
        _ => 0,
    }
}

impl<'a> Context<'a> {
    pub fn save_breaks(
        &mut self,
    ) -> (
        Vec<Box<dyn FnOnce(&mut Self)>>,
        Vec<Box<dyn FnOnce(&mut Self)>>,
        isize,
        isize,
    ) {
        let prev = self.loop_limit;
        self.loop_limit = self.stack as _;
        (
            replace(&mut self.continues, vec![]),
            replace(&mut self.breaks, vec![]),
            prev,
            self.loop_traps,
        )
    }

    pub fn process_continues(&mut self, oldc: Vec<Box<dyn FnOnce(&mut Self)>>) {
        for c in replace(&mut self.continues, oldc) {
            c(self);
        }
    }

    pub fn enter_scope(&mut self) {
        self.scope.push(Scope {
            locals: vec![],
            save_stack: self.stack,
        });
    }

    pub fn leave_scope(&mut self) {
        let scope = self.scope.pop().unwrap();
        let there_is_captured = scope.locals.iter().any(|x| x.is_captured());
        if !there_is_captured {
            self.write(Op::Pop(scope.locals.len() as _));
        } else {
            for local in scope.locals.iter().rev() {
                if local.is_captured() {
                    self.write(Op::CloseUpvalue);
                } else {
                    self.write(Op::Pop(1));
                }
            }
        }
        if scope.save_stack < self.stack {
            self.write(Op::Pop(self.stack as i32 - scope.save_stack as i32));
        }
    }

    pub fn process_breaks(
        &mut self,
        oldc: Vec<Box<dyn FnOnce(&mut Self)>>,
        oldl: isize,
        oldt: isize,
    ) {
        for c in replace(&mut self.breaks, oldc) {
            c(self);
        }
        self.loop_limit = oldl as _;
        self.loop_traps = oldt as _;
    }
    pub fn write(&mut self, op: Op) {
        self.stack = self.stack + stack_delta(op);

        self.ops.push(op);
    }
    pub fn pos(&self) -> isize {
        self.ops.len() as _
    }
    pub fn jmp(&mut self) -> impl FnOnce(&mut Context) {
        let p = self.pos();
        self.write(Op::Jump(0));
        move |cx: &mut Context| {
            cx.ops[p as usize] = Op::Jump(cx.pos() as i32 - p as i32);
        }
    }

    pub fn cjmp(&mut self, cond: bool) -> impl FnOnce(&mut Context) {
        let p = self.pos();
        self.write(Op::Jump(0));
        move |cx: &mut Context| {
            let jmp_pos = cx.pos() as i32 - p as i32;

            cx.ops[p as usize] = if cond {
                Op::JumpIf(jmp_pos)
            } else {
                Op::JumpIfNot(jmp_pos)
            }
        }
    }

    pub fn trap(&mut self) -> impl FnOnce(&mut Context) {
        let p = self.pos();
        self.write(Op::Trap(0));
        move |cx: &mut Context| cx.ops[p as usize] = Op::Trap((cx.pos() - p) as _)
    }

    pub fn goto(&mut self, p: isize) {
        self.write(Op::Jump(p as i32 - self.pos() as i32));
    }

    pub fn global(&mut self, g: Rc<Global>) -> isize {
        let gid = self.g.gtable.len();
        match self.g.globals.entry(g.clone()) {
            Entry::Occupied(x) => *x.get(),
            Entry::Vacant(x) => {
                x.insert(gid as _);
                self.g.gtable.push(g);
                gid as _
            }
        }
    }

    fn lookup_scope(&mut self, name: &str) -> Option<Access> {
        for i in 0..self.scope.len() {
            for j in 0..self.scope[i].locals.len() {
                if self.scope[i].locals[j].name == name {
                    if self.scope[i].locals[j].offset <= self.limit {
                        match self.env.get(name).copied() {
                            Some(x) => return Some(Access::Env(x)),
                            None => {
                                let e = self.nenv;
                                self.nenv += 1;
                                self.env.insert(name.to_string(), e as _);
                                self.scope[i].locals[j].capture();
                                return Some(Access::Env(e as _));
                            }
                        }
                    } else {
                        return Some(Access::Stack(self.scope[i].locals[j].offset));
                    }
                }
            }
        }
        None
    }

    pub fn access_var(&mut self, s: &str) -> Access {
        self.lookup_scope(s).unwrap_or_else(|| {
            let g = self.global(Rc::new(Global::Var(s.to_string().into_boxed_str())));
            Access::Global(g)
        })
    }

    pub fn compile_access_get(&mut self, a: &Access) {
        match *a {
            Access::Env(x) => self.write(Op::AccEnv(x as _)),
            Access::Stack(l) => self.write(Op::AccStack(self.stack as i32 - l as i32)),
            Access::Global(g) => self.write(Op::AccGlobal(g as _)),
            Access::Field(f) => self.write(Op::AccField(f as _)),
            Access::This => self.write(Op::AccThis),
            Access::Index(x) => self.write(Op::AccIndex(x as _)),
            Access::Array => {
                self.write(Op::Push);
                self.write(Op::AccStack(2));
                self.write(Op::AccArray);
            }
        }
    }

    pub fn compile_access_set(&mut self, a: &Access) {
        match *a {
            Access::Env(x) => self.write(Op::SetEnv(x as _)),
            Access::Stack(l) => self.write(Op::SetStack(self.stack as i32 - l as i32)),
            Access::Global(g) => self.write(Op::SetGlobal(g as _)),
            Access::Field(f) => self.write(Op::SetField(f as _)),
            Access::Index(i) => self.write(Op::SetIndex(i as _)),
            Access::This => self.write(Op::SetThis),
            Access::Array => self.write(Op::SetArray),
        }
    }

    pub fn use_var(&mut self, id: &str) {
        /*match self.locals.get(id).copied() {
            Some(l) => {
                if l <= self.limit as isize {
                    let e = self.env.get(id).copied().unwrap_or_else(|| {
                        let e = self.nenv;
                        self.nenv += 1;
                        self.env.insert(id.to_string(), e as _);
                        e as isize
                    });

                    self.write(Op::AccEnv(e as _));
                } else {
                    let p = self.stack as isize - l;
                    self.write(Op::AccStack(p as _));
                }
            }
            None => {
                let g = self.global(Rc::new(Global::Var(id.to_string().into_boxed_str())));
                self.write(Op::AccGlobal(g as _));
            }
        }*/
        let acc = self.access_var(id);
        self.compile_access_get(&acc)
    }

    pub fn add_var(&mut self, id: &str, has_val: bool) {
        if !has_val {
            self.write(Op::AccNull);
        }

        self.write(Op::Push);
        self.scope
            .last_mut()
            .unwrap()
            .locals
            .push(Local::new(id, self.stack));
        //self.locals.insert(id.to_string(), self.stack as _);
    }

    pub fn check_stack(&mut self, stack: isize) -> Result<(), String> {
        if self.stack != stack as _ {
            return Err(format!("Stack alignment failure"));
        }
        Ok(())
    }

    pub fn compile_function<R>(
        &mut self,
        params: &[Box<str>],
        mut cb: impl FnMut(&mut Context) -> Result<R, String>,
    ) -> Result<R, String> {
        let mut locals = self.locals.clone();
        let mut ctx = Context {
            g: self.g,
            ops: vec![],
            pos: vec![],
            breaks: vec![],
            continues: vec![],
            env: HashMap::new(),
            nenv: 0,
            traps: vec![],
            loop_traps: 0,
            limit: self.stack as _,
            stack: self.stack,
            locals: &mut locals,
            loop_limit: self.loop_limit,
            scope: self.scope,
        };
        ctx.enter_scope();
        for p in params {
            ctx.stack += 1;

            ctx.scope
                .last_mut()
                .unwrap()
                .locals
                .push(Local::new(p, ctx.stack));
            //ctx.locals.insert(p.clone().to_string(), ctx.stack as _);
        }

        let s = ctx.stack;
        let ret = cb(&mut ctx)?;
        ctx.check_stack(s as _)?;
        ctx.leave_scope();
        ctx.write(Op::Ret(ctx.stack as i32 - ctx.limit as i32));
        let gid = ctx.g.gtable.len();
        ctx.g.functions.push((
            replace(&mut ctx.ops, vec![]).into_boxed_slice(),
            ctx.pos.clone(),
            gid as _,
            params.len() as _,
        ));
        ctx.g.gtable.push(Rc::new(Global::Func(gid as _, -1)));

        if ctx.nenv > 0 {
            let mut a = vec![String::new(); ctx.nenv as usize];

            for (v, i) in ctx.env.iter() {
                a[*i as usize] = v.clone();
            }
            drop(ctx);
            let mut upvals = vec![];
            for id in a {
                let acc = self.access_var(&id);
                match acc {
                    Access::Env(x) => upvals.push((false, x as u16)),
                    Access::Stack(x) => upvals.push((true, (self.stack as i32 - x as i32) as u16)),
                    _ => unreachable!(),
                }
            }
            /*for id in a {
                self.use_var(&id);
                self.write(Op::Push);
            }*/

            let upvals = self.global(Rc::new(Global::Upval(upvals)));
            self.write(Op::AccGlobal(gid as _));
            self.write(Op::MakeEnv(upvals as _));
        } else {
            drop(ctx);
            self.write(Op::AccGlobal(gid as _));
        }
        Ok(ret)
    }

    pub fn try_catch<R>(
        &mut self,
        mut try_: impl FnMut(&mut Self) -> Result<(), String>,
        mut catch: impl FnMut(&mut Self) -> Result<(), String>,
    ) -> Result<(), String> {
        let trap = self.trap();

        self.traps.push(self.stack as _);
        try_(self)?;
        self.write(Op::EndTrap);
        self.traps.pop().unwrap();
        let jend = self.jmp();
        trap(self);

        catch(self)?;
        jend(self);

        Ok(())
    }

    pub fn if_else(
        &mut self,
        mut e1: impl FnMut(&mut Self) -> Result<(), String>,
        e2: Option<impl FnMut(&mut Self) -> Result<(), String>>,
    ) -> Result<(), String> {
        let stack = self.stack;

        let jelse = self.cjmp(false);
        e1(self)?;
        self.check_stack(stack as _)?;

        match e2 {
            None => {
                jelse(self);
            }
            Some(mut e2) => {
                let jend = self.jmp();
                jelse(self);
                e2(self)?;
                self.check_stack(stack as _)?;
                jend(self);
            }
        }
        Ok(())
    }

    pub fn while_loop(
        &mut self,
        mut cond: impl FnMut(&mut Self) -> Result<(), String>,
        mut body: impl FnMut(&mut Self) -> Result<(), String>,
    ) -> Result<(), String> {
        let start = self.pos();
        self.write(Op::Loop);
        cond(self)?;
        let jend = self.cjmp(false);
        let (oldc, oldb, oldl, oldt) = self.save_breaks();
        body(self)?;
        self.process_continues(oldc);
        self.goto(start);
        self.process_breaks(oldb, oldl, oldt);
        jend(self);
        Ok(())
    }

    pub fn compile(
        cb: impl FnOnce(&mut Self) -> Result<(), String>,
    ) -> Result<(Vec<Rc<Global>>, Vec<Op>), String> {
        let g = Box::leak(Box::new(Globals::default())) as *mut Globals;
        let locals = Box::leak(Box::new(HashMap::new())) as *mut _;
        let scope = Box::leak(Box::new(Vec::new())) as *mut Vec<Scope>;
        let mut _ops = {
            let mut ctx = Self {
                g: unsafe { &mut *g },
                locals: unsafe { &mut *locals },
                stack: 0,
                loop_limit: 0,
                loop_traps: 0,
                limit: -1,
                ops: vec![],
                breaks: vec![],
                continues: vec![],
                env: HashMap::new(),
                nenv: 0,
                traps: vec![],
                pos: vec![],
                scope: unsafe { &mut *scope },
            };
            ctx.enter_scope();
            cb(&mut ctx)?;
            ctx.leave_scope();
            if !ctx.g.functions.is_empty() || !ctx.g.gobjects.is_empty() {
                let ctxops = replace(&mut ctx.ops, vec![]);

                ctx.write(Op::Jump(0));

                for (fops, _fpos, gid, nargs) in replace(&mut ctx.g.functions, vec![]) {
                    ctx.g.gtable[gid as usize] =
                        Rc::new(Global::Func(ctx.ops.len() as _, nargs as _));
                    ctx.ops.extend_from_slice(&*fops);
                }

                ctx.ops[0] = Op::Jump(ctx.ops.len() as _);

                ctx.ops.extend_from_slice(&ctxops);
            }
            ctx.ops
        };

        let g = unsafe { Box::from_raw(g) };
        let _locals = unsafe { Box::from_raw(locals) };
        let _ = unsafe { Box::from_raw(scope) };
        Ok((g.gtable, _ops))
    }
}
use std::fmt::Write;
pub fn disassembly(globals: &[Rc<Global>], ops: &[Op]) -> String {
    let mut f = String::new();
    let mut write = || -> Result<(), std::fmt::Error> {
        writeln!(f, "globals section: ")?;
        for (i, g) in globals.iter().enumerate() {
            writeln!(f, "{}: {:?}", i, g)?;
        }

        writeln!(f, "code section: ")?;
        for i in 0..ops.len() {
            write!(f, "{:04}: ", i)?;
            match ops[i] {
                Op::AccArray => write!(f, "AccArray")?,
                Op::AccNull => write!(f, "AccNull")?,
                Op::AccTrue => write!(f, "AccTrue")?,
                Op::AccFalse => write!(f, "AccFalse")?,
                Op::AccThis => write!(f, "AccThis")?,
                Op::AccInt(i) => write!(f, "AccInt {}", i)?,
                Op::AccStack(x) => write!(f, "AccStack {}", x)?,
                Op::AccGlobal(x) => write!(f, "AccGlobal {} // {:?}", x, globals[x as usize])?,
                Op::AccEnv(x) => write!(f, "AccEnv {}", x)?,
                Op::AccField(x) => write!(f, "AccField {:?}", globals[x as usize])?,
                Op::AccIndex(x) => write!(f, "AccIndex {}", x)?,
                Op::AccBuiltin(x) => write!(f, "AccBuiltin ${} // {:?}", x, globals[x as usize])?,
                Op::SetStack(x) => write!(f, "SetStack {}", x)?,
                Op::SetGlobal(x) => write!(f, "SetGlobal {}", x)?,
                Op::SetEnv(x) => write!(f, "SetEnv {}", x)?,
                Op::SetField(x) => write!(f, "SetField {:?}", globals[x as usize])?,
                Op::SetArray => write!(f, "SetArray")?,
                Op::SetIndex(x) => write!(f, "SetIndex {}", x)?,
                Op::Pop(x) => write!(f, "Pop {}", x)?,
                Op::Call(x) => write!(f, "Call {}", x)?,
                Op::ObjCall(x) => write!(f, "ObjCall {}", x)?,
                Op::Jump(x) => write!(f, "Jump {:04}", x)?,
                Op::JumpIf(x) => write!(f, "JumpIf {:04}", x)?,
                Op::JumpIfNot(x) => write!(f, "JumpIfNot {:04}", x)?,
                Op::Trap(x) => write!(f, "Trap {:04}", x)?,
                Op::Ret(x) => write!(f, "Ret {}", x)?,
                _ => write!(f, "{:?}", ops[i])?,
            }

            writeln!(f)?;
        }
        Ok(())
    };

    write().unwrap();
    f
}

pub fn make_module(vm: &mut VM, globals: &[Rc<Global>], ops: &[Op]) -> Gc<Module> {
    unsafe {
        let module = vm.gc().malloc_varsize::<Module>(ops.len() + 1, &mut []);
        let ptr = module.as_mut_ptr();

        ptr.write(Module {
            name: Value::Null,
            globals: Nullable::NULL,
            exports: Value::Null,
            loader: Value::Null,
            code: [],
            code_size: ops.len() + 1,
        });

        core::ptr::copy_nonoverlapping(ops.as_ptr(), (&mut *ptr).code.as_mut_ptr(), ops.len());
        (*ptr).code.as_mut_ptr().add(ops.len()).write(Op::Leave);
        let mut m = module.assume_init();
        gc_frame!(vm.gc().roots() => m: Gc<Module>);
        m.globals = vm.gc().array(globals.len(), Value::Null).nullable();
        for (i, g) in globals.iter().enumerate() {
            match &**g {
                Global::Float(x) => m.globals[i] = Value::Float(f64::from_bits(*x)),
                Global::Int(x) => m.globals[i] = Value::Int(*x as i32),
                Global::Str(x) => {
                    m.globals[i] = Value::Str(vm.gc().str(x));
                    vm.gc().write_barrier(m.get());
                }
                Global::Symbol(x) => {
                    m.globals[i] = Value::Symbol(vm.intern(x));
                    vm.gc().write_barrier(m.get());
                }
                Global::Func(addr, nargs) => {
                    let func = vm.gc().fixed(Function {
                        nargs: *nargs as _,
                        varsize: false,
                        addr: *addr as _,
                        module: m.get().nullable(),
                        env: Nullable::NULL,
                        prim: false,
                    });

                    m.globals[i] = Value::Function(func);
                    vm.gc().write_barrier(m.get());
                }

                _ => {}
            }
        }
        m.get()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    name: String,
    offset: isize,
    is_captured: bool,
}

impl Local {
    /// Crate a new local binding definition.
    pub fn new(name: &str, offset: isize) -> Local {
        Local {
            name: name.into(),
            offset,
            is_captured: false,
        }
    }
    pub fn offset(&self) -> isize {
        self.offset
    }
    /// The local binding's name, as a `&str`.
    pub fn as_str(&self) -> &str {
        &self.name
    }

    /// Is this captured by some later closure?
    pub fn is_captured(&self) -> bool {
        self.is_captured
    }

    pub fn capture(&mut self) {
        self.is_captured = true;
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Capture {
    index: usize,
    is_local: bool,
}

impl Capture {
    pub fn new(index: usize, is_local: bool) -> Capture {
        Capture { index, is_local }
    }

    /// Get the capture's index.
    pub fn index(&self) -> usize {
        self.index
    }

    /// Get the capture's is local.
    pub fn is_local(&self) -> bool {
        self.is_local
    }
}
