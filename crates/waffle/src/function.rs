use crate::{memory::{*,gcwrapper::*}, structure::Structure};
use super::value::*;
pub struct Function {
    pub nargs: u32,
    pub varsize: bool,
    pub env: Nullable<Array<Nullable<Upvalue>>>,
    pub addr: usize,
    pub prim: bool,
    pub construct_struct: Nullable<Structure>,
    pub module: Nullable<Module>,
}


unsafe impl Finalize for Upvalue {}
unsafe impl Allocation for Upvalue {}
impl Managed for Upvalue {}
unsafe impl Trace for Function {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.env.trace(visitor);
        self.module.trace(visitor);
        self.construct_struct.trace(visitor);
    }
}

unsafe impl Finalize for Function {}
impl Managed for Function {}