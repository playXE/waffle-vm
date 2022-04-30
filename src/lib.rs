#![feature(
    ptr_metadata,
    const_type_id,
    const_type_name,
    core_intrinsics,
    specialization,
    const_trait_impl,
    const_mut_refs,
    const_ptr_offset_from,
    const_refs_to_cell,
    try_trait_v2,
    box_patterns
)]
#![allow(incomplete_features)]

use memory::gcwrapper::{GCWrapper, Gc};

pub mod builtin;
pub mod bytecode;
pub mod memory;
pub mod opcode;
pub mod reflect;
pub mod support;
pub mod value;
pub mod vm;
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum CellType {
    Abstract,
}

pub fn foo(gc: &mut GCWrapper, init: i32) -> Gc<i32> {
    gc.fixed(init)
}
