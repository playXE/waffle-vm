use crate::{
    memory::{gcwrapper::WeakRef, Trace},
    object::Object,
    structure::{Structure, StructureChain},
    value::Value,
};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Op {
    AccNull,
    AccTrue,
    AccFalse,
    AccThis,
    AccInt(i32),
    AccStack(i32),
    AccGlobal(i32),
    AccEnv(i32),
    AccField(i32, u32),
    AccArray,
    AccIndex(i32),
    AccBuiltin(i32),
    AccBuiltinResolved(Value),
    SetStack(i32),
    SetGlobal(i32),
    SetEnv(i32),
    SetField(i32, u32),
    SetArray,
    SetIndex(i32),
    SetThis,
    Push,
    Pop(i32),
    Call(i32),
    ObjCall(i32),
    Jump(i32),
    JumpIf(i32),
    JumpIfNot(i32),
    Trap(i32),
    EndTrap,
    Ret(i32),
    MakeEnv(i32),
    MakeArray(i32),
    Bool,
    IsNull,
    IsNotNull,
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Shl,
    Shr,
    UShr,
    Or,
    And,
    Xor,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Not,
    TypeOf,
    Compare,
    Hash,
    New(u32, u32),
    JumpTable(i32),
    Apply(i32),
    PhysCompare,
    TailCall(i16, i16),
    ToNum,
    Loop,
    Leave,
    Ctor(i32),
    CloseUpvalue,
    Swap,
    Super(u16, u32),
    SpreadCall(u32),
    Last,
}

#[derive(Clone, Copy)]
pub enum GetByIdMode {
    /// Load property directly from an object
    Default,
    /// Load array length
    ArrayLength,
}

#[repr(C)]
#[derive(Clone, Copy)]
pub enum Feedback {
    None,
    ProtoLoad {
        proto_structure: WeakRef<Structure>,
        structure: WeakRef<Structure>,
        cached_offset: u32,
        cached_slot: WeakRef<Object>,
    },
    PropertyCache {
        structure: WeakRef<Structure>,
        offset: u32,
        mode: GetByIdMode,
    },
    PutNewCache {
        new_structure: WeakRef<Structure>,
        old_structure: WeakRef<Structure>,
        offset: u32,
        chain: WeakRef<StructureChain>,
    },
    PutReplaceCache {
        structure: WeakRef<Structure>,
        offset: u32,
    },
}

unsafe impl Trace for Feedback {
    fn trace(&mut self, visitor: &mut dyn crate::memory::Visitor) {
        match self {
            Feedback::PropertyCache { structure, .. } => structure.trace(visitor),
            Feedback::PutNewCache {
                new_structure,
                old_structure,
                chain,
                ..
            } => {
                chain.trace(visitor);
                new_structure.trace(visitor);
                old_structure.trace(visitor);
            }
            Feedback::PutReplaceCache { structure, .. } => {
                structure.trace(visitor);
            }
            Feedback::ProtoLoad {
                structure,
                cached_offset,
                cached_slot,
                proto_structure,
            } => {
                structure.trace(visitor);
                cached_offset.trace(visitor);
                cached_slot.trace(visitor);
                proto_structure.trace(visitor);
            }
            _ => (),
        }
    }
}
