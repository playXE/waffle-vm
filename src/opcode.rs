use crate::value::Value;

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
    AccField(i32),
    AccArray,
    AccIndex(i32),
    AccBuiltin(i32),
    AccBuiltinResolved(Value),
    SetStack(i32),
    SetGlobal(i32),
    SetEnv(i32),
    SetField(i32),
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
    New,
    JumpTable(i32),
    Apply(i32),
    PhysCompare,
    TailCall(i16, i16),
    ToNum,
    Loop,
    Leave,
    Last,
}
