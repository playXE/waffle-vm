//! Stack based tracing JIT intermediate representation.
//!

use crate::value::Value;

pub enum Ins {
    AccNull,
    AccTrue,
    AccFalse,
    AccThis,
    AccInt(i32),
    AccStack(i32),
    AccGlobal(i32),
    AccEnv(i32),
    // acc: any, sp[0]: any
    AccArraySlow,
    // acc: int32, sp[0]: object<array>
    AccArrayFast,

    AccIndexSlow(i32),
    // acc: object<array>
    AccIndexFast(i32),

    AccBuiltinResolved(Value),
    SetStack(i32),
    SetGlobal(i32),
    SetEnv(i32),
    // acc: any, sp[0]: any
    SetArraySlow,
    // acc: int32, sp[0]: object<array>
    SetArrayFast,
    // acc: any
    SetIndexSlow(i32),
    // acc: object<array>
    SetindexFast(i32),
    SetThis,

    Push,
    Pop(i32),

    Jump(i32),
}
