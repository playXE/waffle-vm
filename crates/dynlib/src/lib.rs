use waffle::{value::Value, vm::VM};

#[no_mangle]
pub extern "C" fn __waffle_entry_point(_: &mut VM) {
    println!("Entry point!");
}

pub extern "C" fn add(_: &mut VM, x: &Value, y: &Value) -> Value {
    match (x, y) {
        (Value::Int(x), Value::Int(y)) => return Value::Int(*x + *y),
        _ => Value::Null,
    }
}

#[no_mangle]
pub extern "C" fn add__2() -> usize {
    add as _
}
