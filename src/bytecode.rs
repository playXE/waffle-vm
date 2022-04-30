use super::opcode::*;
use super::reflect::*;
use byteorder::ReadBytesExt;
use byteorder::{LittleEndian, WriteBytesExt};
use std::io::{Cursor, Write};
use std::rc::Rc;
pub fn write_module(code: &[Op], globals: &[Rc<Global>]) -> Vec<u8> {
    let mut buf = Cursor::new(vec![]);
    let mut res = || -> Result<(), std::io::Error> {
        buf.write_u32::<LittleEndian>(globals.len() as _)?;
        let code_pos = buf.position() as usize; // we will patch with actual code size later
        buf.write_u32::<LittleEndian>(code.len() as _)?;

        for global in globals.iter() {
            match &**global {
                Global::Float(x) => {
                    buf.write_u8(0)?;
                    buf.write_u64::<LittleEndian>(*x)?;
                }
                Global::Func(pos, argc) => {
                    buf.write_u8(1)?;
                    buf.write_u32::<LittleEndian>(*pos as _)?;
                    buf.write_u32::<LittleEndian>(*argc as _)?;
                }
                Global::Int(x) => {
                    buf.write_u8(2)?;
                    buf.write_i64::<LittleEndian>(*x as _)?;
                }
                Global::Str(x) => {
                    buf.write_u8(3)?;
                    buf.write_u32::<LittleEndian>(x.len() as _)?;
                    buf.write_all(x.as_bytes())?;
                }
                Global::Var(_) => {
                    buf.write_u8(4)?;
                }
            }
        }
        let start = buf.position();
        for op in code {
            match *op {
                Op::AccNull => buf.write_u8(0)?,
                Op::AccTrue => buf.write_u8(1)?,
                Op::AccFalse => buf.write_u8(2)?,
                Op::AccThis => buf.write_u8(3)?,
                Op::AccInt(x) => {
                    buf.write_u8(4)?;
                    buf.write_i32::<LittleEndian>(x)?;
                }
                Op::AccStack(x) => {
                    buf.write_u8(5)?;
                    buf.write_i32::<LittleEndian>(x)?;
                }
                Op::AccGlobal(x) => {
                    buf.write_u8(6)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::AccEnv(x) => {
                    buf.write_u8(7)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::AccField(x) => {
                    buf.write_u8(8)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::AccArray => buf.write_u8(9)?,
                Op::AccIndex(ix) => {
                    buf.write_u8(10)?;
                    buf.write_u32::<LittleEndian>(ix as _)?;
                }
                Op::AccBuiltin(x) => {
                    buf.write_u8(11)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetStack(x) => {
                    buf.write_u8(12)?;
                    buf.write_i32::<LittleEndian>(x)?;
                }
                Op::SetGlobal(x) => {
                    buf.write_u8(13)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetEnv(x) => {
                    buf.write_u8(14)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetField(x) => {
                    buf.write_u8(15)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetArray => buf.write_u8(16)?,
                Op::SetIndex(x) => {
                    buf.write_u8(16)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetThis => buf.write_u8(17)?,
                Op::Push => buf.write_u8(18)?,
                Op::Pop(x) => {
                    buf.write_u8(19)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Call(x) => {
                    buf.write_u8(20)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::ObjCall(x) => {
                    buf.write_u8(21)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Jump(x) => {
                    buf.write_u8(22)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::JumpIf(x) => {
                    buf.write_u8(23)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::JumpIfNot(x) => {
                    buf.write_u8(24)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Trap(x) => {
                    buf.write_u8(25)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::EndTrap => buf.write_u8(26)?,
                Op::Ret(x) => {
                    buf.write_u8(26)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::MakeEnv(x) => {
                    buf.write_u8(27)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::MakeArray(x) => {
                    buf.write_u8(28)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Bool => buf.write_u8(29)?,
                Op::IsNull => buf.write_u8(30)?,
                Op::IsNotNull => buf.write_u8(31)?,
                Op::Add => buf.write_u8(32)?,
                Op::Sub => buf.write_u8(33)?,
                Op::Div => buf.write_u8(34)?,
                Op::Mul => buf.write_u8(35)?,
                Op::Mod => buf.write_u8(36)?,
                Op::Shl => buf.write_u8(37)?,
                Op::Shr => buf.write_u8(38)?,
                Op::UShr => buf.write_u8(39)?,
                Op::Or => buf.write_u8(40)?,
                Op::And => buf.write_u8(41)?,
                Op::Xor => buf.write_u8(42)?,
                Op::Eq => buf.write_u8(43)?,
                Op::Neq => buf.write_u8(44)?,
                Op::Lt => buf.write_u8(45)?,
                Op::Lte => buf.write_u8(46)?,
                Op::Gt => buf.write_u8(47)?,
                Op::Gte => buf.write_u8(48)?,
                Op::Not => buf.write_u8(49)?,
                Op::TypeOf => buf.write_u8(50)?,
                Op::Compare => buf.write_u8(51)?,
                Op::Hash => buf.write_u8(52)?,
                Op::New => buf.write_u8(53)?,
                Op::JumpTable(x) => {
                    buf.write_u8(54)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Apply(x) => {
                    buf.write_u8(55)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::PhysCompare => buf.write_u8(56)?,
                Op::TailCall(stack, nargs) => {
                    buf.write_u8(57)?;
                    buf.write_u16::<LittleEndian>(stack as _)?;
                    buf.write_u16::<LittleEndian>(nargs as _)?;
                }
                Op::ToNum => buf.write_u8(58)?,
                Op::Loop => buf.write_u8(59)?,
                Op::Leave => buf.write_u8(60)?,
                Op::Last => buf.write_u8(61)?,
                _ => unreachable!(),
            }
        }
        let sz = buf.position() - start;
        buf.set_position(code_pos as _);

        buf.write_u32::<LittleEndian>(sz as _)?;
        Ok(())
    };

    res().expect("failed to write bytecode file");

    buf.into_inner()
}

pub fn read_module<T: AsRef<[u8]>>(buf: &T) -> (Vec<Op>, Vec<Rc<Global>>) {
    let mut buf = Cursor::new(buf.as_ref());

    let mut ops = vec![];
    let mut globals = vec![];
    let mut res = || -> Result<(), std::io::Error> {
        let nglobals = buf.read_u32::<LittleEndian>()?;
        let code_size = buf.read_u32::<LittleEndian>()?;

        for _ in 0..nglobals {
            let tag = buf.read_u8()?;
            match tag {
                0 => {
                    globals.push(Rc::new(Global::Float(buf.read_u64::<LittleEndian>()?)));
                }
                1 => {
                    let pos = buf.read_u32::<LittleEndian>()?;
                    let nargs = buf.read_u32::<LittleEndian>()?;
                    globals.push(Rc::new(Global::Func(pos as _, nargs as _)));
                }
                _ => todo!(),
            }
        }
        Ok(())
    };

    res().unwrap();
    (ops, globals)
}
