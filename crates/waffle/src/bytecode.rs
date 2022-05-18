use super::opcode::*;
use super::reflect::*;
use byteorder::ReadBytesExt;
use byteorder::{LittleEndian, WriteBytesExt};
use std::io::Read;
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
                Global::Symbol(x) => {
                    buf.write_u8(3)?;
                    buf.write_u32::<LittleEndian>(x.len() as _)?;
                    buf.write_all(x.as_bytes())?;
                }
                Global::Str(x) => {
                    buf.write_u8(4)?;
                    buf.write_u32::<LittleEndian>(x.len() as _)?;
                    buf.write_all(x.as_bytes())?;
                }
                Global::Var(name) => {
                    buf.write_u8(5)?;
                    buf.write_u32::<LittleEndian>(name.len() as _)?;
                    buf.write_all(name.as_bytes())?;
                }
                Global::Upval(x) => {
                    buf.write_u8(6)?;
                    buf.write_u16::<LittleEndian>(x.len() as _)?;
                    for (is_local, index) in x.iter() {
                        buf.write_u8(*is_local as u8)?;
                        buf.write_u16::<LittleEndian>(*index as u16)?;
                    }
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
                    buf.write_u8(17)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::SetThis => buf.write_u8(18)?,
                Op::Push => buf.write_u8(19)?,
                Op::Pop(x) => {
                    buf.write_u8(20)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Call(x) => {
                    buf.write_u8(21)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::ObjCall(x) => {
                    buf.write_u8(22)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Jump(x) => {
                    buf.write_u8(23)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::JumpIf(x) => {
                    buf.write_u8(24)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::JumpIfNot(x) => {
                    buf.write_u8(25)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Trap(x) => {
                    buf.write_u8(26)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::EndTrap => buf.write_u8(27)?,
                Op::Ret(x) => {
                    buf.write_u8(28)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::MakeEnv(x) => {
                    buf.write_u8(29)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::MakeArray(x) => {
                    buf.write_u8(30)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Bool => buf.write_u8(31)?,
                Op::IsNull => buf.write_u8(32)?,
                Op::IsNotNull => buf.write_u8(33)?,
                Op::Add => buf.write_u8(34)?,
                Op::Sub => buf.write_u8(35)?,
                Op::Div => buf.write_u8(36)?,
                Op::Mul => buf.write_u8(37)?,
                Op::Mod => buf.write_u8(38)?,
                Op::Shl => buf.write_u8(39)?,
                Op::Shr => buf.write_u8(40)?,
                Op::UShr => buf.write_u8(41)?,
                Op::Or => buf.write_u8(42)?,
                Op::And => buf.write_u8(43)?,
                Op::Xor => buf.write_u8(44)?,
                Op::Eq => buf.write_u8(45)?,
                Op::Neq => buf.write_u8(46)?,
                Op::Lt => buf.write_u8(47)?,
                Op::Lte => buf.write_u8(48)?,
                Op::Gt => buf.write_u8(49)?,
                Op::Gte => buf.write_u8(50)?,
                Op::Not => buf.write_u8(51)?,
                Op::TypeOf => buf.write_u8(52)?,
                Op::Compare => buf.write_u8(53)?,
                Op::Hash => buf.write_u8(54)?,
                Op::New => buf.write_u8(55)?,
                Op::JumpTable(x) => {
                    buf.write_u8(56)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::Apply(x) => {
                    buf.write_u8(57)?;
                    buf.write_u32::<LittleEndian>(x as _)?;
                }
                Op::PhysCompare => buf.write_u8(58)?,
                Op::TailCall(stack, nargs) => {
                    buf.write_u8(59)?;
                    buf.write_u16::<LittleEndian>(stack as _)?;
                    buf.write_u16::<LittleEndian>(nargs as _)?;
                }
                Op::ToNum => buf.write_u8(60)?,
                Op::Loop => buf.write_u8(61)?,
                Op::Leave => buf.write_u8(62)?,
                Op::Ctor(argc) => {
                    buf.write_u8(63)?;
                    buf.write_u32::<LittleEndian>(argc as _)?;
                }
                Op::CloseUpvalue => buf.write_u8(64)?,
                Op::Last => buf.write_u8(255)?,
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
                2 => {
                    let int = buf.read_i64::<LittleEndian>()?;
                    globals.push(Rc::new(Global::Int(int)));
                }
                3 => {
                    let len = buf.read_u32::<LittleEndian>()? as usize;
                    let mut bytes = vec![0u8; len];
                    buf.read_exact(&mut bytes)?;
                    globals.push(Rc::new(Global::Symbol(
                        String::from_utf8(bytes).unwrap().into_boxed_str(),
                    )));
                }
                4 => {
                    let len = buf.read_u32::<LittleEndian>()? as usize;
                    let mut bytes = vec![0u8; len];
                    buf.read_exact(&mut bytes)?;
                    globals.push(Rc::new(Global::Str(
                        String::from_utf8(bytes).unwrap().into_boxed_str(),
                    )));
                }
                5 => {
                    let len = buf.read_u32::<LittleEndian>()? as usize;
                    let mut bytes = vec![0u8; len];
                    buf.read_exact(&mut bytes)?;

                    globals.push(Rc::new(Global::Var(
                        String::from_utf8(bytes).unwrap().into_boxed_str(),
                    )));
                }
                6 => {
                    let c = buf.read_u16::<LittleEndian>()? as usize;
                    let mut upvals = vec![(false, 0); c];
                    for i in 0..c {
                        let is_local = buf.read_u8()?;
                        let index = buf.read_u16::<LittleEndian>()?;
                        upvals[i] = (is_local != 0, index);
                    }
                    globals.push(Rc::new(Global::Upval(upvals)))
                }
                _ => todo!(),
            }
        }
        let start = buf.position();

        while (buf.position() - start) < code_size as u64 {
            use Op::*;

            let op = buf.read_u8()?;
            let op = match op {
                0 => AccNull,
                1 => AccTrue,
                2 => AccFalse,
                3 => AccThis,
                4 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccInt(i)
                }
                5 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccStack(i)
                }
                6 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccGlobal(i)
                }
                7 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccEnv(i)
                }
                8 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccField(i)
                }
                9 => AccArray,
                10 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccIndex(i)
                }
                11 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    AccBuiltin(i)
                }
                12 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    SetStack(i)
                }
                13 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    SetGlobal(i)
                }
                14 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    SetEnv(i)
                }
                15 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    SetField(i)
                }
                16 => SetArray,
                17 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    SetIndex(i)
                }
                18 => SetThis,
                19 => Push,
                20 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Pop(i)
                }
                21 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Call(i)
                }
                22 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    ObjCall(i)
                }
                23 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Jump(i)
                }
                24 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    JumpIf(i)
                }
                25 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    JumpIfNot(i)
                }
                26 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Trap(i)
                }
                27 => EndTrap,
                28 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Ret(i)
                }
                29 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    MakeEnv(i)
                }
                30 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    MakeArray(i)
                }
                31 => Bool,
                32 => IsNull,
                33 => IsNotNull,
                34 => Add,
                35 => Sub,
                36 => Div,
                37 => Mul,
                38 => Mod,
                39 => Shl,
                40 => Shr,
                41 => UShr,
                42 => Or,
                43 => And,
                44 => Xor,
                45 => Eq,
                46 => Neq,
                47 => Lt,
                48 => Lte,
                49 => Gte,
                51 => Not,
                52 => TypeOf,
                53 => Compare,
                54 => Hash,
                55 => New,
                56 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    JumpTable(i)
                }
                57 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Apply(i)
                }
                58 => PhysCompare,
                59 => {
                    let i = buf.read_i16::<LittleEndian>()?;
                    let i2 = buf.read_i16::<LittleEndian>()?;

                    TailCall(i, i2)
                }
                60 => ToNum,
                61 => Loop,
                62 => Leave,
                63 => {
                    let i = buf.read_i32::<LittleEndian>()?;
                    Ctor(i)
                }
                64 => CloseUpvalue,
                255 => Last,
                x => unreachable!("{}", x),
            };

            ops.push(op);
        }

        Ok(())
    };

    res().unwrap();
    (ops, globals)
}
