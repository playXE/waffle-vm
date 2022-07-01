/// The numeric identifier of the C `void` type.
const TYPE_VOID: i64 = 0;

/// The numeric identifier of the C `void*` type.
const TYPE_POINTER: i64 = 1;

/// The numeric identifier of the C `double` type.
const TYPE_DOUBLE: i64 = 2;

/// The numeric identifier of the C `float` type.
const TYPE_FLOAT: i64 = 3;

/// The numeric identifier of the C `signed char` type.
const TYPE_I8: i64 = 4;

/// The numeric identifier of the C `short` type.
const TYPE_I16: i64 = 5;

/// The numeric identifier of the C `int` type.
const TYPE_I32: i64 = 6;

/// The numeric identifier of the C `long` type.
const TYPE_I64: i64 = 7;

/// The numeric identifier of the C `unsigned char` type.
const TYPE_U8: i64 = 8;

/// The numeric identifier of the C `unsigned short` type.
const TYPE_U16: i64 = 9;

/// The numeric identifier of the C `unsigned int` type.
const TYPE_U32: i64 = 10;

/// The numeric identifier of the C `unsigned long` type.
const TYPE_U64: i64 = 11;

/// The numeric identifier for the C `const char*` type.
const TYPE_STRING: i64 = 12;

/// The numeric identifier for a C `const char*` type that should be read into a
/// byte array..
const TYPE_BYTE_ARRAY: i64 = 13;

/// The numeric identifier of the C `size_t` type.
const TYPE_SIZE_T: i64 = 14;

/// Returns a pointer to a statically allocated FFI type.
macro_rules! ffi_type {
    ($name: ident) => {
        &types::$name as *const ffi_type as *mut ffi_type
    };
}

/// Converts a &T to a *mut c_void pointer.
macro_rules! raw_pointer {
    ($value: expr) => {
        $value as *mut _ as RawPointer
    };
}

macro_rules! ffi_type_error {
    ($type: expr) => {
        return Err(format!("Invalid FFI type: {}", $type))
    };
}

use libffi::low::{
    call as ffi_call, ffi_abi_FFI_DEFAULT_ABI as ABI, ffi_cif, ffi_type, prep_cif, types, CodePtr,
    Error as FFIError,
};
use std::convert::Into;
use std::ffi::{CStr, OsStr};
use std::fmt::{Debug, Display};
use std::mem;
use std::os::raw::{c_char, c_double, c_long, c_void};
use std::ptr;

use crate::builtin::alloc_buffer;
use crate::memory::{Finalize, Managed, Trace};
use crate::value::Value;
use crate::vm::VM;

/// A pointer to an FFI type.
pub(crate) type TypePointer = *mut ffi_type;

/// A raw C pointer.
pub(crate) type RawPointer = *mut c_void;

/// A wrapper around a C pointer.
#[derive(Clone, Copy)]
#[repr(transparent)]
pub(crate) struct Pointer {
    inner: RawPointer,
}

unsafe impl Send for Pointer {}

pub struct Library {
    inner: libloading_mini::Library,
}

unsafe impl Trace for Library {}
unsafe impl Finalize for Library {}
impl Managed for Library {}

/// A function with a fixed number of arguments.
pub(crate) struct Function {
    /// The pointer to the function to call.
    pointer: Pointer,

    /// The CIF (Call Interface) to use for this function.
    cif: ffi_cif,

    /// The argument types of the function.
    arguments: Vec<TypePointer>,

    /// The return type of the function.
    return_type: TypePointer,
}

unsafe impl Trace for Function {}
unsafe impl Finalize for Function {}
impl Managed for Function {}

#[allow(dead_code)]
pub(crate) fn type_size(id: i64) -> Result<Value, String> {
    let size = unsafe {
        match id {
            TYPE_VOID => types::void.size,
            TYPE_POINTER | TYPE_STRING | TYPE_BYTE_ARRAY => types::pointer.size,
            TYPE_DOUBLE => types::double.size,
            TYPE_FLOAT => types::float.size,
            TYPE_I8 => types::sint8.size,
            TYPE_I16 => types::sint16.size,
            TYPE_I32 => types::sint32.size,
            TYPE_I64 => types::sint64.size,
            TYPE_U8 => types::uint8.size,
            TYPE_U16 => types::uint16.size,
            TYPE_U32 => types::uint32.size,
            TYPE_U64 => types::uint64.size,
            TYPE_SIZE_T => mem::size_of::<usize>(),
            _ => ffi_type_error!(id),
        }
    };
    Ok(Value::new(size as i32))
}

/// Returns the alignment of a type ID.
///
/// The alignment of the type is returned as a tagged integer.
#[allow(dead_code)]
pub(crate) fn type_alignment(id: i64) -> Result<Value, String> {
    let size = unsafe {
        match id {
            TYPE_VOID => types::void.alignment,
            TYPE_POINTER | TYPE_STRING | TYPE_BYTE_ARRAY => types::pointer.alignment,
            TYPE_DOUBLE => types::double.alignment,
            TYPE_FLOAT => types::float.alignment,
            TYPE_I8 => types::sint8.alignment,
            TYPE_I16 => types::sint16.alignment,
            TYPE_I32 => types::sint32.alignment,
            TYPE_I64 => types::sint64.alignment,
            TYPE_U8 => types::uint8.alignment,
            TYPE_U16 => types::uint16.alignment,
            TYPE_U32 => types::uint32.alignment,
            TYPE_U64 => types::uint64.alignment,
            TYPE_SIZE_T => mem::align_of::<usize>() as u16,
            _ => ffi_type_error!(id),
        }
    };

    Ok(Value::new(size as i32))
}

/// A value of some sort to be passed to a C function.
pub(crate) enum Argument {
    Pointer(RawPointer),
    Void,
    F32(f32),
    F64(f64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
}

macro_rules! match_ffi_type {
    (
        $pointer: expr,

        $(
            $($type: ident)|+ => $body: expr
        )+
    ) => {
        match $pointer {
            $(
                $(
                    t if t == ffi_type!($type) => { $body }
                )+
            )+
            _ => unreachable!()
        }
    }
}

use crate::memory::gcwrapper::*;
use crate::value::*;
impl Argument {
    unsafe fn wrap(_vm: &mut VM, ffi_type: *mut ffi_type, val: Value) -> Result<Argument, String> {
        let argument = match_ffi_type!(
            ffi_type,
            pointer => {
                Argument::Pointer(if let Some(str) = val.downcast_ref::<Str>() {
                    str.as_c_str().as_ptr() as RawPointer
                } else if let Some(buf) = val.downcast_ref::<ByteBuffer>() {
                    buf.as_ptr() as RawPointer
                } else if let Some(ptr) = val.downcast_ref::<Pointer>() {
                    ptr.inner as RawPointer
                } else {
                    val.0.as_int64 as RawPointer
                })
            }
            void => Argument::Void
            float => Argument::F32(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as f32
            })
            double => Argument::F64(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number()
            })
            sint8 => Argument::I8(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            sint16 => Argument::I16(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            sint32 => Argument::I32(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })

            sint64 => Argument::I64(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            uint8 => Argument::U8(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            uint16 => Argument::U16(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            uint32 => Argument::U32(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {
                val.get_number() as _
            })
            uint64 => Argument::U64(if !val.is_number() {
                return Err(format!("cffi: number expected but '{}' was found",val))
            } else {

                val.get_number() as i64 as u64
            })
        );
        Ok(argument)
    }

    /// Returns a C pointer to the wrapped value.
    fn as_c_pointer(&mut self) -> RawPointer {
        match self {
            Argument::Pointer(ref mut val) => {
                // When passing a pointer we shouldn't pass the pointer
                // directly, instead we want a pointer to the pointer to pass to
                // the underlying C function.
                val as *mut RawPointer as RawPointer
            }
            Argument::Void => ptr::null_mut() as RawPointer,
            Argument::F32(ref mut val) => raw_pointer!(val),
            Argument::F64(ref mut val) => raw_pointer!(val),
            Argument::I8(ref mut val) => raw_pointer!(val),
            Argument::I16(ref mut val) => raw_pointer!(val),
            Argument::I32(ref mut val) => raw_pointer!(val),
            Argument::I64(ref mut val) => raw_pointer!(val),
            Argument::U8(ref mut val) => raw_pointer!(val),
            Argument::U16(ref mut val) => raw_pointer!(val),
            Argument::U32(ref mut val) => raw_pointer!(val),
            Argument::U64(ref mut val) => raw_pointer!(val),
        }
    }
}

/// Returns an FFI type for an integer pointer.
unsafe fn ffi_type_for(pointer: Value) -> Result<TypePointer, String> {
    let int = pointer.get_int32() as i64;
    let typ = match int {
        TYPE_VOID => ffi_type!(void),
        TYPE_POINTER | TYPE_STRING | TYPE_BYTE_ARRAY => ffi_type!(pointer),
        TYPE_DOUBLE => ffi_type!(double),
        TYPE_FLOAT => ffi_type!(float),
        TYPE_I8 => ffi_type!(sint8),
        TYPE_I16 => ffi_type!(sint16),
        TYPE_I32 => ffi_type!(sint32),
        TYPE_I64 => ffi_type!(sint64),
        TYPE_U8 => ffi_type!(uint8),
        TYPE_U16 => ffi_type!(uint16),
        TYPE_U32 => ffi_type!(uint32),
        TYPE_U64 => ffi_type!(uint64),
        TYPE_SIZE_T => {
            match mem::size_of::<usize>() * 8 {
                64 => ffi_type!(uint64),
                32 => ffi_type!(uint32),
                8 => ffi_type!(uint8),

                // The C spec states that `size_t` is at least 16 bits, so we
                // can use this as the default.
                _ => ffi_type!(uint16),
            }
        }
        _ => ffi_type_error!(int),
    };

    Ok(typ as TypePointer)
}

impl Library {
    pub(crate) fn open<P: AsRef<OsStr> + Debug + Display>(search_for: &[P]) -> Option<Self> {
        for name in search_for {
            if let Some(library) =
                libloading_mini::Library::new(name).map(|inner| Library { inner })
            {
                return Some(library);
            }
        }
        None
    }

    /// Obtains a pointer to a symbol.
    ///
    /// This method is unsafe because the pointer could be of any type, thus it
    /// is up to the caller to make sure the result is used appropriately.
    pub(crate) unsafe fn get(&self, name: &str) -> Option<Pointer> {
        self.inner
            .get(name.as_bytes())
            .map(|ptr| Pointer::new(ptr as _))
    }
}

#[allow(dead_code)]
impl Pointer {
    pub(crate) fn new(inner: RawPointer) -> Self {
        Pointer { inner }
    }

    /// Returns a new Pointer, optionally starting at the given offset.
    ///
    /// The `offset` argument is the offset in _bytes_, not the number of
    /// elements (unlike Rust's `pointer::offset`).
    pub(crate) fn with_offset(self, offset_bytes: usize) -> Self {
        let inner = (self.inner as usize + offset_bytes) as RawPointer;

        Pointer::new(inner)
    }

    /// Returns the underlying pointer.
    pub(crate) fn as_ptr(self) -> *mut u8 {
        self.inner as _
    }

    unsafe fn read<R>(self) -> R {
        ptr::read(self.inner as *mut R)
    }

    unsafe fn write<T>(self, value: T) {
        ptr::write(self.inner as *mut T, value);
    }

    unsafe fn read_signed_integer<T: Into<i64>>(self) -> Value {
        Value::new(self.read::<T>().into() as i32)
    }

    unsafe fn read_float<T: Into<f64>>(self) -> Value {
        Value::Float(self.read::<T>().into())
    }

    unsafe fn read_cstr<'a>(self) -> &'a CStr {
        CStr::from_ptr(self.inner as *mut c_char)
    }

    pub(crate) unsafe fn read_as(self, vm: &mut VM, kind: Value) -> Result<Value, String> {
        let pointer = match kind.get_int32() as i64 {
            TYPE_POINTER => {
                let ptr = self.read::<RawPointer>();
                Value::encode_object_value(vm.gc().fixed(Pointer::new(ptr as *mut _)))
            }
            TYPE_BYTE_ARRAY => {
                let bytes = self.read_cstr().to_bytes().to_vec();

                let mut buf = alloc_buffer(vm, bytes.len());
                buf.copy_from_slice(&bytes);
                Value::encode_object_value(buf)
            }
            TYPE_STRING => {
                let string = self.read_cstr().to_string_lossy().into_owned();
                Value::encode_object_value(vm.gc().str(string))
            }
            TYPE_FLOAT => Value::Float(self.read::<f32>() as _),
            TYPE_DOUBLE => Value::Float(self.read::<f64>() as _),
            TYPE_I8 | TYPE_U8 => Value::new(self.read::<i8>() as i32),
            TYPE_I16 | TYPE_U16 => Value::new(self.read::<i16>() as i32),
            TYPE_I32 | TYPE_U32 => Value::new(self.read::<i32>() as i32),
            TYPE_I64 | TYPE_U64 => Value::new(self.read::<i64>() as f64),
            TYPE_SIZE_T => Value::new(self.read::<usize>() as i32),
            _ => ffi_type_error!(kind.get_int32() as i64),
        };
        Ok(pointer)
    }

    pub(crate) unsafe fn write_as(self, kind: Value, value: Value) -> Result<(), String> {
        let int = kind.get_int32() as i64;
        match int {
            TYPE_STRING => {
                if let Some(str) = kind.downcast_ref::<Str>() {
                    core::ptr::copy(str.as_ptr(), self.inner as *mut u8, str.len());
                    Ok(())
                } else {
                    ffi_type_error!(int);
                }
            }
            TYPE_BYTE_ARRAY => {
                if let Some(str) = kind.downcast_ref::<ByteBuffer>() {
                    core::ptr::copy(str.as_ptr(), self.inner as *mut u8, str.len());
                    Ok(())
                } else {
                    ffi_type_error!(int);
                }
            }
            TYPE_POINTER => {
                if let Some(ptr) = kind.downcast_ref::<Pointer>() {
                    self.write(ptr.inner as RawPointer);
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            TYPE_DOUBLE => {
                if value.is_number() {
                    self.write(value.get_number());
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            TYPE_FLOAT => {
                if value.is_number() {
                    self.write(value.get_number() as f32);
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }

            TYPE_I8 | TYPE_U8 => {
                if value.is_int32() {
                    self.write(value.get_int32() as u8);
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            TYPE_I16 | TYPE_U16 => {
                if value.is_int32() {
                    self.write(value.get_int32() as u16);
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            TYPE_I32 | TYPE_U32 => {
                if value.is_int32() {
                    self.write(value.get_int32());
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            TYPE_I64 | TYPE_U64 => {
                if value.is_int32() {
                    self.write(value.get_int32() as i64);
                    Ok(())
                } else {
                    ffi_type_error!(int)
                }
            }
            _ => ffi_type_error!(int),
        }
    }
}

#[derive(Copy, Clone)]
pub struct PointerObject(pub *mut u8);

unsafe impl Trace for Pointer {}
unsafe impl Finalize for Pointer {}
impl Managed for Pointer {}

impl Function {
    /// Creates a new function using object pointers.
    pub(crate) unsafe fn attach(
        library: &Library,
        name: &str,
        arguments: &[Value],
        return_type: Value,
    ) -> Result<Option<Function>, String> {
        let func_ptr = if let Some(sym) = library.get(name) {
            sym
        } else {
            return Ok(None);
        };
        let ffi_rtype = ffi_type_for(return_type)?;
        let mut ffi_arg_types = Vec::with_capacity(arguments.len());

        for ptr in arguments {
            ffi_arg_types.push(ffi_type_for(*ptr)?);
        }

        Self::create(func_ptr, ffi_arg_types, ffi_rtype)
            .map_err(|e| e.into())
            .map(|f| Some(f))
    }

    /// Creates a new prepared function.
    unsafe fn create(
        pointer: Pointer,
        arguments: Vec<TypePointer>,
        return_type: TypePointer,
    ) -> Result<Function, String> {
        let mut func = Function {
            pointer,
            cif: Default::default(),
            arguments,
            return_type,
        };

        let result = prep_cif(
            &mut func.cif,
            ABI,
            func.arguments.len(),
            func.return_type,
            func.arguments.as_mut_ptr(),
        );

        match result {
            Ok(_) => Ok(func),
            Err(FFIError::Typedef) => {
                Err("The type representation is invalid or unsupported".to_string())
            }
            Err(FFIError::Abi) => Err("The ABI is invalid or unsupported".to_string()),
        }
    }

    /// Calls the function with the given arguments.
    pub(crate) unsafe fn call(&self, vm: &mut VM, arg_ptrs: &[Value]) -> Result<Value, String> {
        if arg_ptrs.len() != self.arguments.len() {
            return Err(format!(
                "Invalid number of arguments, expected {} but got {}",
                self.arguments.len(),
                arg_ptrs.len()
            ));
        }

        let mut arguments = Vec::with_capacity(arg_ptrs.len());

        for (index, arg) in arg_ptrs.iter().enumerate() {
            let wrapped = Argument::wrap(vm, self.arguments[index], *arg)?;

            arguments.push(wrapped);
        }

        // libffi expects an array of _pointers_ to the arguments to pass,
        // instead of an array containing the arguments directly. The pointers
        // and the values they point to must outlive the FFI call, otherwise we
        // may end up passing pointers to invalid memory.
        let mut argument_pointers: Vec<RawPointer> =
            arguments.iter_mut().map(Argument::as_c_pointer).collect();

        // libffi requires a mutable pointer to the CIF, but "self" is immutable
        // since we never actually modify the current function. To work around
        // this we manually cast to a mutable pointer.
        let cif_ptr = &self.cif as *const _ as *mut _;
        let fun_ptr = CodePtr::from_ptr(self.pointer.inner);
        let args_ptr = argument_pointers.as_mut_ptr();

        // Instead of reading the result into some kind of generic pointer (*mut
        // c_void for example) and trying to cast that to the right type, we'll
        // immediately read the call's return value into the right type. This
        // requires a bit more code, but is much less unsafe than trying to cast
        // types from X to Y without knowing if this even works reliably.
        let pointer = match_ffi_type!(
            self.return_type,
            pointer => {
                Value::encode_object_value(vm.gc().fixed(Pointer::new(ffi_call(cif_ptr, fun_ptr, args_ptr))))
            }
            void => {
                ffi_call::<c_void>(cif_ptr, fun_ptr, args_ptr);

                Value::Null
            }
            double | float => {
                let result: c_double = ffi_call(cif_ptr, fun_ptr, args_ptr);

                Value::Float(
                    result as f64
                )
            }
            sint8 | sint16 | sint32 | sint64 | uint8 | uint16 | uint32 | uint64 => {
                let result: c_long = ffi_call(cif_ptr, fun_ptr, args_ptr);

                Value::new(
                    result as u32 as i32
                )
            }
        );

        Ok(pointer)
    }
}
