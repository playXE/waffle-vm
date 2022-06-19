use std::mem::size_of;

use memoffset::offset_of;

use super::{Visitor, Allocation};
use crate::{value::Value,};
use super::{gcwrapper::Gc, Trace, Finalize, Managed};
use super::gcwrapper::GCWrapper;
///
/// A GC-managed resizable vector of values. It is used for storage of property
/// values in objects and also indexed property values in arrays. It suppoctxs
/// resizing on both ends which is necessary for the simplest implementation of
/// JavaScript arrays (using a base offset and length).
#[repr(C)]
pub struct ArrayStorage {
    pub(crate) size: usize,
    pub(crate) capacity: usize,
    pub(crate) data: [Value; 0],
}

impl Gc<ArrayStorage> {
    pub fn resize_within_capacity(&mut self, _vm: &mut GCWrapper, new_size: usize) {
        assert!(
            new_size <= self.capacity(),
            "new_size must be <= capacity in resize_Within_capacity"
        );

        let sz = self.size();
        unsafe {
            if new_size > sz {
                Value::fill(
                    self.data_mut().add(sz as _),
                    self.data_mut().add(new_size as _),
                    Value::encode_empty_value(),
                );
            }
        }
        self.size = new_size;
    }


    pub unsafe fn reallocate_to_larger(
        &mut self,
        vm: &mut GCWrapper,
        capacity: usize,
        from_first: usize,
        to_first: usize,
        to_last: usize,
    ) {
        assert!(capacity > self.capacity());

        let mut arr_res = ArrayStorage::new(vm, capacity);
        let copy_size = std::cmp::min(self.size() - from_first, to_last - to_first);

        {
            let from = self.data_mut().add(from_first as _);
            let to = arr_res.data_mut().add(to_first as _);
            Value::uninit_copy(from, from.add(copy_size as _), to);
        }

        Value::fill(
            arr_res.data_mut(),
            arr_res.data_mut().add(to_first as _),
            Value::encode_empty_value(),
        );

        if to_first + copy_size < to_last {
            Value::fill(
                arr_res
                    .data_mut()
                    .add(to_first as usize + copy_size as usize),
                arr_res.data_mut().add(to_last as usize),
                Value::encode_empty_value(),
            );
        }

        arr_res.size = to_last;
        *self = arr_res;
    }

    pub fn shift(&mut self, ctx: &mut GCWrapper, from_first: usize, to_first: usize, to_last: usize) {
        assert!(to_first <= to_last, "First must be before last");
        assert!(from_first <= self.size, "from_first must be before size");
        unsafe {
            if to_last <= self.capacity() {
                let copy_size = std::cmp::min(self.size() - from_first, to_last - to_first);
                if from_first > to_first {
                    Value::copy(
                        self.data_mut().add(from_first as usize),
                        self.data_mut()
                            .add(from_first as usize + copy_size as usize),
                        self.data_mut().add(to_first as usize),
                    );
                } else if from_first < to_first {
                    Value::copy_backward(
                        self.data_mut().add(from_first as usize),
                        self.data_mut()
                            .add(from_first as usize + copy_size as usize),
                        self.data_mut().add(to_first as _),
                    );
                }
                Value::fill(
                    self.data_mut().add(to_first as usize + copy_size as usize),
                    self.data_mut().add(to_last as usize),
                    Value::encode_empty_value(),
                );
                self.size = to_last;
                return;
            }

            let mut capacity = self.capacity();
            if capacity < ArrayStorage::max_elements()  / 2 {
                capacity = std::cmp::max(capacity * 2, to_last);
            } else {
                capacity = ArrayStorage::max_elements();
            }
            self.reallocate_to_larger(ctx, capacity, from_first, to_first, to_last)
        }
    }

    pub fn ensure_capacity(&mut self, ctx: &mut GCWrapper, capacity: usize) {
        assert!(
            capacity <= ArrayStorage::max_elements(),
            "capacity overflows 32-bit storage"
        );

        if capacity <= self.capacity() {
            return;
        }

        unsafe { self.reallocate_to_larger(ctx, capacity, 0, 0, self.size()) }
    }
    pub fn resize(&mut self, ctx: &mut GCWrapper, new_size: usize) {
        self.shift(ctx, 0, 0, new_size)
    }

    #[cold]
    pub fn push_back_slowpath(&mut self, ctx: &mut GCWrapper, value: Value) {
        let size = self.size();

        self.resize(ctx, self.size() + 1);
        *self.at_mut(size) = value;
    }

    pub fn push_back(&mut self, ctx: &mut GCWrapper, value: Value) {
        let currsz = self.size();
        if currsz < self.capacity() {
            unsafe {
                self.data_mut().add(currsz as _).write(value);
                self.size = currsz + 1;
            }
            return;
        }
        self.push_back_slowpath(ctx, value)
    }

    pub fn pop_back(&mut self, _ctx: &mut GCWrapper) -> Value {
        let sz = self.size();
        assert!(sz > 0, "empty ArrayStorage");

        unsafe {
            let val = self.data().add(sz as usize - 1).read();
            self.size = sz - 1;
            val
        }
    }
}

impl ArrayStorage {
    pub fn data(&self) -> *const Value {
        self.data.as_ptr()
    }
    pub fn as_slice(&self) -> &[Value] {
        unsafe { std::slice::from_raw_parts(self.data(), self.size as _) }
    }

    pub fn data_mut(&mut self) -> *mut Value {
        self.data.as_mut_ptr()
    }
    pub fn as_slice_mut(&mut self) -> &mut [Value] {
        unsafe { std::slice::from_raw_parts_mut(self.data_mut(), self.size as _) }
    }
    pub fn at(&self, index: usize) -> &Value {
        assert!(index < self.size(), "index out of range");
        unsafe { &*self.data().add(index as _) }
    }
    pub fn at_mut(&mut self, index: usize) -> &mut Value {
        assert!(index < self.size(), "index out of range");
        unsafe { &mut *self.data_mut().add(index as _) }
    }

    pub fn max_elements() -> usize {
        (usize::MAX as usize - 8) / size_of::<Value>()
    }
    pub fn size(&self) -> usize {
        self.size
    }

    pub fn capacity(&self) -> usize {
        self.capacity
    }

    pub fn is_empty(&self) -> bool {
        self.size == 0
    }

    pub fn with_size(vm: &mut GCWrapper, size: usize, mut capacity: usize) -> Gc<Self >{
        if capacity < size {
            capacity = size;
        }
        let mut ary = Self::new(vm, capacity);
        ary.resize_within_capacity(vm, size);
        ary
    }

    pub fn new(vm: &mut GCWrapper, capacity: usize) -> Gc<Self> {
        unsafe {
            let ary = vm.malloc_varsize::<Self>(capacity, &mut []);
            let ptr = ary.as_mut_ptr();
            (*ptr).size = 0;
            (*ptr).capacity = capacity;
            ary.assume_init()
        }
    }
}

unsafe impl Trace for ArrayStorage {
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        self.as_slice_mut().iter_mut().for_each(|value| {
            value.trace(visitor);
        });
    }
}
unsafe impl Finalize for ArrayStorage {}
unsafe impl Allocation for ArrayStorage {
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = offset_of!(ArrayStorage,capacity);
    const VARSIZE_OFFSETOF_VARPART: usize = offset_of!(ArrayStorage,data);
    const VARSIZE_ITEM_SIZE: usize = size_of::<Value>();
}
impl Managed for ArrayStorage {}