use std_::{
    mem::MaybeUninit,
    ops::{Deref, DerefMut},
    ptr::null_mut,
};

use super::{
    minimark::{MiniMark, GROWTH_RATE_MAX, MAJOR_COLLECTION_THRESHOLD},
    roots::StackChain,
    *,
};
use std::{marker::PhantomData, ptr::NonNull};

#[repr(transparent)]
pub struct Gc<T: Object + ?Sized> {
    pub(crate) header: NonNull<HeapObjectHeader>,
    marker: PhantomData<*mut T>,
}

#[repr(C)]
pub(crate) struct WeakInner {
    weakptr: Option<NonNull<HeapObjectHeader>>,
}

unsafe impl Allocation for WeakInner {
    const HAS_WEAKPTR: bool = true;
}

unsafe impl Finalize for WeakInner {}
unsafe impl Trace for WeakInner {}
impl Object for WeakInner {}
pub struct WeakRef<T: Object + ?Sized> {
    pub(crate) value: Gc<WeakInner>,
    marker: PhantomData<*mut T>,
}

impl<T: Object + ?Sized> WeakRef<T> {
    pub fn upgrade(self) -> Option<Gc<T>> {
        self.value.weakptr.map(|p| Gc {
            header: p,
            marker: PhantomData,
        })
    }
}

impl<T: Object + ?Sized> Copy for WeakRef<T> {}
impl<T: Object + ?Sized> Clone for WeakRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

unsafe impl<T: Object + ?Sized> Trace for WeakRef<T> {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        mark(vis, &mut self.value);
    }
}

unsafe impl<T: Trace> Trace for MaybeUninit<T> {
    fn trace(&mut self, _: &mut dyn Visitor) {
        unreachable!("trying to trace uninit");
    }
}

unsafe impl<T: Finalize> Finalize for MaybeUninit<T> {
    fn finalize(&mut self) {
        unreachable!("trying to finalize uninit");
    }
}

impl<T: Object> Object for MaybeUninit<T> {}

impl<T: Object> Gc<MaybeUninit<T>> {
    pub unsafe fn assume_init(self) -> Gc<T> {
        Gc {
            header: self.header,
            marker: PhantomData,
        }
    }

    pub fn as_mut_ptr(self) -> *mut T {
        unsafe { self.header.as_ptr().add(1).cast() }
    }

    pub fn as_ptr(self) -> *const T {
        unsafe { self.header.as_ptr().add(1).cast() }
    }
}

impl<T: Object + ?Sized> Copy for Gc<T> {}
impl<T: Object + ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Object> Deref for Gc<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.header.as_ptr().add(1).cast::<T>() }
    }
}

impl<T: Object> DerefMut for Gc<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.header.as_ptr().add(1).cast::<T>() }
    }
}

unsafe impl<T: Object + ?Sized> Trace for Gc<T> {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        mark(vis, self);
    }
}

pub struct GCWrapper(Box<MiniMark>);

impl GCWrapper {
    pub fn new() -> Self {
        Self(MiniMark::new(
            MAJOR_COLLECTION_THRESHOLD,
            GROWTH_RATE_MAX,
            (16384 + 512) * WORD,
        ))
    }
    /// Allocates fixed sized object on the heap
    #[inline]
    pub unsafe fn malloc_fixedsize<A: 'static + Allocation + Object>(
        &mut self,
        safestack: &mut [&mut dyn Trace],
    ) -> Gc<MaybeUninit<A>> {
        let memory = self.0.malloc_fixedsize::<A>(safestack);
        Gc {
            header: NonNull::new_unchecked(memory),
            marker: PhantomData,
        }
    }

    /// Allocates variable-sized object on the heap
    #[inline]
    pub unsafe fn malloc_varsize<A: 'static + Allocation + Object>(
        &mut self,
        length: usize,
        safestack: &mut [&mut dyn Trace],
    ) -> Gc<MaybeUninit<A>> {
        Gc {
            header: NonNull::new_unchecked(self.0.malloc_varsize::<A>(length, safestack)),
            marker: PhantomData,
        }
    }
    pub fn array<T: 'static + Trace + Clone>(
        &mut self,
        length: usize,
        mut init: T,
    ) -> Gc<Array<T>> {
        unsafe {
            let uninit = self.malloc_varsize::<Array<T>>(length, &mut [&mut init]);
            let arr = uninit.as_mut_ptr();
            let ptr = (*arr).data.as_mut_ptr();
            for i in 0..length {
                ptr.add(i).write(init.clone());
            }
            uninit.assume_init()
        }
    }
    pub fn str(&mut self, str: impl AsRef<str>) -> Gc<Str> {
        unsafe {
            let str = str.as_ref();
            let uninit = self.malloc_varsize::<Str>(str.len() + 1, &mut []);
            let ptr = uninit.as_mut_ptr();

            let ptr = (*ptr).data.as_mut_ptr();

            core::ptr::copy_nonoverlapping(str.as_ptr(), ptr, str.len());
            ptr.add(str.len()).write(0);
            uninit.assume_init()
        }
    }
    pub fn weak<T: Object + ?Sized>(&mut self, value: Gc<T>) -> WeakRef<T> {
        let p = self.fixed(WeakInner {
            weakptr: Some(value.header),
        });
        WeakRef {
            value: p,
            marker: PhantomData,
        }
    }

    pub fn weak_null<T: Object + ?Sized>(&mut self) -> WeakRef<T> {
        WeakRef {
            value: self.fixed(WeakInner { weakptr: None }),
            marker: PhantomData,
        }
    }
    /// Allocates fixed sized object on the heap and initializes it with `value`.
    #[inline]
    pub fn fixed<A: 'static + Allocation + Object>(&mut self, mut value: A) -> Gc<A> {
        unsafe {
            let uninit = self.malloc_fixedsize::<A>(&mut [&mut value]);

            uninit.as_mut_ptr().write(value);
            uninit.assume_init()
        }
    }
    pub fn add_trace_callback<F: 'static + FnMut(&mut dyn Visitor)>(&mut self, x: F) -> u32 {
        self.0.add_trace_callback(x)
    }

    pub fn remove_trace_callback(&mut self, key: u32) -> bool {
        self.0.remove_trace_callback(key)
    }
    pub fn roots(&self) -> *mut StackChain {
        self.0.roots.get()
    }

    pub fn collect(&mut self, gen: u8, safestack: &mut [&mut dyn Trace]) {
        self.0.collect(gen, safestack);
    }

    pub fn minor_collection(&mut self, safestack: &mut [&mut dyn Trace]) {
        self.0.minor_collection(safestack);
    }
    #[inline]
    pub fn write_barrier(&mut self, object: Gc<impl Object>) {
        unsafe {
            self.0.write_barrier(object.header.as_ptr());
        }
    }

    #[inline]
    pub fn identity<T: ?Sized + Object>(&mut self, value: Gc<T>) -> usize {
        self.0.identity(value.header.as_ptr())
    }

    #[inline]
    pub fn identity_weak(&mut self, value: WeakRef<impl Object>) -> usize {
        self.identity(value.value)
    }
}

macro_rules! impl_prim {
    ($($t: ty)*) => {
        $(
            unsafe impl Trace for $t {}
            unsafe impl Finalize for $t {}
            impl Object for $t {}
        )*
    };
}

impl_prim!(u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 bool f32 f64);

use std::fmt;

impl<T: Object + ?Sized> fmt::Pointer for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:p}", self.header)
    }
}

#[repr(C)]
pub struct Array<T: Trace> {
    length: usize,
    data: [T; 0],
}

impl<T: Trace> Deref for Array<T> {
    type Target = [T];
    fn deref(&self) -> &Self::Target {
        unsafe { core::slice::from_raw_parts(self.data.as_ptr(), self.length) }
    }
}

impl<T: Trace> DerefMut for Array<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { core::slice::from_raw_parts_mut(self.data.as_mut_ptr(), self.length) }
    }
}

unsafe impl<T: Trace> Finalize for Array<T> {
    fn finalize(&mut self) {
        unsafe {
            core::ptr::drop_in_place(std::slice::from_raw_parts_mut(
                self.data.as_mut_ptr(),
                self.length,
            ));
        }
    }
}

unsafe impl<T: Trace> Trace for Array<T> {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        unsafe {
            let mut i = 0;
            while i < self.length {
                (*self.data.as_mut_ptr().add(i)).trace(vis);
                i += 1;
            }
        }
    }
}

impl<T: Trace> Object for Array<T> {}

unsafe impl<T: Trace> Allocation for Array<T> {
    const LIGHT_FINALIZER: bool = true;
    const FINALIZE: bool = std::mem::needs_drop::<T>();
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Array::<T>, length);
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Array::<T>, data);
    const VARSIZE_ITEM_SIZE: usize = size_of::<T>();
}

#[repr(C)]
pub struct Str {
    length: usize,
    data: [u8; 0],
}
impl Str {
    pub const fn len(&self) -> usize {
        self.length - 1
    }

    pub const fn len_with_null(&self) -> usize {
        self.length
    }

    pub fn as_c_str<'a>(&'a self) -> &'a std::ffi::CStr {
        unsafe { std::ffi::CStr::from_ptr(self.data.as_ptr() as _) }
    }
}
impl Deref for Str {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.data.as_ptr(),
                self.length - 1,
            ))
        }
    }
}

unsafe impl Allocation for Str {
    const LIGHT_FINALIZER: bool = false;
    const FINALIZE: bool = false;
    const VARSIZE: bool = true;
    const VARSIZE_OFFSETOF_LENGTH: usize = memoffset::offset_of!(Str, length);
    const VARSIZE_OFFSETOF_VARPART: usize = memoffset::offset_of!(Str, data);
    const VARSIZE_ITEM_SIZE: usize = 1;
}

unsafe impl Trace for Str {}
unsafe impl Finalize for Str {}

impl Object for Str {}

unsafe impl<T: Trace> Trace for Option<T> {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        match self {
            Some(x) => x.trace(vis),
            _ => (),
        }
    }
}

impl Gc<dyn Object> {
    pub fn downcast<T: 'static + Object>(self) -> Option<Gc<T>> {
        unsafe {
            if self.header.as_ref().tid().as_vtable.type_id == TypeId::of::<T>() {
                Some(Gc {
                    header: self.header,
                    marker: PhantomData,
                })
            } else {
                None
            }
        }
    }

    pub unsafe fn downcast_unchecked<T: 'static + Object>(self) -> Gc<T> {
        self.downcast().unwrap_unchecked()
    }

    pub fn is<T: 'static + Object>(self) -> bool {
        self.downcast::<T>().is_some()
    }
}

impl<T: Object + ?Sized> Gc<T> {
    pub fn as_dyn(self) -> Gc<dyn Object> {
        Gc {
            header: self.header,
            marker: PhantomData,
        }
    }

    pub fn get_size(this: Self) -> usize {
        unsafe {
            let vt = this.header.as_ref().tid().as_vtable;
            let fixedsize = size_of::<HeapObjectHeader>() + vt.size;
            fixedsize
                + if this.header.as_ref().tid().as_vtable.is_varsize {
                    let lenoff = vt.varsize.offset_of_length;
                    let len = this
                        .header
                        .as_ptr()
                        .add(1)
                        .cast::<u8>()
                        .add(lenoff)
                        .cast::<usize>()
                        .read();
                    len * vt.varsize.itemsize
                } else {
                    0
                }
        }
    }

    pub fn nullable(self) -> Nullable<T> {
        Nullable {
            ptr: self.header.as_ptr(),
            marker: PhantomData,
        }
    }
}

#[repr(transparent)]
pub struct Nullable<T: Object + ?Sized> {
    ptr: *mut HeapObjectHeader,
    marker: PhantomData<*mut T>,
}

impl<T: Object + ?Sized> Copy for Nullable<T> {}
impl<T: Object + ?Sized> Clone for Nullable<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T: Object + ?Sized> Nullable<T> {
    pub const NULL: Self = Self {
        ptr: null_mut(),
        marker: PhantomData,
    };

    pub fn as_dyn(self) -> Nullable<dyn Object> {
        Nullable {
            ptr: self.ptr,
            marker: PhantomData,
        }
    }

    pub fn as_gc(self) -> Gc<T> {
        Gc {
            header: NonNull::new(self.ptr).expect("null reference"),
            marker: self.marker,
        }
    }

    pub fn is_null(self) -> bool {
        self.ptr.is_null()
    }

    pub fn is_not_null(self) -> bool {
        !self.ptr.is_null()
    }
}

impl Nullable<dyn Object> {
    pub fn downcast<T: 'static + Object>(self) -> Nullable<T> {
        unsafe {
            assert!(!self.ptr.is_null(), "attempt to downcast null reference");
            if (*self.ptr).tid().as_vtable.type_id == TypeId::of::<T>() {
                Nullable {
                    ptr: self.ptr,
                    marker: PhantomData,
                }
            } else {
                Nullable::NULL
            }
        }
    }

    pub fn is<T: 'static + Object>(self) -> bool {
        !self.downcast::<T>().is_null()
    }
}

unsafe impl<T: Object + ?Sized> Trace for Nullable<T> {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        if !self.is_null() {
            unsafe {
                vis.mark_object(std::mem::transmute(&mut self.ptr));
            }
        }
    }
}

impl<T: Object + Sized> Deref for Nullable<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        assert!(!self.ptr.is_null(), "trying to read null reference");
        unsafe { &*self.ptr.add(1).cast::<T>() }
    }
}
impl<T: Object> DerefMut for Nullable<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        assert!(!self.ptr.is_null(), "trying to read null reference");
        unsafe { &mut *self.ptr.add(1).cast::<T>() }
    }
}
impl<T: Object> std::fmt::Pointer for Nullable<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", self.ptr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::gc_frame;

    #[test]
    fn weak_refs() {
        let mut gc = GCWrapper::new();

        let mut p = gc.fixed(42i32);
        let p2 = gc.fixed(44i32);
        let mut w1 = gc.weak(p);
        let mut w2 = gc.weak(p2);
        gc_frame!(gc.roots() => p: Gc<i32>, w1: WeakRef<i32>, w2 : WeakRef<i32>);

        gc.minor_collection(&mut []);

        assert!(w1.as_ref().upgrade().map(|x| *x) == Some(42));
        assert!(w2.as_ref().upgrade().is_none());
        assert!(**p.as_ref() == 42);

        gc.collect(2, &mut []);
        assert!(w1.as_ref().upgrade().map(|x| *x) == Some(42));
        assert!(w2.as_ref().upgrade().is_none());
        assert!(**p.as_ref() == 42);
    }
    #[test]
    fn weak_refs_generational() {
        let mut gc = GCWrapper::new();
        let mut p = gc.array(1, None);
        gc_frame!(gc.roots() => p: Option<Gc<i32>>);
        gc.minor_collection(&mut []); // `p` should be now in old space
        {
            // without write-barrier weak ref should become null
            let p1 = gc.fixed(1);
            let mut w1 = gc.weak(p1);
            p.as_mut()[0] = Some(p1);

            gc.minor_collection(&mut [&mut w1]); // do full GC

            assert!(
                w1.upgrade().is_none(),
                "without write barrier object should be dead"
            );
            p.as_mut()[0] = None;
        }

        {
            let p1 = gc.fixed(42i32);
            let mut w1 = gc.weak(p1);
            p.as_mut()[0] = Some(p1);
            gc.write_barrier(*p.as_ref());
            gc.minor_collection(&mut [&mut w1]); // do full GC

            assert!(
                *w1.upgrade().unwrap() == 42,
                "with write barrier object should stay alive"
            );
        }
    }
}

unsafe impl<T: Trace> Trace for [T] {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        for item in self.iter_mut() {
            item.trace(vis);
        }
    }
}

unsafe impl<const N: usize, T: Trace> Trace for [T; N] {
    fn trace(&mut self, vis: &mut dyn Visitor) {
        for item in self.iter_mut() {
            item.trace(vis);
        }
    }
}

unsafe impl Trace for () {}

unsafe impl Trace for std::fs::File {}
unsafe impl Finalize for std::fs::File {}
impl Object for std::fs::File {}
