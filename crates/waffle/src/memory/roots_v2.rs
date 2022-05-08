use super::{Trace, Visitor};
use std::ops::{Deref, DerefMut};
use std::{marker::PhantomData, mem::MaybeUninit};

pub unsafe fn visit_roots(mut root: *mut Rooted<()>, vis: &mut dyn Visitor) {
    while !root.is_null() {
        let ptr = (*root).ptr.as_mut_ptr();
        ((*root).trace)(ptr, vis);
        root = (*root).prev;
    }
}

#[repr(C)]
pub struct Rooted<T: Trace> {
    stack: *mut *mut Rooted<()>,
    prev: *mut Rooted<()>,
    trace: fn(*mut (), &mut dyn Visitor),
    ptr: MaybeUninit<T>,
}
impl<T: Trace> Rooted<T> {
    const _X: () = {
        assert!(
            std::mem::align_of::<T>() == std::mem::align_of::<usize>(),
            "T must have an alignment of word"
        );
    };
    unsafe fn add_to_root_stack(&mut self, stack: *mut *mut Rooted<()>) {
        self.stack = stack;
        self.prev = stack.read();
        self.trace = erase_trace::<T>;
        self.stack.write(self as *mut Self as *mut Rooted<()>);
    }

    unsafe fn remove_from_root_stack(&mut self) {
        self.stack.write(self.prev);
    }
}

fn erase_trace<T: Trace>(p: *mut (), v: &mut dyn Visitor) {
    unsafe {
        let x = &mut *p.cast::<T>();
        x.trace(v);
    }
}

pub struct RootedGuard<'a, T: 'a + Trace> {
    root: &'a mut Rooted<T>,
}
impl<'a, T: 'a + Trace> RootedGuard<'a, T> {
    pub fn new(stack: *mut *mut Rooted<()>, root: &'a mut Rooted<T>, initial: T) -> Self {
        unsafe {
            root.ptr.as_mut_ptr().write(initial);
            root.add_to_root_stack(stack);
        }
        Self { root }
    }

    pub fn handle(&'a self) -> Handle<'a, T> {
        unsafe { Handle::new(&*self.root.ptr.as_ptr()) }
    }

    pub fn handle_mut(&mut self) -> MutableHandle<T> {
        unsafe { MutableHandle::from_marked_location(&mut *self.root.ptr.as_mut_ptr()) }
    }

    pub fn get(&self) -> T
    where
        T: Copy,
    {
        unsafe { self.root.ptr.as_ptr().read() }
    }

    pub fn set(&mut self, v: T) {
        unsafe {
            *&mut *self.root.ptr.as_mut_ptr() = v;
        }
    }
}

impl<'a, T: 'a + Trace> Drop for RootedGuard<'a, T> {
    fn drop(&mut self) {
        unsafe {
            self.root.remove_from_root_stack();
            core::ptr::drop_in_place(self.root.ptr.as_mut_ptr());
        }
    }
}

pub struct Handle<'a, T: 'a> {
    ptr: &'a T,
}

pub struct MutableHandle<'a, T: 'a> {
    ptr: *mut T,
    anchor: PhantomData<&'a mut T>,
}

impl<'a, T> Handle<'a, T> {
    pub fn get(&self) -> T
    where
        T: Copy,
    {
        *self.ptr
    }

    fn new(ptr: &'a T) -> Self {
        Handle { ptr: ptr }
    }

    pub unsafe fn from_marked_location(ptr: *const T) -> Self {
        Handle::new(&*ptr)
    }
}

impl<'a, T> Deref for Handle<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.ptr
    }
}

impl<'a, T> MutableHandle<'a, T> {
    pub unsafe fn from_marked_location(ptr: *mut T) -> Self {
        MutableHandle::new(&mut *ptr)
    }

    pub fn handle(&self) -> Handle<T> {
        unsafe { Handle::new(&*self.ptr) }
    }

    pub fn new(ptr: &'a mut T) -> Self {
        Self {
            ptr,
            anchor: PhantomData,
        }
    }

    pub fn get(&self) -> T
    where
        T: Copy,
    {
        unsafe { *self.ptr }
    }

    pub fn set(&mut self, v: T)
    where
        T: Copy,
    {
        unsafe { *self.ptr = v }
    }
}

impl<'a, T> Deref for MutableHandle<'a, T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.ptr }
    }
}

impl<'a, T> DerefMut for MutableHandle<'a, T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.ptr }
    }
}
