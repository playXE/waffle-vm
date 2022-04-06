use super::*;
use std::{marker::PhantomData, ptr::NonNull};

#[repr(C)]
pub struct FrameMap<const N: usize> {
    pub num_roots: u32,
    pub num_meta: u32,
    pub meta: [*const u8; N],
}

impl<const N: usize> FrameMap<N> {
    #[inline]
    pub const fn new(num_roots: u32, meta: [*const u8; N]) -> FrameMap<N> {
        Self {
            num_roots,
            num_meta: N as u32,
            meta,
        }
    }
    pub fn metas(&self) -> &[*const u8] {
        unsafe { std::slice::from_raw_parts(self.meta.as_ptr(), self.num_meta as _) }
    }

    #[inline]
    pub const fn as_unsized(this: *const Self) -> *const FrameMap<0> {
        this.cast()
    }
}

#[repr(C)]
pub struct StackEntry<const N: usize> {
    pub next: *mut StackEntry<0>,
    pub map: NonNull<FrameMap<0>>,
    pub roots: [*const u8; N],
}

impl<const N: usize> StackEntry<N> {
    #[inline]
    pub const unsafe fn new<const M: usize>(
        next: *mut StackEntry<0>,
        map: *const FrameMap<M>,
        roots: [*const u8; N],
    ) -> Self {
        Self {
            next,
            map: NonNull::new_unchecked(FrameMap::as_unsized(map) as _),
            roots,
        }
    }
    pub fn roots(&mut self) -> &mut [*const u8] {
        unsafe {
            let num_roots = self.map.as_ref().num_roots;
            let roots = std::slice::from_raw_parts_mut(self.roots.as_mut_ptr(), num_roots as _);
            roots
        }
    }

    #[inline]
    pub const fn as_unsized(this: *const Self) -> *const StackEntry<0> {
        this.cast()
    }
}

pub type StackChain = *mut StackEntry<0>;

#[inline]
pub unsafe fn push_gcframe(root: &mut StackChain, frame: *mut StackEntry<0>) {
    *root = frame;
}
#[inline]
pub unsafe fn pop_gcframe(root: &mut StackChain, frame: *mut StackEntry<0>) {
    debug_assert!(!root.is_null());
    debug_assert_eq!(*root, frame);
    *root = (**root).next;
}

pub unsafe fn visit_roots<F>(root: StackChain, mut vis: F)
where
    F: FnMut(*mut *const u8, *const u8),
{
    let mut entry = root;
    while !entry.is_null() {
        let roots = (*entry).roots();
        let metas = (*entry).map.as_ref().metas();
        for (i, root) in roots.iter().copied().enumerate() {
            vis(
                root as *mut *const u8,
                metas.get(i).copied().unwrap_or_else(core::ptr::null),
            );
        }

        entry = (*entry).next;
    }
}

pub struct GcFrameRegistration<'a> {
    frame: *mut StackEntry<0>,
    chain: *mut StackChain,
    _phantom: PhantomData<&'a StackEntry<0>>,
}

impl<'a> GcFrameRegistration<'a> {
    pub fn new<const N: usize>(chain: *mut StackChain, frame: &'a StackEntry<N>) -> Self {
        let frame = StackEntry::as_unsized(frame) as *mut StackEntry<0>;
        unsafe {
            push_gcframe(&mut *chain, frame);
        }
        Self {
            frame,
            chain,
            _phantom: PhantomData,
        }
    }
}
impl<'a> Drop for GcFrameRegistration<'a> {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            pop_gcframe(&mut *self.chain, self.frame);
        }
    }
}

pub struct Rooted<T: Rootable> {
    pub value: *mut T,
}

impl<T: Rootable> Rooted<T> {
    pub fn as_ref(&self) -> &T {
        unsafe { &*self.value }
    }

    pub fn as_mut(&mut self) -> &mut T {
        unsafe { &mut *self.value }
    }

    pub fn get(&self) -> T
    where
        T: Copy,
    {
        unsafe { self.value.read() }
    }

    pub fn set(&mut self, value: T) -> T {
        std::mem::replace(self.as_mut(), value)
    }
}

pub trait Rootable {
    /// Pointer to `trace` function or NULL. If NULL then it is `Gc<T>` object
    const METADATA: *const u8;
}

impl<T: Trace> Rootable for T {
    const METADATA: *const u8 = T::trace as *const u8;
}

#[macro_export]
macro_rules! count {
    () => (0usize);
    ( $x:tt $($xs:tt)* ) => (1usize + $crate::count!($($xs)*));
}
#[inline]
pub const fn get_root_meta_of<T: Rootable>(_: &T) -> *const u8 {
    T::METADATA
}

#[macro_export]
macro_rules! gc_frame {
    ($chain: expr => $($i: ident),*) => {

        let frame_map = $crate::memory::roots::FrameMap::new($crate::count!($($i)*) as u32,[
            $(
                $crate::memory::roots::get_root_meta_of(&$i)
            ),*
        ]);

        #[allow(unused_unsafe)]
        let stack_entry = unsafe {$crate::memory::roots::StackEntry::new(*$chain, &frame_map, [
            $( &mut $i as *mut _ as *mut u8 ),*
        ])};

        #[allow(unused_unsafe)]
        let _stack_entry_registration = unsafe{$crate::memory::roots::GcFrameRegistration::new($chain,&stack_entry)};

        $crate::gc_frame!(@parse stack_entry, 0, $($i)*);
    };
    (@parse $stack_entry: ident, $n: expr, $i: ident $($is: ident)*) => {
        #[allow(unused_unsafe,unused_mut)]
        let mut $i = unsafe {$crate::memory::roots::gcroot_of_type(($stack_entry).roots[$n], &$i)};
        $crate::gc_frame!(@parse $stack_entry, ($n + 1), $($is)*)
    };

    (@parse $stack_entry: ident, $n: expr,) => {

    };
}

pub unsafe fn gcroot_of_type<T: Rootable>(ptr: *const u8, _to: &T) -> Rooted<T> {
    Rooted {
        value: ptr as *const *const u8 as *mut T,
    }
}
