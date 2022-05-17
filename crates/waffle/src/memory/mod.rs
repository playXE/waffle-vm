use crate::CellType;
use std::any::TypeId;
use std::ptr::NonNull;

pub mod gcwrapper;
pub mod los;
pub mod minimark;
pub mod minimarkpage;
pub mod roots;
pub mod roots_v2;
#[macro_use]
pub mod support;
pub mod gen;
#[repr(C)]
pub struct HeapObjectHeader {
    hdr: HDR,
}

impl HeapObjectHeader {
    pub fn tid(&self) -> Tid {
        unsafe { std::mem::transmute(self.hdr.tid()) }
    }

    pub fn set_tid(&mut self, tid: Tid) {
        unsafe {
            self.hdr.set_tid(std::mem::transmute(tid));
        }
    }

    pub fn is_visited(&self) -> bool {
        self.hdr.visited()
    }

    pub fn set_visited(&mut self, x: bool) {
        self.hdr.set_visited(x);
    }

    pub fn track_young_ptrs(&self) -> bool {
        self.hdr.track_young_ptrs()
    }

    pub fn set_track_young_ptrs(&mut self, x: bool) {
        self.hdr.set_track_young_ptrs(x);
    }

    pub fn has_shadow(&self) -> bool {
        self.hdr.has_shadow()
    }

    pub fn set_has_shadow(&mut self, x: bool) {
        self.hdr.set_has_shadow(x);
    }

    pub fn no_heap_ptrs(&self) -> bool {
        self.hdr.no_heap_ptrs()
    }

    pub fn set_no_heap_ptrs(&mut self, x: bool) {
        self.hdr.set_no_heap_ptrs(x);
    }

    pub fn finalization_ordering(&self) -> bool {
        self.hdr.finalization_ordering()
    }

    pub fn set_finalization_ordering(&mut self, x: bool) {
        self.hdr.set_finalization_ordering(x);
    }
}

#[derive(Clone, Copy)]
pub union Tid {
    pub as_word: Word,
    pub as_vtable: &'static VTable,
}

#[cfg(target_pointer_width = "32")]
pub type Word = u32;
#[cfg(target_pointer_width = "64")]
pub type Word = u64;

bitfield! {
    #[derive(Copy, Clone)]
    pub struct HDR(u64);
    #[doc = "Pointer to VTable or forwarding pointer if object is in nursery and `finalization_ordering` is set to true"]
    Word, tid, set_tid: 63,5;
    bool, track_young_ptrs,set_track_young_ptrs: 0;
    bool, no_heap_ptrs,set_no_heap_ptrs: 1;
    bool, visited,set_visited: 2;
    bool, has_shadow,set_has_shadow: 3;
    bool, finalization_ordering,set_finalization_ordering: 4;
}
pub const WORD: usize = size_of::<usize>();
pub const LONG_BIT: usize = size_of::<usize>() * 8;

pub struct VTable {
    pub ty: CellType,
    pub size: usize,
    pub is_varsize: bool,
    pub varsize: VarSize,
    pub trace: extern "C" fn(*mut (), marker: *mut Marker),
    pub finalize: Option<extern "C" fn(*mut ())>,
    /// Set to true when type finalizer cannot revive object i.e when finalizer is equal to `T::drop`
    pub light_finalizer: bool,
    pub type_id: TypeId,
    pub type_name: &'static str,
}

quasiconst! {
    pub const VTABLE<T: 'static + Allocation>: &'static VTable = &VTable {
        ty: T::CELL_TYPE,
        size: T::SIZE,
        varsize:
            VarSize {
                itemsize:T::VARSIZE_ITEM_SIZE,
                offset_of_length: T::VARSIZE_OFFSETOF_LENGTH,
                offset_of_variable_part:T::VARSIZE_OFFSETOF_VARPART
            },
        is_varsize: T::VARSIZE,
        trace: trace_erased::<T>,
        finalize:if T::FINALIZE { Some(finalize_erased::<T>) } else { None },
        light_finalizer: T::LIGHT_FINALIZER,
        type_id: TypeId::of::<T>(),
        type_name: std::any::type_name::<T>()
    };
}

pub struct VarSize {
    pub itemsize: usize,
    pub offset_of_length: usize,
    pub offset_of_variable_part: usize,
}

use core_extensions::*;
use std_::mem::size_of;

use self::gcwrapper::{Gc, WeakRef};

extern "C" fn finalize_erased<T: Finalize>(ptr: *mut ()) {
    unsafe {
        (&mut *ptr.cast::<T>()).finalize();
    }
}

extern "C" fn trace_erased<T: Trace>(ptr: *mut (), marker: *mut Marker) {
    unsafe {
        let value = ptr.cast::<T>();
        let marker = &mut *marker;
        (&mut *value).trace(marker.vis);
    }
}

#[repr(C)]

pub struct Marker<'a> {
    vis: &'a mut dyn Visitor,
}

impl Visitor for Marker<'_> {
    fn mark_object(&mut self, root: &mut NonNull<HeapObjectHeader>) {
        self.vis.mark_object(root);
    }
}
/// Indicates that a type can be traced by a garbage collector.
///
/// This doesn't necessarily mean that the type is safe to allocate in a garbage collector ([Collectable]).
///
/// ## Safety
/// See the documentation of the `trace` method for more info.
/// Essentially, this object must faithfully trace anything that
/// could contain garbage collected pointers or other `Trace` items.
pub unsafe trait Trace {
    /// Trace each field in this type.
    ///
    /// Structures should trace each of their fields,
    /// and collections should trace each of their elements.
    ///
    /// ### Safety
    /// Some types (like `Gc`) need special actions taken when they're traced,
    /// but those are somewhat rare and are usually already provided by the garbage collector.
    ///
    /// Behavior is restricted during tracing:
    /// ## Permitted Behavior
    /// - Reading your own memory (includes iteration)
    ///   - Interior mutation is undefined behavior, even if you use `RefCell`
    /// - Calling `Visitor::mark_object`
    ///   
    /// - Panicking on unrecoverable errors
    ///   - This should be reserved for cases where you are seriously screwed up,
    ///       and can't fulfill your contract to trace your interior properly.
    ///     - One example is `Gc<T>` which panics if the garbage collectors are mismatched
    ///   - Garbage collectors may chose to [abort](std::process::abort) if they encounter a panic,
    ///     so you should avoid doing it if possible.
    /// ## Never Permitted Behavior
    /// - Forgetting a element of a collection, or field of a structure
    ///   - If you forget an element undefined behavior will result
    ///   - This is why you should always prefer automatically derived implementations where possible.
    ///     - With an automatically derived implementation you will never miss a field
    /// - It is undefined behavior to mutate any of your own data.
    ///   - The mutable `&mut self` is just so copying collectors can relocate GC pointers
    /// - Calling other operations on the garbage collector (including allocations)
    fn trace(&mut self, visitor: &mut dyn Visitor) {
        let _ = visitor;
    }
}
/// Indicates a type that can be finalized after marking phase.
///
/// Note that we have two types of finalizers: light and regular. Light finalizers are executed when your finalizer is simply a call to `T::drop`.
/// Regular finalizers are executed in a specific order and they are allowed to revive objects.
pub unsafe trait Finalize {
    /// Finalization method, invoked when object is dead.
    fn finalize(&mut self) {
        unsafe {
            core::ptr::drop_in_place(self);
        }
    }
}

/// Trait that specifies allocation behaviour.
pub unsafe trait Allocation: Trace + Finalize + Sized {
    /// If true then [T::finalize](Finalize::finalize) is invoked on the object when it is dead
    const FINALIZE: bool = core::mem::needs_drop::<Self>();
    /// If true then finalizer of this object cannot revive objects
    const LIGHT_FINALIZER: bool = Self::FINALIZE;
    /// Object statically known size.
    const SIZE: usize = core::mem::size_of::<Self>();

    /// Cell type of specific object. Should not be changed by users, it is VM internal
    const CELL_TYPE: CellType = CellType::Abstract;

    /// If true then this object has GC pointers inside it, if false then it is not traced
    const HAS_GCPTRS: bool = true;

    /// If true object first field is weakptr. Used for [WeakRef][gcwrapper::WeakRef]
    const HAS_WEAKPTR: bool = false;

    /// Set to true when object is variably sized i.e arrays
    const VARSIZE: bool = false;
    /// Size of varsize object items
    const VARSIZE_ITEM_SIZE: usize = 0;
    /// Offset of length field in varsize object
    const VARSIZE_OFFSETOF_LENGTH: usize = 0;
    /// Offset of variable part in varsize object
    const VARSIZE_OFFSETOF_VARPART: usize = 0;
}

unsafe impl<T: Trace + Finalize> Allocation for T {
    default const FINALIZE: bool = core::mem::needs_drop::<T>();
    default const LIGHT_FINALIZER: bool = Self::FINALIZE;
    default const SIZE: usize = core::mem::size_of::<Self>();
    default const CELL_TYPE: CellType = CellType::Abstract;
    default const HAS_GCPTRS: bool = true;
    default const HAS_WEAKPTR: bool = false;
    default const VARSIZE: bool = false;
    default const VARSIZE_ITEM_SIZE: usize = 0;
    default const VARSIZE_OFFSETOF_LENGTH: usize = 0;
    default const VARSIZE_OFFSETOF_VARPART: usize = 0;
}

pub trait Visitor {
    fn mark_object(&mut self, root: &mut NonNull<HeapObjectHeader>);
    fn mark_weak(&mut self, root: &mut NonNull<HeapObjectHeader>) {
        self.mark_object(root);
    }
}

pub fn mark<T: Object + ?Sized>(vis: &mut dyn Visitor, root: &mut Gc<T>) {
    vis.mark_object(&mut root.header);
}

pub fn mark_weak<T: Object + ?Sized>(vis: &mut dyn Visitor, root: &mut WeakRef<T>) {
    vis.mark_object(&mut root.value.header);
}

pub trait Object: Trace + Finalize {}

pub mod tests;
