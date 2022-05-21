//! # Conservative Mark&Sweep for Scheme
//!
//! This module implements simple conservative mark&sweep that uses runs-of-slots allocator for memory management.
//!
//!
//! ## Allocation
//!
//! Runs of slots allocator is used to implement malloc/free-like functionality. Rosalloc allocates small enough objects (<=2KB) into "runs".
//! Each run consists of N slots and each run can only allocate objects of single size. When allocation request is too large
//! we resort to page allocation algorithm that searches for free pages in the heap and allocates object into them. Note that if you will request
//! to allocate 6kb object allocator will actually allocate 8kb because single page size is 4kb and 6kb object spans two pages.
//!
//! ## Collection
//!
//! GC starts when certain GC threshold is reached, threshold is reseted on each GC cycle based on amount of objects still alive.
//!
//! ### Marking phase
//!
//! Marking phase is fully conservative. This means it can identify any random integer that looks like GC pointer as GC pointer.
//! When it starts, stack is scanned word by word for potential pointers and after that all found objects are scanned for pointers
//! in them recursively.
//!
//! At the end of the cycle mark bitmap should have bits set for all found objects.
//!
//! ### Sweeping
//!
//! In this phase we simply walk live bitmap and free objects based on mark bitmap and live bitmap. Essentially it is like this:
//! ```
//!
//! garbage_bits = live_bitmap & !mark_bitmap;
//! live_bitmap &= mark_bitmap;
//! for ptr in self.pointers_from_bits(garbage_bits) {
//!  self.free(ptr)
//! }
//! ```

use std::{cell::UnsafeCell, mem::size_of, panic::panic_any, ptr::null_mut};

use core_extensions::getconst;

use crate::memory::{
    minimark::{align_usize, read_float_from_env, read_uint_from_env, MemoryError},
    roots::StackChain,
    Allocation, HeapObjectHeader, Tid, Trace, HDR,
};

use self::allocator::{PageReleaseMode, DEFAULT_PAGE_RELEASE_THRESHOLD};

use super::{DEFAULT_MAX_HEAP_SIZE, DEFAULT_MIN_HEAP_SIZE};

pub mod allocator;
pub mod bitfield;
pub mod bitmap;
pub mod mmap;
pub mod segmented_vec;
pub mod stack;

/*
use self::allocator::Run;
use self::bitfield::{
    BitFieldTrait, FreeBitfield, ImmutableBitfield, LargeBitfield, SizeBitField, SyntaticBitfield,
};
use self::{
    allocator::{PageMapKind, PageReleaseMode, PAGE_SIZE},
    bitmap::SpaceBitmap,
};
use std::collections::{HashMap, HashSet};
use std::mem::size_of;

#[derive(Clone, Copy)]
#[repr(C)]
pub struct CellHeader {
    pub(crate) data: u32,
    #[cfg(feature = "track-source")]
    pub(crate) source: &'static std::panic::Location<'static>,
}

impl CellHeader {
    #[inline(always)]
    pub unsafe fn data<T>(&self) -> *mut T {
        let p = self as *const Self as *mut u8;
        p.add(size_of::<Self>()).cast()
    }
    #[inline(always)]
    pub fn is_large(self) -> bool {
        LargeBitfield::decode(self.data as _) != 0
    }
    #[inline(always)]
    pub fn size_raw(self) -> usize {
        SizeBitField::decode(self.data as _) as _
    }
    #[inline(always)]
    pub fn size(self) -> usize {
        if self.is_large() {
            self.size_raw() * PAGE_SIZE
        } else {
            self.size_raw() * 8
        }
    }

    #[inline(always)]
    pub fn set_size(&mut self, size: usize) {
        if size > 2048 {
            self.data = SizeBitField::update(self.data as _, size as u64 / PAGE_SIZE as u64) as u32;
            self.data = LargeBitfield::update(self.data as _, 1) as u32;
        } else {
            self.data = SizeBitField::update(self.data as _, size as u64 / 8) as u32;
            self.data = LargeBitfield::update(self.data as _, 0) as u32;
        }
    }

    #[inline(always)]
    pub fn syntaticp(self) -> bool {
        SyntaticBitfield::decode(self.data as _) != 0
    }

    #[inline(always)]
    pub fn immutablep(self) -> bool {
        ImmutableBitfield::decode(self.data as _) != 0
    }

    #[inline(always)]
    pub fn freep(self) -> bool {
        FreeBitfield::decode(self.data as _) != 0
    }

    #[inline(always)]
    pub fn set_syntatic(&mut self, x: bool) {
        self.data = SyntaticBitfield::update(self.data as _, x as u64) as u32;
    }

    #[inline(always)]
    pub fn set_immutable(&mut self, x: bool) {
        self.data = ImmutableBitfield::update(self.data as _, x as u64) as u32;
    }

    #[inline(always)]
    pub fn set_free(&mut self, x: bool) {
        self.data = FreeBitfield::update(self.data as _, x as u64) as u32;
    }
}

#[repr(C)]
pub struct Heap {
    live_bitmap: SpaceBitmap<8>,
    mark_bitmap: SpaceBitmap<8>,
    deque: Vec<(*mut u8, usize)>,
    min_heap_size: usize,
    max_heap_size: usize,
    allocated: usize,
    threshold: usize,
    allocator: allocator::GcAllocator,
    verbose: u8,
    gc_count: usize,
    growth_multiplier: f64,
    finalize: HashMap<*mut u8, (extern "C" fn(*mut u8, *mut u8), *mut u8)>,
    roots: HashSet<(*mut u8, *mut u8)>,
}

impl Heap {
    pub fn new(
        heap_size: usize,
        min_heap_size: usize,
        max_heap_size: usize,
        threshold: usize,
        growth_multiplier: f64,
        page_release_mode: PageReleaseMode,
        page_release_threshold: usize,
        verbose: u8,
    ) -> Self {
        unsafe {
            let allocator =
                allocator::GcAllocator::new(heap_size, page_release_mode, page_release_threshold);

            let mark_bitmap =
                SpaceBitmap::<8>::create("mark-bitmap", allocator.base(), allocator.size());
            let live_bitmap =
                SpaceBitmap::<8>::create("live-bitmap", allocator.base(), allocator.size());

            Self {
                max_heap_size,
                min_heap_size,
                threshold,
                allocated: 0,
                mark_bitmap,
                live_bitmap,
                deque: Vec::with_capacity(256),
                allocator,
                verbose,
                growth_multiplier,
                gc_count: 0,
                roots: HashSet::new(),
                finalize: HashMap::new(),
            }
        }
    }
    pub fn push_roots(&mut self, start: *mut u8, end: *mut u8) {
        self.roots.insert((start, end));
    }

    pub fn pop_roots(&mut self, start: *mut u8, end: *mut u8) {
        self.roots.remove(&(start, end));
    }

    pub unsafe fn regisrter_finalizer(
        &mut self,
        ptr: *mut u8,
        callback: Option<extern "C" fn(*mut u8, *mut u8)>,
        data: *mut u8,
    ) {
        if callback.is_none() {
            self.finalize.remove(&ptr);
        } else {
            self.finalize.insert(ptr, (callback.unwrap(), data));
        }
    }
    unsafe fn mark_object(&mut self, object: *mut CellHeader, size: usize) {
        if !self.mark_bitmap.set_sync(object.cast()) {
            if !self.live_bitmap.test(object.cast()) {
                panic!("Not valid address: {:p}", object);
            }
            self.deque.push((object.cast::<u8>(), size));
        }
    }
    unsafe fn mark_conservative(&mut self, start: *mut u8, end: *mut u8) {
        let mut cursor = start.cast::<*mut u8>();
        let end = end.cast::<*mut u8>();

        while cursor < end {
            // TODO: Apparently this is UB when scanning stack. It may read stack slot that points to uninit bytes. We have to write
            // inline assembly code to read uninit bytes "safely".
            let pointer = cursor.read();
            if pointer.is_null() {
                cursor = cursor.add(1);
                continue;
            }
            self.try_pointer_conservative(pointer);
            cursor = cursor.add(1);
            continue;
        }
    }

    /// Tries to mark pointer conservatively. If pointer is not from the heap or points to unintialized
    /// memory it is not marked
    #[inline(always)]
    unsafe fn try_pointer_conservative(&mut self, pointer: *mut u8) {
        if self.live_bitmap.has_address(pointer) {
            let mut pm_idx = self.allocator.to_page_map_index(pointer);

            let object = match self.allocator.page_map_kind_at(pm_idx) {
                PageMapKind::LargeObject => {
                    let p = self
                        .allocator
                        .base()
                        .add(pm_idx * PAGE_SIZE)
                        .cast::<CellHeader>();

                    p
                } // no need to check bitmap if it is large page
                PageMapKind::LargeObjectPart => {
                    // if it is large object part we also don't have to check bitmap
                    while {
                        pm_idx -= 1;
                        self.allocator.page_map_kind_at(pm_idx) != PageMapKind::LargeObject
                    } {}

                    let p = self
                        .allocator
                        .base()
                        .add(pm_idx * PAGE_SIZE)
                        .cast::<CellHeader>();

                    p
                }

                // for runs we just search for run start and get the slot pointer. It is OK if
                // GC will identify random integer on the stack as unallocated Run slot as our marking is conservative
                PageMapKind::RunPart => {
                    while {
                        pm_idx -= 1;
                        self.allocator.page_map_kind_at(pm_idx) != PageMapKind::Run
                    } {}

                    let p = (*self.allocator.base().add(pm_idx * PAGE_SIZE).cast::<Run>())
                        .slot_from_ptr(pointer);

                    if !self.live_bitmap.test(p) {
                        return;
                    }
                    p.cast()
                }
                PageMapKind::Run => {
                    let p = (*self.allocator.base().add(pm_idx * PAGE_SIZE).cast::<Run>())
                        .slot_from_ptr(pointer);

                    if !self.live_bitmap.test(p) {
                        return;
                    }

                    p.cast()
                }
                _ => return,
            };

            let sz = (*object).size();

            self.mark_object(object, sz);
        }
    }

    unsafe fn sweep(&mut self) -> usize {
        let time = if self.verbose > 1 {
            Some(std::time::Instant::now())
        } else {
            None
        };
        let sweep_begin = self.allocator.base();
        let sweep_end = sweep_begin.add(self.allocator.size());
        let mut reclaimed = 0;
        SpaceBitmap::<8>::sweep_walk_sync(
            &self.live_bitmap,
            &self.mark_bitmap,
            sweep_begin as _,
            sweep_end as _,
            |argc, argv| {
                reclaimed += self
                    .allocator
                    .bulk_free(std::slice::from_raw_parts(argv.cast(), argc));
            },
        );

        if let Some(elapsed) = time.map(|x| x.elapsed()) {
            println!(
                "[sweep] Complete in {:.4}ms, {} reclaimed",
                elapsed.as_micros() as f64 / 1000.0,
                formatted_size(reclaimed)
            )
        }
        reclaimed
    }
    #[inline(never)]
    unsafe fn mark(&mut self) {
        let time = if self.verbose > 1 {
            Some(std::time::Instant::now())
        } else {
            None
        };
        let mut start = stack::approximate_stack_pointer();
        let mut end = stack::stack_bounds().origin;

        if start > end {
            std::mem::swap(&mut start, &mut end);
        }
        self.mark_conservative(start, end);

        let roots = std::mem::replace(&mut self.roots, HashSet::new());
        for (root_start, root_end) in roots.iter().copied() {
            self.mark_conservative(root_start, root_end);
        }
        let _ = std::mem::replace(&mut self.roots, roots);
        while let Some((start, size)) = self.deque.pop() {
            let mut cursor = start.cast::<*mut u8>();

            let end = start.add(size);

            while cursor < end.cast::<*mut u8>() {
                let ptr = cursor.read();
                self.try_pointer_conservative(ptr);
                cursor = cursor.add(1);
            }
        }

        if let Some(elapsed) = time.map(|x| x.elapsed()) {
            println!(
                "[mark] Complete in {:.4}ms",
                elapsed.as_micros() as f64 / 1000.0
            );
        }
    }
    pub fn collect(&mut self) {
        self.collect_raw(0, 1, 2, 3, 4, 5);
    }
    #[allow(unused_variables)]
    #[inline(never)]
    fn collect_raw(&mut self, a1: usize, a2: usize, a3: usize, a4: usize, a5: usize, a6: usize) {
        let time = if self.verbose > 0 {
            Some(std::time::Instant::now())
        } else {
            None
        };

        unsafe {
            self.mark();

            let finalize = std::mem::replace(&mut self.finalize, HashMap::new());
            let mut finalized = 0;
            for (obj, (callback, data)) in finalize.iter() {
                if !self.mark_bitmap.test(*obj) {
                    finalized += (*obj.sub(size_of::<CellHeader>()).cast::<CellHeader>()).size();
                    (*callback)(*obj, *data);
                }
            }

            let reclaimed = self.sweep();
            let prev = self.allocated;
            self.allocated -= reclaimed;

            let new_threshold = (self.allocated as f64 * self.growth_multiplier) as usize;
            let new_threshold = if new_threshold < self.min_heap_size {
                self.min_heap_size
            } else if new_threshold > self.max_heap_size {
                self.max_heap_size
            } else {
                new_threshold
            };

            self.threshold = new_threshold;

            if let Some(elapsed) = time.map(|x| x.elapsed().as_micros() as f64 / 1000.0) {
                println!("[finalize] Finalized {} ", formatted_size(finalized));
                println!(
                    "[gc] GC({}) Mark&Sweep Pause {}->{}({}) {:.4}ms",
                    self.gc_count,
                    formatted_size(prev),
                    formatted_size(self.allocated),
                    formatted_size(new_threshold),
                    elapsed
                );
            }
            self.gc_count += 1;
        }
    }

    pub unsafe fn malloc(&mut self, size: usize) -> *mut u8 {
        let size = size + size_of::<CellHeader>();
        if self.allocated > self.threshold {
            self.collect();
        }

        let mut blk = 0;
        let mut memory = self.allocator.alloc(size, &mut blk);
        if memory.is_null() {
            self.collect();
            memory = self.allocator.alloc(size, &mut blk);
            if memory.is_null() {
                oom_abort();
            }
        }
        self.allocated += blk;
        self.live_bitmap.set_sync(memory);

        let cell = memory.cast::<CellHeader>();
        (*cell).data = 0;
        (*cell).set_size(blk);
        memory.add(size_of::<CellHeader>())
    }

    pub unsafe fn free(&mut self, pointer: *mut u8) {
        let cell = pointer.sub(size_of::<CellHeader>());
        let sz = (*cell.cast::<CellHeader>()).size();
        self.allocator.free(cell);
        self.live_bitmap.clear(cell);

        self.allocated -= sz;
    }
}

use std::fmt;
pub struct FormattedSize {
    pub size: usize,
}

impl fmt::Display for FormattedSize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ksize = (self.size as f64) / 1024f64;

        if ksize < 1f64 {
            return write!(f, "{}B", self.size);
        }

        let msize = ksize / 1024f64;

        if msize < 1f64 {
            return write!(f, "{:.1}K", ksize);
        }

        let gsize = msize / 1024f64;

        if gsize < 1f64 {
            write!(f, "{:.1}M", msize)
        } else {
            write!(f, "{:.1}G", gsize)
        }
    }
}

pub fn formatted_size(size: usize) -> FormattedSize {
    FormattedSize { size }
}

#[inline(never)]
#[cold]
fn oom_abort() -> ! {
    eprintln!("out of memory");
    std::process::exit(1);
}
*/

pub struct Heap {
    allocated: usize,
    threshold: usize,
    allocator: allocator::GcAllocator,
    growth_multiplier: f64,
    finalizers: Vec<*mut HeapObjectHeader>,
    max_heap_size_already_raised: bool,
    gcount: usize,
    min_heap_size: usize,
    max_heap_size: usize,
    roots: UnsafeCell<StackChain>,
    weakrefs: Vec<*mut HeapObjectHeader>,
}

pub const HEAP_SIZE: usize = 256 * 1024 * 1024;

impl Heap {
    pub fn new() -> Box<Self> {
        let heap_size = read_uint_from_env("WAFFLE_HEAP_SIZE").unwrap_or_else(|| HEAP_SIZE);
        let page_release_mode = match std::env::var("WAFFLE_PAGE_RELEASE_MODE") {
            Err(_) => PageReleaseMode::End,
            Ok(mode) => match &*(mode.to_lowercase()) {
                "all" => PageReleaseMode::All,
                "end" => PageReleaseMode::End,
                "sizeandend" => PageReleaseMode::SizeAndEnd,
                "none" => PageReleaseMode::None,
                "size" => PageReleaseMode::Size,
                _ => PageReleaseMode::None,
            },
        };
        let page_release_threshold =
            read_uint_from_env("PAGE_RELEASE_THRESHOLD").unwrap_or(DEFAULT_PAGE_RELEASE_THRESHOLD);

        let growth_multiplier = read_float_from_env("WAFFLE_GROWTH_MULTIPLIER").unwrap_or(1.5);
        let min_heap_size =
            read_uint_from_env("WAFFLE_MIN_HEAP_SIZE").unwrap_or(DEFAULT_MIN_HEAP_SIZE);
        let max_heap_size =
            read_uint_from_env("WAFFLE_MAX_HEAP_SIZE").unwrap_or(DEFAULT_MAX_HEAP_SIZE);
        let threshold = read_uint_from_env("WAFFLE_GC_THRESHOLD").unwrap_or(heap_size / 4);
        let alloc = unsafe {
            allocator::GcAllocator::new(heap_size, page_release_mode, page_release_threshold)
        };

        let this = Self {
            max_heap_size,
            min_heap_size,
            threshold,
            growth_multiplier,
            allocator: alloc,
            allocated: 0,
            finalizers: vec![],
            max_heap_size_already_raised: false,
            gcount: 0,
            roots: UnsafeCell::new(null_mut()),
            weakrefs: vec![],
        };

        Box::new(this)
    }

    pub unsafe fn malloc_fixedsize<A: 'static + Allocation>(
        &mut self,
        safestack: &mut [&mut dyn Trace],
    ) -> *mut HeapObjectHeader {
        if self.allocated > self.threshold {
            // todo: gc
        }
        let totalsize = align_usize(size_of::<HeapObjectHeader>() + A::SIZE, 16);
        let mut bulk = 0;
        let mut obj = self
            .allocator
            .alloc(totalsize, &mut bulk)
            .cast::<HeapObjectHeader>();
        if obj.is_null() {
            obj = self
                .allocator
                .alloc(totalsize, &mut bulk)
                .cast::<HeapObjectHeader>();
            if obj.is_null() {
                panic_any(Box::new(MemoryError));
            }
        }
        self.allocated += bulk;

        obj.write(HeapObjectHeader { hdr: HDR(0) });
        (*obj).set_tid(Tid {
            as_vtable: getconst!(crate::memory::VTABLE<A>),
        });
        (*obj).set_has_shadow(false);

        if A::FINALIZE {
            self.finalizers.push(obj);
        }
        if A::HAS_WEAKPTR {
            self.weakrefs.push(obj);
        }

        obj
    }
}
