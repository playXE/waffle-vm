use crate::memory::support::formatted_size;

use super::{
    los::{LargeObjectSpace, PreciseAllocation},
    minimarkpage::out_of_memory,
    roots::{visit_roots, StackChain},
    support::SegmentedVec,
    *,
};
use fxhash::FxHashMap;
use libc::c_void;
use libmimalloc_sys::{mi_heap_area_t, mi_heap_t};
use memmap2::MmapMut;
use std::{
    collections::{HashMap, VecDeque},
    panic::panic_any,
};
use std_::{cell::UnsafeCell, intrinsics::unlikely, mem::replace, ptr::null_mut};
use sys_info::mem_info;

/// Garbage collector based on minimark.py from RPython.
///
/// # How it works
///
/// MiniMark has bump-pointer nursery and mimalloc heap as old space. For large objects (>=135KB on 64 bit targets and >= 66KB on 32bit) separate
/// LargeObjectSpace is used. LOS objects are allocated using `malloc` and `free` directly and sticky mark bits are used to do generational GC for LOS
/// objects.
///
///
///
///
/// # Constraints
/// - All objects from non LOS space are aligned to 16 bytes
/// - All objects from LOS space are aligned to 8 bytes
#[allow(dead_code)]
pub struct MiniMark {
    nursery_free: *mut u8,
    nursery_top: *mut u8,
    total_mi_memory: usize,
    mi_heap: *mut libmimalloc_sys::mi_heap_t,
    los: LargeObjectSpace,

    nursery_map: MmapMut,

    nursery_size: usize,
    nonlarge_max: usize,

    old_objects_pointing_to_young: SegmentedVec<*mut HeapObjectHeader>,
    major_collection_threshold: f64,
    pub roots: UnsafeCell<StackChain>,
    min_heap_size: usize,
    max_heap_size: usize,
    next_major_collection_initial: usize,
    next_major_collection_threshold: usize,
    growth_rate_max: f64,
    max_delta: f64,
    trace_hooks: HashMap<u32, Box<dyn FnMut(&mut dyn Visitor)>>,
    objects_to_trace: SegmentedVec<*mut HeapObjectHeader>,
    probably_young_objects_with_finalizers: SegmentedVec<*mut ()>,
    old_objects_with_finalizers: SegmentedVec<*mut ()>,
    run_finalizers: SegmentedVec<*mut ()>,
    young_objects_with_destructors: SegmentedVec<*mut HeapObjectHeader>,
    old_objects_with_destructors: SegmentedVec<*mut HeapObjectHeader>,

    young_weakrefs: SegmentedVec<*mut HeapObjectHeader>,
    old_weakrefs: SegmentedVec<*mut HeapObjectHeader>,
    /// Map young object <-> old object
    nursery_object_shadows: FxHashMap<usize, usize>,

    kept_alive_by_finalizer: usize,
    tmpstack: VecDeque<*mut HeapObjectHeader>,
    finalize_lock: bool,
    max_heap_size_already_raised: bool,
}

pub const NURSERY_SIZE: usize = 1024 * 1024;
pub const PAGE_SIZE: usize = 1024 * size_of::<usize>();
pub const ARENA_SIZE: usize = 64 * 1024 * size_of::<usize>();
pub const SMALL_REQUEST_THRESHOLD: usize = 35 * size_of::<usize>();
pub const MAJOR_COLLECTION_THRESHOLD: f64 = 1.82;
pub const GROWTH_RATE_MAX: f64 = 1.4;
pub const LARGE_OBJECT: usize = (16 * 1024 + 512) * size_of::<usize>();
pub const NONLARGE_MAX: usize = LARGE_OBJECT - 1;
impl MiniMark {
    pub fn is_forwarded(&self, obj: *const HeapObjectHeader) -> bool {
        unsafe { (*obj).has_shadow() && (*obj).finalization_ordering() }
    }

    pub fn get_forwarding_address(&self, obj: *const HeapObjectHeader) -> *mut HeapObjectHeader {
        unsafe { (*obj).tid().as_word as *mut _ }
    }
    #[inline(never)]
    pub fn new(
        mut major_collection_threshold: f64,
        mut growth_rate_max: f64,
        large_object: usize,
    ) -> Box<Self> {
        let nonlarge_max = large_object - 1;
        let newsize = read_uint_from_env("WAFFLE_GC_NURSERY");
        let newsize = match newsize {
            Some(x) => x,
            _ => NURSERY_SIZE,
        };

        if let Some(major_coll) = read_float_from_env("WAFFLE_GC_MAJOR_COLLECT") {
            if major_coll > 1.0 {
                major_collection_threshold = major_coll
            }
        }

        if let Some(growth) = read_float_from_env("WAFFLE_GC_GROWTH") {
            if growth > 1.0 {
                growth_rate_max = growth;
            }
        }

        let min_heap_size;
        let mut max_heap_size = 0;

        if let Some(min) = read_uint_from_env("WAFFLE_GC_MIN") {
            min_heap_size = min;
        } else {
            min_heap_size = newsize * 8;
        }

        if let Some(max) = read_uint_from_env("WAFFLE_GC_MAX") {
            max_heap_size = max;
        }

        let max_delta: f64;

        if let Some(delta) = read_uint_from_env("WAFFLE_GC_MAX_DELTA") {
            max_delta = delta as f64;
        } else {
            max_delta = 0.125 * mem_info().unwrap().total as f64;
        }
        let mut this = Box::new(Self {
            total_mi_memory: 0,
            los: LargeObjectSpace::new(),
            mi_heap: unsafe { libmimalloc_sys::mi_heap_new() },

            max_delta,
            major_collection_threshold,
            max_heap_size,
            min_heap_size,
            nonlarge_max,
            roots: UnsafeCell::new(null_mut()),
            nursery_free: null_mut(),
            growth_rate_max,
            nursery_size: newsize,
            nursery_object_shadows: FxHashMap::default(),
            nursery_top: null_mut(),
            nursery_map: MmapMut::map_anon(newsize).unwrap(),
            young_objects_with_destructors: SegmentedVec::new(),
            old_objects_pointing_to_young: SegmentedVec::new(),
            old_objects_with_destructors: SegmentedVec::new(),
            old_objects_with_finalizers: SegmentedVec::new(),
            run_finalizers: SegmentedVec::new(),
            trace_hooks: Default::default(),
            young_weakrefs: SegmentedVec::new(),
            next_major_collection_initial: 0,
            next_major_collection_threshold: 0,
            old_weakrefs: SegmentedVec::new(),

            probably_young_objects_with_finalizers: SegmentedVec::new(),
            objects_to_trace: SegmentedVec::new(),
            kept_alive_by_finalizer: 0,
            tmpstack: VecDeque::new(),
            max_heap_size_already_raised: false,
            finalize_lock: false,
        });

        this.allocate_nursery();
        this
    }

    fn allocate_nursery(&mut self) {
        self.nursery_free = self.nursery_map.as_mut_ptr();
        self.nursery_top = unsafe { self.nursery_free.add(self.nursery_map.len()) };
        self.min_heap_size = self
            .min_heap_size
            .max((self.nursery_size as f64 * self.major_collection_threshold).round() as usize);
        self.next_major_collection_initial = self.min_heap_size;
        self.next_major_collection_threshold = self.min_heap_size;

        self.set_major_threshold_from(0, 0);
    }
    pub fn add_trace_callback<F: 'static + FnMut(&mut dyn Visitor)>(&mut self, x: F) -> u32 {
        let key = self.trace_hooks.len() as u32;
        self.trace_hooks.insert(key, Box::new(x));
        key
    }

    pub fn remove_trace_callback(&mut self, key: u32) -> bool {
        self.trace_hooks.remove(&key).is_some()
    }
    fn set_major_threshold_from(&mut self, mut threshold: usize, reserving: usize) -> bool {
        let threshold_max =
            (self.next_major_collection_initial as f64 * self.growth_rate_max).round() as usize;
        if threshold > threshold_max {
            threshold = threshold_max;
        }

        threshold += reserving;
        if threshold < self.min_heap_size {
            threshold = self.min_heap_size;
        }
        let bounded;
        if self.max_heap_size > 0 && threshold > self.max_heap_size {
            threshold = self.max_heap_size;
            bounded = true;
        } else {
            bounded = false;
        }

        self.next_major_collection_initial = threshold;
        self.next_major_collection_threshold = threshold;
        bounded
    }
    pub fn is_in_nursery(&self, ptr: *const ()) -> bool {
        self.nursery_map.as_ptr().cast::<()>() <= ptr && ptr < self.nursery_top.cast::<()>()
    }

    unsafe fn visit_young_raw_malloced_object(&mut self, obj: *mut HeapObjectHeader) {
        if (*obj).is_visited() {
            return;
        }

        (*obj).set_visited(true);

        self.old_objects_pointing_to_young.push(obj);
    }

    unsafe fn malloc_out_of_nursery(&mut self, size: usize) -> *mut u8 {
        let p = libmimalloc_sys::mi_heap_malloc_aligned(self.mi_heap, size, 16).cast::<u8>();
        debug_assert!(!PreciseAllocation::is_precise(p as _));
        self.total_mi_memory += libmimalloc_sys::mi_usable_size(p as _);
        p
    }

    pub unsafe fn free_if_unvisited(&mut self, hdr: *mut HeapObjectHeader) -> bool {
        if (*hdr).is_visited() {
            (*hdr).set_visited(false);
            false
        } else {
            true
        }
    }

    pub unsafe fn get_size(&self, obj: *const HeapObjectHeader) -> usize {
        let tid = (*obj).tid();
        let mut fixedsize = tid.as_vtable.size;
        if tid.as_vtable.is_varsize {
            let off = tid.as_vtable.varsize.offset_of_length;
            let start = obj.add(1).cast::<u8>();
            let length = start.add(off).cast::<usize>().read();
            fixedsize += length * tid.as_vtable.varsize.itemsize;
        }
        fixedsize
    }

    pub unsafe fn trace_drag_out(
        &mut self,
        root: &mut NonNull<HeapObjectHeader>,
        _parent: *mut HeapObjectHeader,
    ) {
        let obj = root.as_ptr();

        if !self.is_in_nursery(obj.cast()) {
            if PreciseAllocation::is_precise(obj.cast()) {
                if self.los.is_young(obj) {
                    self.visit_young_raw_malloced_object(obj);
                }
            }
            return;
        }

        let newhdr;
        let totalsize;
        if !(*obj).has_shadow() {
            totalsize = size_of::<HeapObjectHeader>() + self.get_size(obj);

            newhdr = self.malloc_out_of_nursery(totalsize) as *mut HeapObjectHeader;
        } else if self.is_forwarded(obj) {
            *root = NonNull::new_unchecked((*obj).tid().as_word as _);
            return;
        } else {
            let newobj = self
                .nursery_object_shadows
                .get(&(obj as usize))
                .copied()
                .unwrap();
            newhdr = newobj as *mut HeapObjectHeader;
            (*obj).set_has_shadow(false);
            totalsize = size_of::<HeapObjectHeader>() + self.get_size(obj);
        }

        core::ptr::copy_nonoverlapping(obj.cast::<u8>(), newhdr.cast::<u8>(), totalsize);

        *root = NonNull::new_unchecked(newhdr);
        (*obj).set_tid(Tid {
            as_word: newhdr as Word,
        });
        (*obj).set_has_shadow(true);
        (*obj).set_finalization_ordering(true);

        self.old_objects_pointing_to_young.push(newhdr);
    }

    pub unsafe fn collect_roots_in_nursery(&mut self, safestack: &mut [&mut dyn Trace]) {
        visit_roots(self.roots.get().read(), |root, meta| {
            if meta.is_null() {
                self.trace_drag_out(&mut *root.cast::<NonNull<HeapObjectHeader>>(), null_mut());
            } else {
                let mut tracer = YoungTrace { gc: self };
                let trace = std::mem::transmute::<_, fn(*mut (), &mut dyn Visitor)>(meta);

                trace(root as *mut (), &mut tracer);
            }
        });

        for value in safestack {
            value.trace(&mut YoungTrace { gc: self })
        }

        let mut hooks = replace(&mut self.trace_hooks, HashMap::new());

        for (_, hook) in hooks.iter_mut() {
            hook(&mut YoungTrace { gc: self });
        }
        self.trace_hooks = hooks;
    }

    /// Called during a nursery collection
    ///
    ///
    /// The code relies on the fact that no weakref can be an old object
    /// weakly pointing to a young object.  Indeed, weakrefs are immutable
    /// so they cannot point to an object that was created after it.
    /// Thanks to this, during a minor collection, we don't have to fix
    /// or clear the address stored in old weakrefs.
    pub unsafe fn invalidate_young_weakrefs(&mut self) {
        while let Some(weakref) = self.young_weakrefs.pop() {
            if !self.is_forwarded(weakref) {
                continue; // weakref itself dies
            }

            let obj = self.get_forwarding_address(weakref);

            let pointing_to = obj.add(1).cast::<*mut HeapObjectHeader>().read();
            if self.is_in_nursery(pointing_to as _) {
                if self.is_forwarded(pointing_to) {
                    obj.add(1)
                        .cast::<*mut HeapObjectHeader>()
                        .write(self.get_forwarding_address(pointing_to));
                } else {
                    obj.add(1).cast::<*mut HeapObjectHeader>().write(null_mut());
                    continue; // no need to remember this weakref any longer
                }
            } else if PreciseAllocation::is_precise(pointing_to as _) {
                if !(*pointing_to).is_visited() {
                    obj.add(1).cast::<*mut HeapObjectHeader>().write(null_mut());
                    continue; // no need to remember this weakref any longer
                }
            }

            self.old_weakrefs.push(obj);
        }
    }

    pub unsafe fn invalidate_old_weakrefs(&mut self) {
        let mut new_with_weakref = SegmentedVec::new();
        while let Some(weakref) = self.old_weakrefs.pop() {
            if !(*weakref).is_visited() {
                continue; // weakref itself dies
            }

            let poiting_to = weakref.add(1).cast::<*mut HeapObjectHeader>().read();
            if (*poiting_to).is_visited() {
                new_with_weakref.push(weakref);
            } else {
                weakref
                    .add(1)
                    .cast::<*mut HeapObjectHeader>()
                    .write(null_mut());
            }
        }
        self.old_weakrefs = new_with_weakref;
    }

    /// Perform a minor collection: find the objects from the nursery
    /// that remain alive and move them out.
    pub fn minor_collection(&mut self, safestack: &mut [&mut dyn Trace]) {
        unsafe {
            let verbose = read_uint_from_env("WAFFLE_GC_VERBOSE").unwrap_or_else(|| 0);
            let time = if verbose > 0 {
                eprintln!("gc-minor: start");
                Some(std::time::Instant::now())
            } else {
                None
            };
            self.los.prepare_for_marking(true);
            self.los.begin_marking(false);

            self.collect_roots_in_nursery(safestack);

            // visit the "probably young" objects with finalizers.  They
            // always all survive.
            if self.probably_young_objects_with_finalizers.len() > 0 {
                self.deal_with_young_objects_with_finalizers();
            }

            self.collect_oldrefs_to_nursery();
            if !self.young_weakrefs.is_empty() {
                self.invalidate_young_weakrefs();
            }
            if !self.young_objects_with_destructors.is_empty() {
                self.deal_with_young_objects_with_dtors();
            }
            if !self.nursery_object_shadows.is_empty() {
                self.nursery_object_shadows.clear();
            }
            self.los.sweep();
            self.nursery_free = self.nursery_map.as_mut_ptr();
            self.nursery_top = self.nursery_free.add(self.nursery_size);
            self.los.prepare_for_allocation(true);

            match time {
                Some(x) => {
                    eprintln!(
                        "gc-minor: end in {:.4}ms total memory used: {}",
                        x.elapsed().as_micros() as f64 / 1000.0,
                        formatted_size(self.get_total_memory_used())
                    );
                }
                _ => (),
            }
        }
    }

    pub unsafe fn major_collection(
        &mut self,
        safestack: &mut [&mut dyn Trace],
        reserving_size: usize,
    ) {
        let verbose = read_uint_from_env("WAFFLE_GC_VERBOSE").unwrap_or_else(|| 0);
        let time = if verbose > 0 {
            eprintln!("gc-major: start");
            Some(std::time::Instant::now())
        } else {
            None
        };
        self.objects_to_trace.clear();
        self.los.prepare_for_marking(false);
        self.los.begin_marking(true);
        self.collect_roots(safestack);
        self.visit_all_objects();

        self.kept_alive_by_finalizer = 0;
        if !self.old_objects_with_finalizers.is_empty() {
            self.deal_with_young_objects_with_finalizers();
        }
        self.objects_to_trace.clear();
        if self.old_weakrefs.len() != 0 {
            self.invalidate_old_weakrefs();
        }

        if self.old_objects_with_destructors.len() != 0 {
            self.deal_with_old_objects_with_dtors();
        }

        self.los.sweep();
        self.total_mi_memory = 0;
        libmimalloc_sys::mi_heap_visit_blocks(
            self.mi_heap,
            true,
            Some(visit_mi_heap),
            self as *mut Self as _,
        );
        let mut total_memory_used = self.get_total_memory_used() as f64;
        total_memory_used -= self.kept_alive_by_finalizer as f64;
        if total_memory_used < 0.0 {
            total_memory_used = 0.0;
        }

        let bounded = self.set_major_threshold_from(
            (total_memory_used * self.major_collection_threshold)
                .min(total_memory_used + self.max_delta)
                .round() as _,
            reserving_size,
        );

        if bounded
            && self.get_total_memory_used() as f64 + reserving_size as f64
                >= self.next_major_collection_threshold as f64
        {
            if self.max_heap_size_already_raised {
                out_of_memory("using too much memory, aborting");
            }

            self.max_heap_size_already_raised = true;
            panic_any(Box::new(MemoryError));
        }
        self.execute_finalizers();
        match time {
            Some(x) => {
                eprintln!(
                    "gc-major: end in {:.4}ms total memory used: {}",
                    x.elapsed().as_micros() as f64 / 1000.0,
                    formatted_size(self.get_total_memory_used())
                );
            }
            _ => (),
        }
    }

    pub fn get_total_memory_used(&mut self) -> usize {
        self.total_mi_memory + self.los.bytes
    }

    unsafe fn finalization_state(&self, obj: *const HeapObjectHeader) -> u8 {
        if (*obj).is_visited() {
            if (*obj).finalization_ordering() {
                2
            } else {
                3
            }
        } else {
            if (*obj).finalization_ordering() {
                1
            } else {
                0
            }
        }
    }

    unsafe fn bump_finalization_state_from_0_to_1(&mut self, obj: *mut HeapObjectHeader) {
        (*obj).set_finalization_ordering(true);
        self.kept_alive_by_finalizer += size_of::<HeapObjectHeader>() + self.get_size(obj);
    }

    unsafe fn recursively_bump_finalization_state_from_2_to_3(
        &mut self,
        obj: *mut HeapObjectHeader,
    ) {
        self.tmpstack.push_back(obj);
        while let Some(obj) = self.tmpstack.pop_back() {
            if (*obj).finalization_ordering() {
                (*obj).set_finalization_ordering(false);
                let mut closure = |object: &mut NonNull<HeapObjectHeader>| {
                    self.tmpstack.push_back(object.as_ptr());
                };
                ((*obj).tid().as_vtable.trace)(obj.add(1).cast(), &mut Marker { vis: &mut closure })
            }
        }
    }

    unsafe fn recursively_bump_finalization_state_from_1_to_2(
        &mut self,
        obj: *mut HeapObjectHeader,
    ) {
        self.objects_to_trace.push(obj);
        self.visit_all_objects();
    }

    pub unsafe fn deal_with_objects_with_finalizers(&mut self) {
        // Walk over list of objects with finalizers.
        // If it is not surviving, add it to the list of to-be-called
        // finalizers and make it survive, to make the finalizer runnable.

        let mut new_with_finalizers = SegmentedVec::new();
        let mut marked = SegmentedVec::new();
        let mut pending = SegmentedVec::new();
        while let Some(obj) = self.old_objects_with_finalizers.pop_back() {
            let obj = obj.cast::<HeapObjectHeader>();
            if (*obj).is_visited() {
                new_with_finalizers.push_back(obj.cast::<()>());
                continue;
            }

            marked.push_back(obj.cast::<()>());
            pending.push(obj);

            while let Some(y) = pending.pop() {
                let state = self.finalization_state(y);
                if state == 0 {
                    self.bump_finalization_state_from_0_to_1(y);
                    let mut closure = |object: &mut NonNull<HeapObjectHeader>| {
                        pending.push(object.as_ptr());
                    };
                    ((*y).tid().as_vtable.trace)(
                        y.add(1).cast(),
                        &mut Marker { vis: &mut closure },
                    );
                } else if state == 2 {
                    self.recursively_bump_finalization_state_from_2_to_3(y);
                }
            }
            self.recursively_bump_finalization_state_from_1_to_2(obj.cast());
        }

        while let Some(x) = marked.pop_back().map(|x| x.cast::<HeapObjectHeader>()) {
            let state = self.finalization_state(x);
            if state == 2 {
                self.run_finalizers.push_back(x.cast());
                self.recursively_bump_finalization_state_from_2_to_3(x);
            } else {
                new_with_finalizers.push_back(x.cast());
            }
        }
        self.tmpstack.truncate(0);
        pending.clear();
        marked.clear();
        self.old_objects_with_finalizers = new_with_finalizers;
    }

    pub unsafe fn collect_roots(&mut self, safestack: &mut [&mut dyn Trace]) {
        visit_roots(*self.roots.get(), |root, meta| {
            if meta.is_null() {
                self.objects_to_trace
                    .push(root.read() as *mut HeapObjectHeader);
            } else {
                let trace = std::mem::transmute::<_, fn(*mut (), &mut dyn Visitor)>(meta);

                let mut closure = |object: &mut NonNull<HeapObjectHeader>| {
                    self.objects_to_trace.push(object.as_ptr());
                };
                trace(root as _, &mut closure);
            }
        });

        for i in 0..self.run_finalizers.len() {
            let obj = self.run_finalizers[i].cast::<HeapObjectHeader>();
            self.objects_to_trace.push(obj);
        }

        for object in safestack {
            object.trace(&mut OldTrace { gc: self });
        }

        let mut hooks = replace(&mut self.trace_hooks, HashMap::new());

        for (_, hook) in hooks.iter_mut() {
            hook(&mut OldTrace { gc: self });
        }
        self.trace_hooks = hooks;
    }

    pub unsafe fn is_young(&self, obj: *mut HeapObjectHeader) -> bool {
        self.is_in_nursery(obj as _)
            || (PreciseAllocation::is_precise(obj as _) && !(*obj).is_visited())
    }

    pub unsafe fn visit_all_objects(&mut self) {
        while let Some(obj) = self.objects_to_trace.pop() {
            debug_assert!(!self.is_young(obj as _));
            self.visit(obj);
        }
    }

    pub unsafe fn visit(&mut self, obj: *mut HeapObjectHeader) {
        if (*obj).is_visited() || (*obj).no_heap_ptrs() {
            return;
        }

        (*obj).set_visited(true);

        let tid = (*obj).tid();
        (tid.as_vtable.trace)(
            obj.add(1).cast(),
            &mut Marker {
                vis: &mut OldTrace { gc: self },
            },
        )
    }

    pub unsafe fn collect_oldrefs_to_nursery<'a>(&'a mut self) {
        let mut vis = YoungTrace { gc: self };

        while let Some(obj) = vis.gc.old_objects_pointing_to_young.pop() {
            (*obj).set_track_young_ptrs(true);
            let tid = (*obj).tid();

            (tid.as_vtable.trace)(obj.add(1).cast(), &mut Marker { vis: &mut vis });
        }
        drop(vis);
    }

    pub unsafe fn deal_with_young_objects_with_finalizers(&mut self) {
        while self.probably_young_objects_with_finalizers.len() > 0 {
            let mut obj = NonNull::new_unchecked(
                self.probably_young_objects_with_finalizers
                    .pop()
                    .unwrap()
                    .cast::<HeapObjectHeader>(),
            );

            self.trace_drag_out(&mut obj, null_mut());

            self.old_objects_with_finalizers
                .push_back(obj.as_ptr() as _);
        }
    }

    pub unsafe fn deal_with_young_objects_with_dtors(&mut self) {
        while let Some(obj) = self.young_objects_with_destructors.pop() {
            let tid = (*obj).tid();
            if !self.is_forwarded(obj) {
                match tid.as_vtable.finalize {
                    Some(func) => func(obj.add(1).cast()),
                    _ => (),
                }
            } else {
                let obj = tid.as_word as *mut HeapObjectHeader;
                self.old_objects_with_destructors.push(obj);
            }
        }
    }
    pub unsafe fn deal_with_old_objects_with_dtors(&mut self) {
        let mut new_objects = SegmentedVec::new();
        while let Some(obj) = self.old_objects_with_destructors.pop() {
            if (*obj).is_visited() {
                new_objects.push(obj);
            } else {
                match (*obj).tid().as_vtable.finalize {
                    Some(f) => f(obj.add(1).cast()),
                    _ => (),
                }
            }
        }
        self.old_objects_with_destructors = new_objects;
    }

    pub unsafe fn execute_finalizers(&mut self) {
        if self.finalize_lock {
            return;
        }
        self.finalize_lock = true;
        while let Some(object) = self.run_finalizers.pop_back() {
            // We catch unwind here and continue executing finalizers
            let _ = std::panic::catch_unwind(|| {
                let hdr = object.cast::<HeapObjectHeader>();

                match (*hdr).tid().as_vtable.finalize {
                    Some(f) => f(hdr.add(1).cast()),
                    _ => (),
                }
            });
        }
        self.finalize_lock = false;
    }

    /// Do a minor (gen=0) or major (gen>0) collection
    pub fn collect(&mut self, gen: u8, safestack: &mut [&mut dyn Trace]) {
        self.minor_collection(safestack);
        if gen > 0 {
            unsafe {
                self.major_collection(safestack, 0);
            }
        }
    }
    #[cold]
    pub unsafe fn collect_and_reserve(
        &mut self,
        totalsize: usize,
        safestack: &mut [&mut dyn Trace],
    ) -> *mut u8 {
        self.minor_collection(safestack);

        if self.get_total_memory_used() > self.next_major_collection_threshold {
            self.major_collection(safestack, 0);
            // The nursery might not be empty now, because of execute_finalizers().
            // If it is almost full again, we need to fix it with another call to minor_collection().
            if self.nursery_free.add(totalsize) > self.nursery_top {
                self.minor_collection(safestack);
            }
        }
        let result = self.nursery_free;
        self.nursery_free = result.add(totalsize);
        result
    }

    pub unsafe fn external_malloc<A: 'static + Allocation>(
        &mut self,
        length: usize,
        alloc_young: bool,
        safestack: &mut [&mut dyn Trace],
    ) -> *mut HeapObjectHeader {
        let nonvarsize = size_of::<HeapObjectHeader>() + A::SIZE;

        let totalsize = if length == 0 {
            nonvarsize
        } else if length > 0 {
            debug_assert!(A::VARSIZE);
            let itemsize = A::VARSIZE_ITEM_SIZE;
            nonvarsize + (length * itemsize)
        } else {
            panic_any(Box::new(MemoryError));
        };

        if unlikely(self.get_total_memory_used() + totalsize > self.next_major_collection_threshold)
        {
            self.minor_collection(safestack);
            self.major_collection(safestack, totalsize);
        }

        let result = self.los.allocate(totalsize);

        result.write(HeapObjectHeader { hdr: HDR(0) });

        (*result).set_tid(Tid {
            as_vtable: getconst!(super::VTABLE<A>),
        });

        (*result).set_track_young_ptrs(!alloc_young);
        if !alloc_young {
            // sticky mark bit
            (*result).set_visited(true);
        }
        let obj = result.add(1).cast::<u8>();
        if A::VARSIZE {
            obj.add(A::VARSIZE_OFFSETOF_LENGTH)
                .cast::<usize>()
                .write(length);
        }

        result
    }
    #[inline]
    pub unsafe fn malloc_fixedsize<A: 'static + Allocation>(
        &mut self,
        safestack: &mut [&mut dyn Trace],
    ) -> *mut HeapObjectHeader {
        let totalsize = align_usize(size_of::<HeapObjectHeader>() + A::SIZE, 16);

        if A::FINALIZE && !A::LIGHT_FINALIZER {
            let obj = self.external_malloc::<A>(0, false, safestack);
            self.register_finalizer(obj);

            return obj;
        }

        let obj = if totalsize > NONLARGE_MAX {
            return self.external_malloc::<A>(0, true, safestack);
        } else {
            let mut result = self.nursery_free;

            self.nursery_free = self.nursery_free.add(totalsize);
            if unlikely(self.nursery_free > self.nursery_top) {
                result = self.collect_and_reserve(totalsize, safestack);
            }

            debug_assert!(!PreciseAllocation::is_precise(result as _));
            result.cast::<HeapObjectHeader>()
        };

        obj.write(HeapObjectHeader { hdr: HDR(0) });
        (*obj).set_tid(Tid {
            as_vtable: getconst!(super::VTABLE<A>),
        });
        if A::FINALIZE {
            self.young_objects_with_destructors.push(obj);
        }
        if A::HAS_WEAKPTR {
            self.young_weakrefs.push(obj);
        }

        obj
    }

    /// Allocates memory in GC heap with parameters specified in `A: Allocation`.
    #[inline]
    pub unsafe fn malloc_varsize<A: 'static + Allocation>(
        &mut self,
        length: usize,
        safestack: &mut [&mut dyn Trace],
    ) -> *mut HeapObjectHeader {
        let nonvarsize = size_of::<HeapObjectHeader>() + A::SIZE;

        let maxsize = NONLARGE_MAX as isize - nonvarsize as isize;
        let toobig = if maxsize < 0 {
            0
        } else if A::VARSIZE_ITEM_SIZE != 0 {
            (maxsize as usize / A::VARSIZE_ITEM_SIZE) + 1
        } else {
            isize::MAX as usize + 1
        };

        let obj = if unlikely(length >= toobig as usize) {
            self.external_malloc::<A>(length, true, safestack)
        } else {
            let totalsize = align_usize(nonvarsize + A::VARSIZE_ITEM_SIZE * length, 16);

            let mut result = self.nursery_free;
            self.nursery_free = result.add(totalsize);
            if unlikely(self.nursery_free > self.nursery_top) {
                result = self.collect_and_reserve(totalsize, safestack);
            }

            let obj = result.cast::<HeapObjectHeader>();
            obj.write(HeapObjectHeader { hdr: HDR(0) });
            (*obj).set_tid(Tid {
                as_vtable: getconst!(super::VTABLE<A>),
            });
            debug_assert!(!PreciseAllocation::is_precise(result as _));
            let p = obj.add(1).cast::<u8>();
            p.add(A::VARSIZE_OFFSETOF_LENGTH)
                .cast::<usize>()
                .write(length);
            obj
        };

        obj
    }

    pub unsafe fn register_finalizer(&mut self, obj: *mut HeapObjectHeader) {
        self.probably_young_objects_with_finalizers.push(obj as _);
    }
    #[inline]
    pub unsafe fn write_barrier(&mut self, addr: *mut HeapObjectHeader) {
        if (*addr).track_young_ptrs() {
            self.remember_young_pointer(addr);
        }
    }
    #[inline(never)]
    unsafe fn remember_young_pointer(&mut self, addr: *mut HeapObjectHeader) {
        self.old_objects_pointing_to_young.push(addr);
        (*addr).set_track_young_ptrs(false);
    }

    unsafe fn find_shadow(&mut self, obj: *mut HeapObjectHeader) -> *const HeapObjectHeader {
        if (*obj).has_shadow() {
            self.nursery_object_shadows
                .get(&(obj as usize))
                .copied()
                .expect("has_shadow flag is set but no shadow found") as _
        } else {
            self.allocate_shadow(obj)
        }
    }

    unsafe fn allocate_shadow(&mut self, obj: *mut HeapObjectHeader) -> *mut HeapObjectHeader {
        let totalsize = size_of::<HeapObjectHeader>() + self.get_size(obj);
        let shadowhdr = self
            .malloc_out_of_nursery(totalsize)
            .cast::<HeapObjectHeader>();

        // Initialize the shadow enough to be considered a
        // valid gc object.  If the original object stays
        // alive at the next minor collection, it will anyway
        // be copied over the shadow and overwrite the
        // following fields.  But if the object dies, then
        // the shadow will stay around and only be freed at
        // the next major collection, at which point we want
        // it to look valid (but ready to be freed).
        (*shadowhdr).hdr = (*obj).hdr;
        if (*obj).tid().as_vtable.is_varsize {
            let off = (*obj).tid().as_vtable.varsize.offset_of_length;
            let shadow = shadowhdr.add(1).cast::<u8>();
            shadow
                .add(off)
                .cast::<usize>()
                .write(obj.add(1).cast::<u8>().add(off).cast::<usize>().read());
        }

        (*obj).set_has_shadow(true);
        self.nursery_object_shadows
            .insert(obj as usize, shadowhdr as usize);
        shadowhdr
    }

    pub fn identity(&mut self, obj: *mut HeapObjectHeader) -> usize {
        if obj.is_null() {
            return 0;
        }
        if self.is_in_nursery(obj.cast()) {
            unsafe { self.find_shadow(obj) as usize }
        } else {
            obj as usize
        }
    }
}

fn read_float_and_factor_from_env(var: &str) -> Option<(f64, usize)> {
    let value = std::env::var(var);

    match value {
        Ok(mut value) => {
            if value.len() > 0 {
                if value.len() > 1
                    && (value.as_bytes()[value.len() - 1] == 'b' as u8
                        || value.as_bytes()[value.len() - 1] == 'B' as u8)
                {
                    value = value.as_str()[0..value.len() - 1].to_string();
                }
                let mut realvalue = value.as_str()[0..value.len() - 1].to_string();

                let at = value.len() - 1;
                let factor;
                if value.as_bytes()[at] == 'g' as u8 || value.as_bytes()[at] == 'G' as u8 {
                    factor = 1024 * 1024 * 1024;
                } else if value.as_bytes()[at] == 'm' as u8 || value.as_bytes()[at] == 'M' as u8 {
                    factor = 1024 * 1024;
                } else if value.as_bytes()[at] == 'm' as u8 || value.as_bytes()[at] == 'M' as u8 {
                    factor = 1024;
                } else {
                    realvalue = value;
                    factor = 1;
                }

                match realvalue.parse::<f64>() {
                    Ok(x) => Some((x, factor)),
                    _ => None,
                }
            } else {
                None
            }
        }
        _ => None,
    }
}

pub fn read_uint_from_env(var: &str) -> Option<usize> {
    let (value, factor) = read_float_and_factor_from_env(var)?;

    Some(value as usize * factor)
}

pub fn read_float_from_env(var: &str) -> Option<f64> {
    read_float_and_factor_from_env(var).map(|x| x.0)
}

pub struct YoungTrace<'a> {
    gc: &'a mut MiniMark,
}

impl<'a> Visitor for YoungTrace<'a> {
    fn mark_object(&mut self, root: &mut NonNull<HeapObjectHeader>) {
        unsafe {
            self.gc.trace_drag_out(root, null_mut());
        }
    }
}

pub struct OldTrace<'a> {
    gc: &'a mut MiniMark,
}

impl<'a> Visitor for OldTrace<'a> {
    fn mark_object(&mut self, root: &mut NonNull<HeapObjectHeader>) {
        self.gc.objects_to_trace.push(root.as_ptr());
    }
}

impl<'a, T: FnMut(&mut NonNull<HeapObjectHeader>)> Visitor for T {
    fn mark_object(&mut self, root: &mut NonNull<HeapObjectHeader>) {
        self(root);
    }
}

pub struct MemoryError;

unsafe extern "C" fn visit_mi_heap(
    heap: *const mi_heap_t,
    area: *const mi_heap_area_t,
    block: *mut c_void,
    block_size: usize,
    arg: *mut c_void,
) -> bool {
    let _ = heap;
    let _ = area;
    if block.is_null() {
        return true;
    }
    let heap = &mut *arg.cast::<MiniMark>();
    let object = block.cast::<HeapObjectHeader>();
    if (*object).is_visited() {
        (*object).set_visited(false);
        heap.total_mi_memory += block_size;
    } else {
        libmimalloc_sys::mi_free(block);
    }

    true
}

#[inline(always)]
pub const fn align_usize(value: usize, align: usize) -> usize {
    ((value.wrapping_add(align).wrapping_sub(1)).wrapping_div(align)).wrapping_mul(align)
    //((value + align - 1) / align) * align
}

impl Drop for MiniMark {
    #[inline(never)]
    fn drop(&mut self) {
        unsafe {
            libmimalloc_sys::mi_heap_destroy(self.mi_heap);
        }
    }
}
