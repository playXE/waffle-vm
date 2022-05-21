use super::*;
use std::ptr::null_mut;
/// Precise allocation used for large objects (>= LARGE_CUTOFF).
/// Starlight uses mimalloc that already knows what to do for large allocations. The GC shouldn't
/// have to think about such things. That's where PreciseAllocation comes in. We will allocate large
/// objects directly using mi_malloc, and put the PreciseAllocation header just before them. We can detect
/// when a *mut GcPointerBase is a PreciseAllocation because it will have the ATOM_SIZE / 2 bit set.
#[repr(C)]
pub struct PreciseAllocation {
    //pub link: LinkedListLink,
    /// allocation request size
    pub cell_size: usize,
    /// index in precise_allocations
    pub index_in_space: u32,
    /// Is alignment adjusted?
    //pub is_newly_allocated: bool,
    pub adjusted_alignment: bool,
    pub mature: bool,
    /// Is this even valid allocation?
    pub has_valid_cell: bool,
    pub is_newly_allocated: bool,
}

impl PreciseAllocation {
    /// Alignment of allocation.
    pub const ALIGNMENT: usize = 16;
    /// Alignment of pointer returned by `Self::cell`.
    pub const HALF_ALIGNMENT: usize = Self::ALIGNMENT / 2;
    /// Check if raw_ptr is precisely allocated.
    pub fn is_precise(raw_ptr: *mut ()) -> bool {
        (raw_ptr as usize & Self::HALF_ALIGNMENT) != 0
    }

    /// Create PreciseAllocation from pointer
    pub fn from_cell(ptr: *mut HeapObjectHeader) -> *mut Self {
        unsafe {
            ptr.cast::<u8>()
                .offset(-(Self::header_size() as isize))
                .cast()
        }
    }
    /// Return base pointer
    #[inline]
    pub fn base_pointer(&self) -> *mut () {
        if self.adjusted_alignment {
            ((self as *const Self as isize) - (Self::HALF_ALIGNMENT as isize)) as *mut ()
        } else {
            self as *const Self as *mut ()
        }
    }

    /// Return cell address, it is always aligned to `Self::HALF_ALIGNMENT`.
    pub fn cell(&self) -> *mut HeapObjectHeader {
        let addr = unsafe { (self as *const Self as *const u8).add(Self::header_size()) };
        addr as _
    }
    /// Return true if raw_ptr is above lower bound
    pub fn above_lower_bound(&self, raw_ptr: *mut ()) -> bool {
        let ptr = raw_ptr;
        let begin = self.cell() as *mut ();
        ptr >= begin
    }
    /// Return true if raw_ptr below upper bound
    pub fn below_upper_bound(&self, raw_ptr: *mut ()) -> bool {
        let ptr = raw_ptr;
        let begin = self.cell() as *mut ();
        let end = (begin as usize + self.cell_size) as *mut ();
        ptr <= (end as usize + 8) as *mut ()
    }
    /// Returns header size + required alignment to make cell be aligned to 8.
    pub const fn header_size() -> usize {
        ((core::mem::size_of::<PreciseAllocation>() + Self::HALF_ALIGNMENT - 1)
            & !(Self::HALF_ALIGNMENT - 1))
            | Self::HALF_ALIGNMENT
    }
    /// Does this allocation contains raw_ptr?
    pub fn contains(&self, raw_ptr: *mut ()) -> bool {
        self.above_lower_bound(raw_ptr) && self.below_upper_bound(raw_ptr)
    }
    pub fn make_mature(&mut self) {
        self.mature = true;
    }
    /// Finalize cell if this allocation is not marked.
    pub fn sweep(&mut self) -> bool {
        true
    }
    /// Try to create precise allocation (no way that it will return null for now).
    pub fn try_create(size: usize, index_in_space: u32) -> *mut Self {
        let adjusted_alignment_allocation_size = Self::header_size() + size + Self::HALF_ALIGNMENT;
        unsafe {
            let mut space = libc::malloc(adjusted_alignment_allocation_size).cast::<u8>();

            let mut adjusted_alignment = false;
            if !is_aligned_for_precise_allocation(space) {
                space = space.add(Self::HALF_ALIGNMENT);
                adjusted_alignment = true;
                assert!(is_aligned_for_precise_allocation(space));
            }
            assert!(size != 0);
            space.cast::<Self>().write(Self {
                //link: LinkedListLink::new(),
                adjusted_alignment,

                //is_newly_allocated: true,
                has_valid_cell: true,
                mature: false,
                cell_size: size,
                index_in_space,
                is_newly_allocated: false,
            });

            space.cast()
        }
    }
    pub fn is_newly_allocated(&self) -> bool {
        self.is_newly_allocated
    }

    /// return cell size
    pub fn cell_size(&self) -> usize {
        self.cell_size
    }
    /// Destroy this allocation
    pub fn destroy(&mut self) {
        let base = self.base_pointer();
        unsafe {
            libc::free(base as _);
        }
    }
}
/// Check if `mem` is aligned for precise allocation
pub fn is_aligned_for_precise_allocation(mem: *mut u8) -> bool {
    let allocable_ptr = mem as usize;
    (allocable_ptr & (PreciseAllocation::ALIGNMENT - 1)) == 0
}
/// This space contains objects which are larger than the size limits of other spaces.
/// Each object gets its own malloc'd region of memory.
/// Large objects are never moved by the garbage collector.
pub struct LargeObjectSpace {
    pub(crate) allocations: Vec<*mut PreciseAllocation>,
    pub(crate) bytes: usize,
    pub(crate) precise_allocations_nursery_offset: usize,
    pub(crate) precise_allocations_offest_for_this_collection: usize,
    pub(crate) precise_allocations_offset_nursery_for_sweep: usize,
    pub(crate) precise_allocations_for_this_collection_size: usize,
    pub(crate) precise_allocations_for_this_collection_begin: *mut *mut PreciseAllocation,
    pub(crate) precise_allocations_for_this_collection_end: *mut *mut PreciseAllocation,
}

impl LargeObjectSpace {
    pub fn is_young(&self, object: *const HeapObjectHeader) -> bool {
        unsafe { !(*object).is_visited() }
    }
    pub(crate) fn new() -> Self {
        Self {
            allocations: Vec::new(),
            bytes: 0,
            precise_allocations_nursery_offset: 0,
            precise_allocations_offest_for_this_collection: 0,
            precise_allocations_offset_nursery_for_sweep: 0,
            precise_allocations_for_this_collection_end: null_mut(),
            precise_allocations_for_this_collection_begin: null_mut(),
            precise_allocations_for_this_collection_size: 0,
        }
    }
    pub fn begin_marking(&mut self, full: bool) {
        if full {
            for alloc in self.allocations.iter() {
                unsafe {
                    (*(**alloc).cell()).set_visited(false);
                }
            }
        }
    }
    #[inline]
    pub fn contains(&self, pointer: *const u8) -> *mut HeapObjectHeader {
        // check only for eden space pointers when conservatively scanning.
        unsafe {
            if self.precise_allocations_for_this_collection_size != 0 {
                if (**self.precise_allocations_for_this_collection_begin)
                    .above_lower_bound(pointer as _)
                    && (**(self.precise_allocations_for_this_collection_end.sub(1)))
                        .below_upper_bound(pointer as _)
                {
                    let prec = PreciseAllocation::from_cell(pointer as _);
                    let slice = std::slice::from_raw_parts(
                        self.precise_allocations_for_this_collection_begin,
                        self.precise_allocations_for_this_collection_size,
                    );
                    let result = slice.binary_search_by(|ptr| ptr.cmp(&prec));
                    if let Ok(_) = result {
                        return (*prec).cell();
                    }
                }
            }
            null_mut()
        }
    }

    pub fn prepare_for_allocation(&mut self, eden: bool) {
        if eden {
            self.precise_allocations_offset_nursery_for_sweep =
                self.precise_allocations_nursery_offset;
        } else {
            self.precise_allocations_offset_nursery_for_sweep = 0;
        }
        self.precise_allocations_nursery_offset = self.allocations.len();
    }

    pub fn prepare_for_marking(&mut self, eden: bool) {
        if eden {
            self.precise_allocations_offest_for_this_collection =
                self.precise_allocations_nursery_offset;
        } else {
            self.precise_allocations_offest_for_this_collection = 0;
        }
    }
    /// Sort allocations before consrvative scan.
    pub fn prepare_for_conservative_scan(&mut self) {
        unsafe {
            self.precise_allocations_for_this_collection_begin = self
                .allocations
                .as_mut_ptr()
                .add(self.precise_allocations_offest_for_this_collection);
            self.precise_allocations_for_this_collection_size =
                self.allocations.len() - self.precise_allocations_offest_for_this_collection;
            self.precise_allocations_for_this_collection_end =
                self.allocations.as_mut_ptr().add(self.allocations.len());
            let slice = std::slice::from_raw_parts_mut(
                self.precise_allocations_for_this_collection_begin,
                self.precise_allocations_for_this_collection_size,
            );

            slice.sort_by(|a, b| a.cmp(b));

            let mut index = self.precise_allocations_offest_for_this_collection;
            let mut start = self.precise_allocations_for_this_collection_begin;
            let end = self.precise_allocations_for_this_collection_end;
            while start != end {
                (**start).index_in_space = index as _;
                index += 1;
                start = start.add(1);
            }
        }
    }

    pub fn sweep(&mut self) {
        let mut src_index = self.precise_allocations_offset_nursery_for_sweep;

        let mut dst_index = src_index;
        while src_index < self.allocations.len() {
            let allocation = self.allocations[src_index];
            src_index += 1;
            unsafe {
                let cell = (*allocation).cell();
                if !(*cell).is_visited() {
                    self.bytes -= (*allocation).cell_size();

                    (*allocation).destroy();

                    continue;
                } else {
                    (*allocation).index_in_space = dst_index as u32;
                    self.allocations[dst_index] = allocation;
                    dst_index += 1;
                    // (*allocation).clear_marked();
                }
            }
        }
        self.allocations.resize(dst_index, null_mut());
        self.precise_allocations_nursery_offset = self.allocations.len();
    }

    pub fn allocate(&mut self, size: usize) -> *mut HeapObjectHeader {
        unsafe {
            let index = self.allocations.len();
            let memory = PreciseAllocation::try_create(size, index as _);
            if memory.is_null() {
                panic!("LargeObjectSpace: OOM");
            }

            self.allocations.push(memory);
            self.bytes += (*memory).cell_size();

            (*memory).cell()
        }
    }
}

impl Drop for LargeObjectSpace {
    fn drop(&mut self) {
        while let Some(alloc) = self.allocations.pop() {
            unsafe {
                (*alloc).destroy();
            }
        }
    }
}
