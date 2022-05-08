use std::{mem::size_of, ptr::null_mut};

#[cfg(target_pointer_width = "32")]
pub const WORD_POWER2: usize = 2;
#[cfg(target_pointer_width = "64")]
pub const WORD_POWER2: usize = 3;

pub const WORD: usize = size_of::<usize>();

pub struct Arena {
    base: *mut u8,
    nfreepages: isize,
    totalpages: isize,
    freepages: *mut u8,
    nextarena: *mut Self,
}

pub struct PageHeader {
    nextpage: *mut Self,
    arena: *mut Arena,
    nfree: usize,
    freeblock: *mut u8,
}

pub struct ArenaCollection {
    arena_size: usize,
    page_size: usize,
    small_request_threshold: usize,
    arenas_count: usize,
    page_for_size: Box<[*mut PageHeader]>,
    full_page_for_size: Box<[*mut PageHeader]>,
    old_page_for_size: Box<[*mut PageHeader]>,
    old_full_page_for_size: Box<[*mut PageHeader]>,
    nblocks_for_size: Vec<usize>,
    old_arenas_lists: Box<[*mut Arena]>,
    arenas_list: Box<[*mut Arena]>,
    current_arena: *mut Arena,

    min_empty_nfreepages: usize,
    num_uninitialized_pages: usize,
    pub total_memory_used: usize,
    peak_memory_used: usize,
    pub total_memory_alloced: usize,
    peak_memory_alloced: usize,
    max_pages_per_arena: usize,
    size_class_with_old_pages: isize,
}

impl ArenaCollection {
    pub fn new(arena_size: usize, page_size: usize, small_request_threshold: usize) -> Box<Self> {
        let length = small_request_threshold / WORD + 1;
        println!("len {}", arena_size / page_size);
        let mut this = Box::new(Self {
            arena_size,
            page_size,
            small_request_threshold,
            arenas_count: 0,
            page_for_size: vec![null_mut(); length].into_boxed_slice(),
            full_page_for_size: vec![null_mut(); length].into_boxed_slice(),
            old_full_page_for_size: vec![null_mut(); length].into_boxed_slice(),
            old_page_for_size: vec![null_mut(); length].into_boxed_slice(),
            nblocks_for_size: vec![0; length],
            max_pages_per_arena: arena_size / page_size,
            arenas_list: vec![null_mut(); arena_size / page_size].into_boxed_slice(),
            old_arenas_lists: vec![null_mut(); arena_size / page_size].into_boxed_slice(),
            current_arena: null_mut(),
            min_empty_nfreepages: arena_size / page_size,
            num_uninitialized_pages: 0,
            total_memory_alloced: 0,
            total_memory_used: 0,
            peak_memory_alloced: 0,
            peak_memory_used: 0,
            size_class_with_old_pages: -1,
        });
        for i in 1..length {
            this.nblocks_for_size[i] = (page_size - size_of::<PageHeader>()) / (WORD * i);
        }

        this
    }

    unsafe fn pick_next_arena(&mut self) -> bool {
        let mut i = self.min_empty_nfreepages;
        while i < self.max_pages_per_arena {
            if !self.arenas_list[i].is_null() {
                self.current_arena = self.arenas_list[i];
                self.arenas_list[i] = (*self.current_arena).nextarena;
                return true;
            }
            i += 1;
            self.min_empty_nfreepages = i;
        }

        false
    }

    pub unsafe fn allocate_new_page(&mut self, size_class: usize) -> *mut PageHeader {
        if self.current_arena.is_null() {
            self.allocate_new_arena();
        }

        let arena = self.current_arena;
        let result = (*arena).freepages;
        let freepages;
        if (*arena).nfreepages > 0 {
            (*arena).nfreepages -= 1;
            freepages = result.cast::<*mut u8>().read();
        } else {
            self.num_uninitialized_pages -= 1;
            if self.num_uninitialized_pages > 0 {
                freepages = result.add(self.page_size);
            } else {
                freepages = null_mut();
            }
        }

        (*arena).freepages = freepages;

        if freepages.is_null() {
            (*arena).nextarena = self.arenas_list[0];
            self.current_arena = null_mut();
        }

        let page = result.cast::<PageHeader>();
        page.write(PageHeader {
            arena,
            nfree: 0,
            freeblock: result.add(size_of::<PageHeader>()),
            nextpage: null_mut(),
        });
        self.page_for_size[size_class] = page;
        page
    }

    pub fn malloc(&mut self, size: usize) -> *mut u8 {
        let nsize = size;
        self.total_memory_used += size;

        let size_class = nsize >> WORD_POWER2;
        let mut page = self.page_for_size[size_class];

        if page.is_null() {
            page = unsafe { self.allocate_new_page(size_class) };
        }

        unsafe {
            let result = (*page).freeblock;

            let freeblock;
            if (*page).nfree > 0 {
                (*page).nfree -= 1;
                freeblock = result.cast::<*mut u8>().read();
            } else {
                freeblock = result.add(size);
            }
            (*page).freeblock = freeblock;
            if freeblock as usize - page as usize > self.page_size - size {
                self.page_for_size[size_class] = (*page).nextpage;
                (*page).nextpage = self.full_page_for_size[size_class];
                self.full_page_for_size[size_class] = page;
            }
            result
        }
    }

    pub unsafe fn free_page(&mut self, page: *mut PageHeader) {
        let arena = (*page).arena;
        (*arena).nfreepages += 1;
        page.cast::<*mut u8>().write((*arena).freepages);
        (*arena).freepages = page.cast();
    }

    pub fn mass_free_prepare(&mut self) {
        self.peak_memory_used = self.peak_memory_used.max(self.total_memory_used);
        self.total_memory_used = 0;

        let mut size_class = self.small_request_threshold >> WORD_POWER2;
        self.size_class_with_old_pages = size_class as _;

        while size_class >= 1 {
            self.old_page_for_size[size_class] = self.page_for_size[size_class];
            self.old_full_page_for_size[size_class] = self.full_page_for_size[size_class];
            self.page_for_size[size_class] = null_mut();
            self.full_page_for_size[size_class] = null_mut();
            size_class -= 1;
        }
    }

    pub unsafe fn mass_free_in_pages(
        &mut self,
        size_class: usize,
        ok_to_free_func: &mut impl FnMut(*mut u8) -> bool,
        mut max_pages: isize,
    ) -> isize {
        let nblocks = self.nblocks_for_size[size_class];
        let block_size = size_class * WORD;
        let mut remaining_partial_pages = self.page_for_size[size_class];
        let mut remaining_full_pages = self.full_page_for_size[size_class];

        let mut step = 0;
        while step < 2 {
            let mut page;
            if step == 0 {
                // first free full pages, works better for incremental collector
                page = self.old_full_page_for_size[size_class];
                self.old_full_page_for_size[size_class] = null_mut();
            } else {
                page = self.old_page_for_size[size_class];
                self.old_page_for_size[size_class] = null_mut();
            }

            while !page.is_null() {
                let surviving = self.walk_page(page, block_size, ok_to_free_func);
                let nextpage = (*page).nextpage;
                if surviving == nblocks {
                    (*page).nextpage = remaining_full_pages;
                    remaining_full_pages = page;
                } else if surviving > 0 {
                    (*page).nextpage = remaining_partial_pages;
                    remaining_partial_pages = page;
                } else {
                    self.free_page(page);
                }

                max_pages -= 1;
                if max_pages <= 0 {
                    if step == 0 {
                        self.old_full_page_for_size[size_class] = nextpage;
                    } else {
                        self.old_page_for_size[size_class] = nextpage;
                    }
                    step = 99;
                    break;
                }
                page = nextpage;
            }
            step += 1;
        }
        self.page_for_size[size_class] = remaining_partial_pages;
        self.full_page_for_size[size_class] = remaining_full_pages;
        max_pages
    }

    pub unsafe fn walk_page(
        &mut self,
        page: *mut PageHeader,
        block_size: usize,
        ok_to_free_func: &mut impl FnMut(*mut u8) -> bool,
    ) -> usize {
        let mut freeblock = (*page).freeblock;
        let mut prevfreeblockat = &mut (*page).freeblock as *mut *mut u8 as *mut u8;

        let mut obj = page as *mut u8;
        obj = obj.add(size_of::<PageHeader>());
        let mut surviving = 0;
        let mut skip_free_blocks = (*page).nfree;

        loop {
            if obj == freeblock {
                if skip_free_blocks == 0 {
                    break;
                }

                skip_free_blocks -= 1;
                prevfreeblockat = obj;
                freeblock = obj.cast::<*mut u8>().read();
            } else {
                if ok_to_free_func(obj) {
                    prevfreeblockat.cast::<*mut u8>().write(obj);
                    prevfreeblockat = obj;
                    obj.cast::<*mut u8>().write(freeblock);

                    (*page).nfree += 1;
                } else {
                    surviving += 1;
                }
            }

            obj = obj.add(block_size);
        }

        self.total_memory_used += surviving * block_size;
        surviving
    }

    #[inline(never)]
    pub unsafe fn allocate_new_arena(&mut self) {
        if self.pick_next_arena() {
            return;
        }

        self.rehash_arena_lists();
        if self.pick_next_arena() {
            return;
        }

        let arena_base = libc::malloc(self.arena_size).cast::<u8>();
        self.total_memory_alloced += self.arena_size;
        self.peak_memory_alloced = self.total_memory_alloced.max(self.peak_memory_alloced);

        if arena_base.is_null() {
            out_of_memory("out of memory: couldn't allocate the next arena");
        }

        let arena_end = arena_base.add(self.arena_size);
        let firstpage = start_of_page(arena_base.add(self.page_size).sub(1), self.page_size);
        let npages = (arena_end as usize - firstpage as usize) / self.page_size;

        let arena = libc::malloc(size_of::<Arena>()).cast::<Arena>();
        arena.write(Arena {
            base: arena_base.cast(),
            nfreepages: 0,
            totalpages: npages as _,
            freepages: firstpage.cast(),
            nextarena: null_mut(),
        });
        self.num_uninitialized_pages = npages;
        self.current_arena = arena;
        self.arenas_count += 1;
    }

    pub unsafe fn mass_free(&mut self, ok_to_free_func: impl FnMut(*mut u8) -> bool) {
        self.mass_free_prepare();
        assert!(
            self.mass_free_incremental(ok_to_free_func, isize::MAX),
            "non-incremental mass_free_in_pages returned false"
        );
    }

    pub unsafe fn mass_free_incremental(
        &mut self,
        mut ok_to_free_func: impl FnMut(*mut u8) -> bool,
        mut max_pages: isize,
    ) -> bool {
        let mut size_class = self.size_class_with_old_pages;
        while size_class >= 1 {
            max_pages = self.mass_free_in_pages(size_class as _, &mut ok_to_free_func, max_pages);
            if max_pages <= 0 {
                self.size_class_with_old_pages = size_class;
                return false;
            }
            size_class -= 1;
        }
        if size_class >= 0 {
            self.rehash_arena_lists();
            self.size_class_with_old_pages = -1
        }
        return true;
    }

    unsafe fn rehash_arena_lists(&mut self) {
        std::mem::swap(&mut self.old_arenas_lists, &mut self.arenas_list);

        for i in 0..self.max_pages_per_arena {
            self.arenas_list[i] = null_mut();
        }

        for i in 0..self.max_pages_per_arena {
            let mut arena = self.old_arenas_lists[i];

            while !arena.is_null() {
                let nextarena = (*arena).nextarena;
                if (*arena).nfreepages == (*arena).totalpages {
                    self.total_memory_alloced -= self.arena_size;
                    libc::free((*arena).base.cast());
                    libc::free(arena.cast());
                    self.arenas_count -= 1;
                } else {
                    let n = (*arena).nfreepages as usize;
                    (*arena).nextarena = self.arenas_list[n];
                    self.arenas_list[n] = arena;
                }

                arena = nextarena;
            }
        }

        self.min_empty_nfreepages = 1;
    }
}

pub fn out_of_memory(message: &str) -> ! {
    eprintln!("{}", message);
    std::process::abort()
}

pub fn start_of_page(addr: *mut u8, page_size: usize) -> *mut u8 {
    let offset = addr as usize % page_size;
    (addr as usize - offset) as _
}
