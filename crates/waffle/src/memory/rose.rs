use gc::allocator::{PageReleaseMode, DEFAULT_PAGE_RELEASE_THRESHOLD};

pub mod gc;

pub const DEFAULT_HEAP_SIZE: usize = 128 * 1024 * 1024;
pub const DEFAULT_MIN_HEAP_SIZE: usize = 1 * 1024 * 1024;
pub const DEFAULT_MAX_HEAP_SIZE: usize = 128 * 1024 * 1024;
pub const DEFAULT_INITIAL_THRESHOLD: usize = 4 * 1024 * 1024;
pub const DEFAULT_GROWTH_MULTIPLIER: f64 = 1.5;
#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub struct HeapParams {
    pub heap_size: usize,
    pub max_heap_size: usize,
    pub min_heap_size: usize,
    pub init_threshold: usize,
    pub page_release_mode: PageReleaseMode,
    pub page_release_threshold: usize,
    pub growth_multiplier: f64,
    pub verbose: u8,
}

impl Default for HeapParams {
    fn default() -> Self {
        Self {
            heap_size: DEFAULT_HEAP_SIZE,
            max_heap_size: DEFAULT_MAX_HEAP_SIZE,
            min_heap_size: DEFAULT_MIN_HEAP_SIZE,
            init_threshold: DEFAULT_INITIAL_THRESHOLD,
            page_release_mode: PageReleaseMode::All,
            page_release_threshold: DEFAULT_PAGE_RELEASE_THRESHOLD,
            growth_multiplier: DEFAULT_GROWTH_MULTIPLIER,
            verbose: 0,
        }
    }
}
