pub struct Simple {
    mi_heap: *mut libmimalloc_sys::mi_heap_t,

    allocated: usize,
    threshold: usize,
}
