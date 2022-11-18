use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::Value};

pub mod mark_stack;
#[cfg(debug_assertions)]
pub mod sanity_checks;
pub mod write_barrier;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    incremental_gc(mem)
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;

    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table_ptr_loc: crate::continuation_table::continuation_table_loc(),
    };
    let heap = Heap { mem, limits, roots };
    let mut gc = IncrementalGC::new(heap);
    gc.run();
}

pub struct Heap<'a, M: Memory> {
    pub mem: &'a mut M,
    pub limits: Limits,
    pub roots: Roots,
}

pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_ptr_loc: *mut Value,
}

pub struct Limits {
    pub base: usize,
    pub free: usize,
}

pub struct IncrementalGC<'a, M: Memory> {
    pub heap: Heap<'a, M>,
}

impl<'a, M: Memory> IncrementalGC<'a, M> {
    pub fn new(heap: Heap<M>) -> IncrementalGC<M> {
        IncrementalGC { heap }
    }

    pub unsafe fn run(&mut self) {
        println!(100, "Incremental GC");

        #[cfg(debug_assertions)]
        sanity_checks::check_memory(&self.heap.limits, &self.heap.roots)
    }
}
