use motoko_rts_macros::ic_mem_fn;

use crate::{memory::Memory, types::Value};

use self::mark_stack::MarkStack;

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
    #[cfg(debug_assertions)]
    sanity_checks::check_memory(&limits, &roots);
    let mut heap = Heap { mem, limits, roots };
    INCREMENTAL_GC.increment_empty_call_stack(&mut heap);
}

pub struct Heap<'a, M: Memory> {
    pub mem: &'a mut M,
    pub limits: Limits,
    pub roots: Roots,
}

pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_ptr_loc: *mut Value,
    // If new roots are added in future, extend `mark_roots()`.
}

pub struct Limits {
    pub base: usize,
    pub free: usize,
}

enum State {
    Pausing,
    Marking(MarkStack),
}

pub struct IncrementalGC {
    state: State,
}

static mut INCREMENTAL_GC: IncrementalGC = IncrementalGC {
    state: State::Pausing,
};

impl IncrementalGC {
    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// Since the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * The moving phase can only be completed on an empty call stack.
    pub unsafe fn increment_empty_call_stack<M: Memory>(&mut self, heap: &mut Heap<M>) {
        if let State::Pausing = self.state {
            self.start(heap);
        }
        self.increment();
    }

    /// General GC increment invoked at any time, except during critical operations such as upgrades.
    pub unsafe fn increment(&mut self) {
        println!(100, "GC increment");
    }

    // Start a new GC run
    unsafe fn start<M: Memory>(&mut self, heap: &mut Heap<M>) {
        if let State::Pausing = self.state {
            println!(100, "Starting mark phase");
            let stack = MarkStack::new(heap.mem);
            self.state = State::Marking(stack);
            self.mark_roots();
        } else {
            panic!("Invalid GC start point");
        }
    }

    unsafe fn mark_roots(&mut self) {}
}
