use motoko_rts_macros::ic_mem_fn;

use crate::{
    continuation_table,
    memory::{Memory, MARK_ON_ALLOCATION},
    types::Value,
};

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

    #[cfg(debug_assertions)]
    sanity_checks::check_memory(mem);

    let heap_base = ic::get_aligned_heap_base() as usize;
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *continuation_table::continuation_table_loc(),
    };
    IncrementalGC::empty_call_stack_increment(mem, heap_base, roots);
}

struct State {
    pub heap_base: usize,
    pub mark_stack: MarkStack,
}

static mut STATE: Option<State> = None;

struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `mark_roots()`.
}

struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
}

impl<'a, M: Memory> IncrementalGC<'a, M> {
    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * The moving phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(mem: &mut M, heap_base: usize, roots: Roots) {
        let mut gc = IncrementalGC { mem };
        if STATE.is_none() {
            gc.start_run(heap_base, roots);
        }
        gc.increment();
    }

    #[inline]
    pub unsafe fn write_barrier(mem: &mut M, value: Value) {
        if value.is_ptr() {
            if let Some(state) = &STATE {
                if value.get_ptr() >= state.heap_base {
                    let mut gc = IncrementalGC { mem };
                    gc.mark_object(value);
                    gc.increment();
                }
            }
        }
    }

    unsafe fn start_run(&mut self, heap_base: usize, roots: Roots) {
        assert!(STATE.is_none());
        let mark_stack = MarkStack::new(self.mem);
        STATE = Some(State {
            heap_base,
            mark_stack,
        });
        MARK_ON_ALLOCATION = true;
        self.mark_roots(roots);
    }

    unsafe fn increment(&mut self) {
        println!(100, "GC increment");
    }

    unsafe fn mark_roots(&mut self, roots: Roots) {
        self.mark_static_roots(roots.static_roots);
        self.mark_potential_object(roots.continuation_table);
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            assert!((mutbox as usize) < Self::heap_base());
            self.mark_potential_object((*mutbox).field);
        }
    }

    unsafe fn mark_potential_object(&mut self, value: Value) {
        if value.is_ptr() && value.get_ptr() >= Self::heap_base() {
            self.mark_object(value)
        }
    }

    unsafe fn mark_object(&mut self, value: Value) {
        if let Some(state) = &mut STATE {
            println!(100, "Mark object {:#x}", value.get_ptr());
            assert!((value.get_ptr() >= state.heap_base));
            let object = value.as_obj();
            assert!(
                object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL
            );
            state.mark_stack.push(self.mem, value);
        } else {
            panic!("Invalid state");
        }
    }

    unsafe fn heap_base() -> usize {
        STATE.as_ref().unwrap().heap_base
    }
}
