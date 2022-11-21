use motoko_rts_macros::ic_mem_fn;

use crate::{
    continuation_table,
    memory::{Memory, MARK_ON_ALLOCATION},
    types::{Obj, Value, TAG_ARRAY, TAG_ARRAY_SLICE_MIN},
    visitor::visit_pointer_fields,
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
    sanity_checks::check_memory();

    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        last_free: ic::LAST_HP as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *continuation_table::continuation_table_loc(),
    };
    IncrementalGC::empty_call_stack_increment(mem, limits, roots);
}

enum Phase {
    Pause,
    Mark(MarkState),
}

struct MarkState {
    pub heap_base: usize,
    pub mark_stack: MarkStack,
}

static mut PHASE: Phase = Phase::Pause;

struct Limits {
    pub base: usize,
    pub last_free: usize,
    pub free: usize,
}

struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `mark_roots()`.
}

const INCREMENT_LIMIT: usize = 1024; // soft limit on marked fields per GC run
const START_THRESHOLD: usize = 32 * 1024 * 1024; // new allocation since last GC run

struct IncrementalGC<'a, M: Memory> {
    mem: &'a mut M,
    steps: usize,
}

impl<'a, M: Memory + 'a> IncrementalGC<'a, M> {
    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase is only started on an empty call stack.
    /// * The moving phase can only be completed on an empty call stack.
    pub unsafe fn empty_call_stack_increment(mem: &'a mut M, limits: Limits, roots: Roots) {
        let mut gc = Self::instance(mem);
        if Self::pausing() && Self::should_start_gc(&limits) {
            gc.start_run(limits.base, roots);
        }
        gc.increment();
    }

    /// Pre-update field-level write barrier peforming snapshot-at-the-beginning marking.
    /// The barrier is only effective while the GC is in the mark phase.
    /// A GC increment is implicitly combined with the write barrier.
    #[inline]
    pub unsafe fn write_barrier(mem: &'a mut M, value: Value) {
        if value.is_ptr() {
            if let Phase::Mark(state) = &PHASE {
                if value.get_ptr() >= state.heap_base {
                    let mut gc = Self::instance(mem);
                    gc.mark_object(value);
                    gc.increment();
                }
            }
        }
    }

    fn should_start_gc(limits: &Limits) -> bool {
        assert!(limits.last_free <= limits.free);
        limits.free - limits.last_free >= START_THRESHOLD
    }

    fn instance(mem: &'a mut M) -> IncrementalGC<'a, M> {
        IncrementalGC { mem, steps: 0 }
    }

    unsafe fn start_run(&mut self, heap_base: usize, roots: Roots) {
        assert!(Self::pausing());
        let mark_stack = MarkStack::new(self.mem);
        let state = MarkState {
            heap_base,
            mark_stack,
        };
        PHASE = Phase::Mark(state);
        MARK_ON_ALLOCATION = true;
        self.mark_roots(roots);
    }

    unsafe fn pausing() -> bool {
        match &PHASE {
            Phase::Pause => true,
            _ => false,
        }
    }

    unsafe fn increment(&mut self) {
        println!(100, "GC increment");
        self.steps = 0;
        match &mut PHASE {
            Phase::Pause => {}
            Phase::Mark(_) => self.mark_phase_increment(),
        }
    }

    unsafe fn mark_roots(&mut self, roots: Roots) {
        self.mark_static_roots(roots.static_roots);
        self.mark_continuation_table(roots.continuation_table);
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            assert!((mutbox as usize) < Self::heap_base());
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= Self::heap_base() {
                self.mark_object(value);
            }
        }
    }

    unsafe fn mark_continuation_table(&mut self, continuation_table: Value) {
        if continuation_table.is_ptr() {
            self.mark_object(continuation_table);
        }
    }

    unsafe fn mark_object(&mut self, value: Value) {
        if let Phase::Mark(state) = &mut PHASE {
            assert!((value.get_ptr() >= state.heap_base));
            let object = value.as_obj();
            assert!(
                object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL
            );
            if object.is_marked() {
                return;
            }
            object.mark();
            state.mark_stack.push(self.mem, value);
        } else {
            panic!("Invalid phase");
        }
    }

    unsafe fn mark_phase_increment(&mut self) {
        if let Phase::Mark(state) = &mut PHASE {
            while let Some(value) = state.mark_stack.pop() {
                self.mark_fields(value.as_obj());
                self.steps += 1;
                if self.steps > INCREMENT_LIMIT {
                    return;
                }
            }
            self.mark_complete();
        } else {
            panic!("Invalid phase")
        }
    }

    unsafe fn mark_complete(&mut self) {
        println!(100, "Mark complete");
        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        sanity_checks::check_mark_completeness(self.mem);
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        assert!(object.is_marked());
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            Self::heap_base(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
                gc.steps += 1;
            },
            |gc, slice_start, array| {
                const SLICE_INCREMENT: u32 = 127;
                assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    (*array).header.set_tag(new_start, true);
                    if let Phase::Mark(state) = &mut PHASE {
                        state
                            .mark_stack
                            .push(gc.mem, Value::from_ptr(array as usize));
                    } else {
                        panic!("Invalid phase");
                    }
                    new_start
                } else {
                    (*array).header.set_tag(TAG_ARRAY, true);
                    array.len()
                }
            },
        );
    }

    unsafe fn heap_base() -> usize {
        match &PHASE {
            Phase::Mark(state) => state.heap_base,
            _ => panic!("Invalid phase"),
        }
    }
}
