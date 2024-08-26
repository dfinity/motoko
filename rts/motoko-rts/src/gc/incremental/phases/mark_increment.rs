use motoko_rts_macros::{classical_persistence, enhanced_orthogonal_persistence};

use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        mark_stack::{MarkStack, STACK_EMPTY},
        partitioned_heap::PartitionedHeap,
        roots::{visit_roots, Roots},
        time::BoundedTime,
        State,
    },
    memory::Memory,
    stable_option::StableOption,
    types::*,
    visitor::visit_pointer_fields,
};

/// Use a long-term representation by relying on C layout.
#[repr(C)]
pub struct MarkState {
    mark_stack: MarkStack,
    complete: bool,
}

pub struct MarkIncrement<'a, M: Memory> {
    mem: &'a mut M,
    time: &'a mut BoundedTime,
    heap: &'a mut PartitionedHeap,
    mark_stack: &'a mut MarkStack,
    complete: &'a mut bool,
}

impl<'a, M: Memory + 'a> MarkIncrement<'a, M> {
    pub unsafe fn start_phase(mem: &mut M, state: &mut State, time: &mut BoundedTime) {
        state.partitioned_heap.start_collection(mem, time);
        debug_assert!(state.mark_state.is_none());
        let mark_stack = MarkStack::new(mem);
        state.mark_state = StableOption::Some(MarkState {
            mark_stack,
            complete: false,
        });
    }

    pub unsafe fn complete_phase(state: &mut State) {
        debug_assert!(Self::mark_completed(state));
        state.mark_state = StableOption::None;
    }

    pub unsafe fn mark_completed(state: &State) -> bool {
        let mark_state = state.mark_state.as_ref().unwrap();
        debug_assert!(!mark_state.complete || mark_state.mark_stack.is_empty());
        mark_state.complete
    }

    pub unsafe fn instance(
        mem: &'a mut M,
        state: &'a mut State,
        time: &'a mut BoundedTime,
    ) -> MarkIncrement<'a, M> {
        let heap = &mut state.partitioned_heap;
        let mark_state = state.mark_state.as_mut().unwrap();
        MarkIncrement {
            mem,
            time,
            heap,
            mark_stack: &mut mark_state.mark_stack,
            complete: &mut mark_state.complete,
        }
    }

    pub unsafe fn mark_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.heap.base_address(), self, |gc, field| {
            gc.mark_object(*field);
            gc.time.tick();
        });
    }

    pub unsafe fn run(&mut self) {
        if *self.complete {
            // Allocation after complete marking: Wait until the next GC increment.
            debug_assert!(self.mark_stack.is_empty());
            return;
        }
        loop {
            let value = self.mark_stack.pop();

            #[enhanced_orthogonal_persistence]
            debug_assert!(value.is_non_null_ptr());
            #[classical_persistence]
            debug_assert!(value.is_ptr());

            if value == STACK_EMPTY {
                self.complete_marking();
                return;
            }

            self.mark_fields(value.as_obj());

            self.time.tick();
            if self.time.is_over() {
                return;
            }
        }
    }

    pub unsafe fn mark_object(&mut self, value: Value) {
        self.time.tick();

        #[enhanced_orthogonal_persistence]
        debug_assert_ne!(value, NULL_POINTER);

        debug_assert!((value.get_ptr() >= self.heap.base_address()));
        debug_assert!(!value.is_forwarded());
        let object = value.as_obj();
        if self.heap.mark_object(object) {
            // A write barrier after a completed mark phase must see the object as already marked.
            debug_assert!(!*self.complete);
            debug_assert!(is_object_tag(object.tag()));
            self.mark_stack.push(self.mem, value);
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.heap.base_address(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                let length = slice_array(array);
                if (*array).header.tag >= TAG_ARRAY_SLICE_MIN {
                    gc.mark_stack.push(gc.mem, Value::from_ptr(array as usize));
                }
                gc.time.advance(1 + length - slice_start);
                length
            },
        );
    }

    unsafe fn complete_marking(&mut self) {
        debug_assert!(!*self.complete);
        *self.complete = true;

        #[cfg(debug_assertions)]
        self.mark_stack.assert_unmarked(self.heap);
    }
}
