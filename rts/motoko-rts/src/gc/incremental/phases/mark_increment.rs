use crate::{
    gc::incremental::{
        mark_stack::MarkStack, partitioned_heap::PartitionedHeap, Phase, Roots, INCREMENT_LIMIT,
        PARTITIONED_HEAP, PHASE,
    },
    memory::Memory,
    types::*,
    visitor::visit_pointer_fields,
};

pub struct MarkIncrement<'a, M: Memory> {
    mem: &'a mut M,
    steps: &'a mut usize,
    heap: &'a mut PartitionedHeap,
    mark_stack: &'a mut MarkStack,
    complete: &'a mut bool,
}

impl<'a, M: Memory + 'a> MarkIncrement<'a, M> {
    pub unsafe fn instance(mem: &'a mut M, steps: &'a mut usize) -> MarkIncrement<'a, M> {
        if let Phase::Mark(state) = &mut PHASE {
            MarkIncrement {
                mem,
                steps,
                heap: PARTITIONED_HEAP.as_mut().unwrap(),
                mark_stack: &mut state.mark_stack,
                complete: &mut state.complete,
            }
        } else {
            panic!("Invalid phase");
        }
    }

    pub unsafe fn mark_roots(&mut self, roots: Roots) {
        self.mark_static_roots(roots.static_roots);
        self.mark_continuation_table(roots.continuation_table);
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            debug_assert!((mutbox as usize) < self.heap.base_address());
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.heap.base_address() {
                self.mark_object(value);
            }
            *self.steps += 1;
        }
    }

    unsafe fn mark_continuation_table(&mut self, continuation_table: Value) {
        if continuation_table.is_ptr() {
            self.mark_object(continuation_table);
        }
    }

    pub unsafe fn run(&mut self) {
        if *self.complete {
            // Allocation after complete marking: Wait until the next empty call stack increment.
            debug_assert!(self.mark_stack.is_empty());
            return;
        }
        while let Some(value) = self.mark_stack.pop() {
            debug_assert!(value.is_ptr());
            debug_assert!(value.as_obj().is_marked());
            self.mark_fields(value.as_obj());

            *self.steps += 1;
            if *self.steps > INCREMENT_LIMIT {
                return;
            }
        }
        self.complete_marking();
    }

    pub unsafe fn mark_object(&mut self, value: Value) {
        *self.steps += 1;
        debug_assert!(!*self.complete);
        debug_assert!((value.get_ptr() >= self.heap.base_address()));
        assert!(!value.is_forwarded());
        let object = value.as_obj();
        if object.is_marked() {
            return;
        }
        object.mark();
        self.heap.record_marked_space(object);
        debug_assert!(
            object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL
        );
        self.mark_stack.push(self.mem, value);
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        debug_assert!(object.is_marked());
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
                debug_assert!(array.is_marked());
                const SLICE_INCREMENT: u32 = 128;
                debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    (*array).header.raw_tag = mark(new_start);
                    debug_assert!(!*gc.complete);
                    gc.mark_stack.push(gc.mem, Value::from_ptr(array as usize));
                    *gc.steps += SLICE_INCREMENT as usize;
                    new_start
                } else {
                    (*array).header.raw_tag = mark(TAG_ARRAY);
                    *gc.steps += (array.len() % SLICE_INCREMENT) as usize;
                    array.len()
                }
            },
        );
    }

    unsafe fn complete_marking(&mut self) {
        debug_assert!(!*self.complete);
        *self.complete = true;

        #[cfg(debug_assertions)]
        #[cfg(feature = "ic")]
        crate::gc::incremental::sanity_checks::check_mark_completeness(self.mem);

        #[cfg(debug_assertions)]
        self.mark_stack.assert_is_garbage();
    }
}
