use crate::{
    gc::incremental::{
        partitioned_heap::PartitionedHeapIterator, Phase, Roots, INCREMENT_LIMIT, PARTITIONED_HEAP,
        PHASE,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    steps: &'a mut usize,
    heap_base: usize,
    heap_iterator: PartitionedHeapIterator<'a>,
    complete: &'a mut bool,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn instance(steps: &'a mut usize) -> UpdateIncrement<'a> {
        if let Phase::Update(state) = &mut PHASE {
            let heap = PARTITIONED_HEAP.as_mut().unwrap();
            UpdateIncrement {
                steps,
                heap_base: heap.base_address(),
                heap_iterator: PartitionedHeapIterator::resume(heap, &mut state.iterator_state),
                complete: &mut state.complete,
            }
        } else {
            panic!("Invalid phase");
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        self.update_static_roots(roots.static_roots);
        self.update_continuation_table(roots.continuation_table);
    }

    unsafe fn update_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for index in 0..root_array.len() {
            let mutbox = root_array.get(index).as_mutbox();
            debug_assert!((mutbox as usize) < self.heap_base);
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.heap_base {
                (*mutbox).field = value.forward_if_possible();
            }
            *self.steps += 1;
        }
    }

    unsafe fn update_continuation_table(&mut self, continuation_table: Value) {
        if continuation_table.is_ptr() {
            visit_pointer_fields(
                self,
                continuation_table.get_ptr() as *mut Obj,
                continuation_table.tag(),
                self.heap_base,
                |_, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                },
                |gc, _, array| {
                    *gc.steps += array.len() as usize;
                    array.len()
                },
            );
        }
    }

    pub unsafe fn run(&mut self) {
        while self.heap_iterator.current_partition().is_some() {
            let partition = self.heap_iterator.current_partition().unwrap();
            if !partition.to_be_evacuated() {
                assert!(!partition.is_free());
                self.update_partition();
                if *self.steps > INCREMENT_LIMIT {
                    return;
                }
            } else {
                self.heap_iterator.next_partition();
            }
        }
        *self.complete = true;
    }

    unsafe fn update_partition(&mut self) {
        while self.heap_iterator.current_object().is_some() && *self.steps <= INCREMENT_LIMIT {
            let object = self.heap_iterator.current_object().unwrap();
            if object.is_marked() {
                self.update_fields(object);
                if *self.steps > INCREMENT_LIMIT {
                    // Keep mark bit and later resume updating more slices of this array
                    return;
                }
            }
            object.unmark();
            self.heap_iterator.next_object();
            *self.steps += 1;
        }
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
        loop {
            // Loop over array slices and return if GC increment is exceeded.
            visit_pointer_fields(
                self,
                object,
                object.tag(),
                self.heap_base,
                |_, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                },
                |gc, slice_start, array| {
                    debug_assert!(array.is_marked());
                    const SLICE_INCREMENT: u32 = 128;
                    debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                    if array.len() - slice_start > SLICE_INCREMENT {
                        let new_start = slice_start + SLICE_INCREMENT;
                        (*array).header.raw_tag = mark(new_start);
                        *gc.steps += SLICE_INCREMENT as usize;
                        new_start
                    } else {
                        (*array).header.raw_tag = mark(TAG_ARRAY);
                        *gc.steps += (array.len() % SLICE_INCREMENT) as usize;
                        array.len()
                    }
                },
            );
            if object.tag() < TAG_ARRAY_SLICE_MIN || *self.steps > INCREMENT_LIMIT {
                return;
            }
        }
    }
}
