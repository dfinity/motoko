use crate::{
    gc::incremental::{
        partitioned_heap::{HeapIteratorState, PartitionedHeap, PartitionedHeapIterator},
        roots::visit_roots,
        Roots, INCREMENT_LIMIT,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    steps: &'a mut usize,
    heap_base: usize,
    heap_iterator: PartitionedHeapIterator<'a>,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn instance(
        steps: &'a mut usize,
        state: &'a mut HeapIteratorState,
        heap: &'a PartitionedHeap,
    ) -> UpdateIncrement<'a> {
        UpdateIncrement {
            steps,
            heap_base: heap.base_address(),
            heap_iterator: PartitionedHeapIterator::resume(heap, state),
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.heap_base, self, |gc, object| {
            loop {
                // Deal with array slicing on the continuation table.
                gc.update_fields(object.get_ptr() as *mut Obj);
                if object.tag() < TAG_ARRAY_SLICE_MIN {
                    return;
                }
            }
        });
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
