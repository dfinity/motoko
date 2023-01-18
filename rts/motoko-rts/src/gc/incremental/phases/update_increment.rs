use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        partitioned_heap::{HeapIteratorState, PartitionedHeap, PartitionedHeapIterator},
        roots::visit_roots,
        Roots,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    steps: &'a mut usize,
    limit: usize,
    heap_base: usize,
    heap_iterator: PartitionedHeapIterator<'a>,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn instance(
        steps: &'a mut usize,
        limit: usize,
        state: &'a mut HeapIteratorState,
        heap: &'a PartitionedHeap,
    ) -> UpdateIncrement<'a> {
        UpdateIncrement {
            steps,
            limit,
            heap_base: heap.base_address(),
            heap_iterator: PartitionedHeapIterator::resume(heap, state),
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.heap_base, self, |gc, field| {
            let value = *field;
            if value.is_forwarded() {
                *field = value.forward_if_possible();
            }
            *gc.steps += 1;
        });
    }

    pub unsafe fn run(&mut self) {
        while self.heap_iterator.current_partition().is_some() {
            let partition = self.heap_iterator.current_partition().unwrap();
            if !partition.to_be_evacuated() {
                assert!(!partition.is_free());
                self.update_partition(partition.get_index());
                if *self.steps > self.limit {
                    return;
                }
            } else {
                self.heap_iterator.next_partition();
            }
        }
    }

    unsafe fn update_partition(&mut self, partition_index: usize) {
        while self.heap_iterator.is_inside_partition(partition_index) && *self.steps <= self.limit {
            let object = self.heap_iterator.current_object().unwrap();
            if object.is_marked() {
                assert!(!object.is_forwarded());
                self.update_fields(object);
                if *self.steps > self.limit {
                    // Keep mark bit and later resume updating more slices of this array
                    return;
                }
                object.unmark();
                assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
            }
            self.heap_iterator.next_object();
            *self.steps += 1;
        }
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        loop {
            // Loop over potentially multiple array slices and return if the GC increment limit is exceeded.
            visit_pointer_fields(
                self,
                object,
                object.tag(),
                self.heap_base,
                |_, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                },
                |gc, _, array| {
                    debug_assert!(array.is_marked());
                    let length = slice_array(array);
                    *gc.steps += length as usize;
                    length
                },
            );
            if object.tag() < TAG_ARRAY_SLICE_MIN || *self.steps > self.limit {
                return;
            }
        }
    }
}
