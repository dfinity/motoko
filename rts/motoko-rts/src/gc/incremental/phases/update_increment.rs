use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        partitioned_heap::{HeapIteratorState, PartitionedHeap, PartitionedHeapIterator},
        roots::visit_roots,
        time::BoundedTime,
        Roots,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    time: &'a mut BoundedTime,
    heap_base: usize,
    heap_iterator: PartitionedHeapIterator<'a>,
    updates_needed: bool,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn instance(
        time: &'a mut BoundedTime,
        state: &'a mut HeapIteratorState,
        heap: &'a PartitionedHeap,
    ) -> UpdateIncrement<'a> {
        let updates_needed = heap.updates_needed();
        UpdateIncrement {
            time,
            heap_base: heap.base_address(),
            heap_iterator: PartitionedHeapIterator::resume(heap, state),
            updates_needed,
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.heap_base, self, |gc, field| {
            let value = *field;
            if value.is_forwarded() {
                *field = value.forward_if_possible();
            }
            gc.time.tick();
        });
    }

    pub unsafe fn run(&mut self) {
        while self.heap_iterator.current_partition().is_some() {
            let partition = self.heap_iterator.current_partition().unwrap();
            if !partition.to_be_evacuated() {
                debug_assert!(!partition.is_free());
                self.update_partition(partition.get_index());
                if self.time.is_over() {
                    return;
                }
            } else {
                self.heap_iterator.next_partition();
            }
        }
    }

    unsafe fn update_partition(&mut self, partition_index: usize) {
        while self.heap_iterator.is_inside_partition(partition_index) && !self.time.is_over() {
            let object = self.heap_iterator.current_object().unwrap();
            if object.is_marked() {
                debug_assert!(!object.is_forwarded());
                if self.updates_needed {
                    self.update_fields(object);
                }
                self.time.tick();
                if self.time.is_over() {
                    // Keep mark bit and later resume updating more slices of this array
                    return;
                }
                object.unmark();
                debug_assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
            }
            self.heap_iterator.next_object();
            self.time.tick();
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
                |gc, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                    gc.time.tick();
                },
                |gc, slice_start, array| {
                    debug_assert!(array.is_marked());
                    let length = slice_array(array);
                    gc.time.advance((length - slice_start) as usize);
                    length
                },
            );
            if object.tag() < TAG_ARRAY_SLICE_MIN || self.time.is_over() {
                return;
            }
        }
    }
}
