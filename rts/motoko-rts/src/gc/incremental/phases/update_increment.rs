use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        partitioned_heap::{Partition, PartitionedHeap, PartitionedHeapIterator},
        roots::visit_roots,
        time::BoundedTime,
        Roots, State,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateState {
    iterator: PartitionedHeapIterator,
    next_slice: u32,
}

pub struct UpdateIncrement<'a> {
    time: &'a mut BoundedTime,
    heap: &'a PartitionedHeap,
    iterator: &'a mut PartitionedHeapIterator,
    next_slice: &'a mut u32,
    updates_needed: bool,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn start_phase(state: &mut State) {
        debug_assert!(state.update_state.is_none());
        let heap = &mut state.partitioned_heap;
        state.update_state = Some(UpdateState {
            iterator: PartitionedHeapIterator::new(heap),
            next_slice: 0,
        });
        heap.collect_large_objects();
        heap.plan_updates();
    }

    pub unsafe fn complete_phase(state: &mut State) {
        debug_assert!(Self::update_completed(state));
        state.update_state = None;
        state.partitioned_heap.complete_collection();
    }

    pub unsafe fn update_completed(state: &State) -> bool {
        let update_state = state.update_state.as_ref().unwrap();
        !update_state.iterator.has_partition()
    }

    pub unsafe fn instance(state: &'a mut State, time: &'a mut BoundedTime) -> UpdateIncrement<'a> {
        let heap = &state.partitioned_heap;
        let update_state = state.update_state.as_mut().unwrap();
        let iterator = &mut update_state.iterator;
        let updates_needed = heap.updates_needed();
        let next_slice = &mut update_state.next_slice;
        UpdateIncrement {
            time,
            heap,
            iterator,
            updates_needed,
            next_slice,
        }
    }

    pub unsafe fn update_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.heap.base_address(), self, |gc, field| {
            let value = *field;
            if value.is_forwarded() {
                *field = value.forward_if_possible();
            }
            gc.time.tick();
        });
    }

    pub unsafe fn run(&mut self) {
        while self.iterator.has_partition() {
            let partition = self.iterator.current_partition(&self.heap);
            if partition.to_be_updated() {
                self.update_partition(partition);
                if self.time.is_over() {
                    // Resume updating the same partition later.
                    break;
                }
            }
            self.iterator.next_partition(&self.heap);
        }
    }

    pub unsafe fn update_partition(&mut self, partition: &Partition) {
        debug_assert!(!partition.is_free());
        debug_assert!(!partition.to_be_evacuated());
        while self.iterator.has_object() && !self.time.is_over() {
            let object = self.iterator.current_object();
            self.update_object(object);
            if self.time.is_over() && *self.next_slice > 0 {
                // Resume updating the same object later.
                break;
            }
            self.iterator.next_object();
        }
    }

    unsafe fn update_object(&mut self, object: *mut Obj) {
        debug_assert!(!object.is_forwarded());
        if self.updates_needed {
            self.update_fields(object);
        }
        if self.time.is_over() {
            // Keep mark bit and later resume updating more slices of this array.
            return;
        }
        debug_assert_eq!(*self.next_slice, 0);
        self.time.tick();
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        loop {
            // Loop over potentially multiple array slices and return if the GC increment limit is exceeded.
            visit_pointer_fields(
                self,
                object,
                if *self.next_slice > 0 {
                    *self.next_slice
                } else {
                    object.tag()
                },
                self.heap.base_address(),
                |gc, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                    gc.time.tick();
                },
                |gc, slice_start, array| {
                    let slice_end = slice_array(array, slice_start);
                    *gc.next_slice = if slice_end < array.len() {
                        slice_end
                    } else {
                        0
                    };
                    gc.time.advance((slice_end - slice_start) as usize);
                    slice_end
                },
            );
            if *self.next_slice == 0 || self.time.is_over() {
                return;
            }
        }
    }
}
