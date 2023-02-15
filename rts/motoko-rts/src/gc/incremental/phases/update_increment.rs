use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        partitioned_heap::{
            HeapIteratorState, Partition, PartitionIterator, PartitionedHeap,
            PartitionedHeapIterator,
        },
        roots::visit_roots,
        time::BoundedTime,
        Roots, State,
    },
    types::*,
    visitor::visit_pointer_fields,
};

pub struct UpdateIncrement<'a> {
    time: &'a mut BoundedTime,
    heap: &'a PartitionedHeap,
    iterator_state: &'a mut HeapIteratorState,
    updates_needed: bool,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn start_phase(state: &mut State) {
        debug_assert!(state.iterator_state.is_none());
        state.iterator_state = Some(HeapIteratorState::new());
        let heap = state.partitioned_heap.as_mut().unwrap();
        heap.collect_large_objects();
        heap.plan_updates();
    }

    pub unsafe fn complete_phase(state: &mut State) {
        debug_assert!(Self::update_completed(state));
        state.iterator_state = None;
        state
            .partitioned_heap
            .as_mut()
            .unwrap()
            .complete_collection();
    }

    pub unsafe fn update_completed(state: &State) -> bool {
        state.iterator_state.as_ref().unwrap().completed()
    }

    pub unsafe fn instance(state: &'a mut State, time: &'a mut BoundedTime) -> UpdateIncrement<'a> {
        let heap = state.partitioned_heap.as_ref().unwrap();
        let iterator_state = state.iterator_state.as_mut().unwrap();
        let updates_needed = heap.updates_needed();
        UpdateIncrement {
            time,
            heap,
            iterator_state,
            updates_needed,
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
        let mut iterator = PartitionedHeapIterator::load_from(&self.heap, &self.iterator_state);
        while iterator.has_partition() {
            let partition = iterator.current_partition(&self.heap);
            if partition.to_be_updated() {
                self.update_partition(partition);
                if self.time.is_over() {
                    // Resume updating the same partition later.
                    break;
                }
            }
            iterator.next_partition(&self.heap);
        }
        iterator.save_to(&mut self.iterator_state);
    }

    pub unsafe fn update_partition(&mut self, partition: &Partition) {
        debug_assert!(!partition.is_free());
        debug_assert!(!partition.to_be_evacuated());
        let mut iterator = PartitionIterator::load_from(partition, &mut self.iterator_state);
        while iterator.has_object() {
            let object = iterator.current_object();
            self.update_object(object);
            if self.time.is_over() {
                // Resume updating the same object later.
                break;
            }
            iterator.next_object();
        }
        iterator.save_to(&mut self.iterator_state);
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
        debug_assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
        self.time.tick();
    }

    unsafe fn update_fields(&mut self, object: *mut Obj) {
        loop {
            // Loop over potentially multiple array slices and return if the GC increment limit is exceeded.
            visit_pointer_fields(
                self,
                object,
                object.tag(),
                self.heap.base_address(),
                |gc, field_address| {
                    *field_address = (*field_address).forward_if_possible();
                    gc.time.tick();
                },
                |gc, slice_start, array| {
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
