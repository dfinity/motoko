use crate::{
    gc::incremental::{
        array_slicing::slice_array,
        partitioned_heap::{
            HeapIteratorState, Partition, PartitionIterator, PartitionedHeap,
            PartitionedHeapIterator,
        },
        roots::visit_roots,
        time::BoundedTime,
        Roots, PARTITIONED_HEAP,
    },
    types::*,
    visitor::visit_pointer_fields, constants::WORD_SIZE,
};

static mut UPDATE_STATE: Option<HeapIteratorState> = None;

pub struct UpdateIncrement<'a> {
    time: &'a mut BoundedTime,
    heap: &'a PartitionedHeap,
    state: &'a mut HeapIteratorState,
    updates_needed: bool,
}

impl<'a> UpdateIncrement<'a> {
    pub unsafe fn start_phase() {
        debug_assert!(UPDATE_STATE.is_none());
        UPDATE_STATE = Some(HeapIteratorState::new());
        PARTITIONED_HEAP.as_mut().unwrap().collect_large_objects();
    }

    pub unsafe fn complete_phase() {
        debug_assert!(Self::update_completed());
        UPDATE_STATE = None;
        PARTITIONED_HEAP
            .as_mut()
            .unwrap()
            .free_evacuated_partitions();
    }

    pub unsafe fn update_completed() -> bool {
        UPDATE_STATE.as_ref().unwrap().completed()
    }

    pub unsafe fn instance(time: &'a mut BoundedTime) -> UpdateIncrement<'a> {
        let heap = PARTITIONED_HEAP.as_ref().unwrap();
        let state = UPDATE_STATE.as_mut().unwrap();
        let updates_needed = heap.updates_needed();
        UpdateIncrement {
            time,
            heap,
            state,
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
        let mut iterator = PartitionedHeapIterator::load_from(self.heap, &self.state);
        while iterator.current_partition().is_some() {
            let partition = iterator.current_partition().unwrap();
            if !partition.to_be_evacuated() {
                self.update_partition(partition);
                if self.time.is_over() {
                    // Resume updating the same partition later.
                    break;
                }
                self.time.advance(partition.dynamic_size() / WORD_SIZE as usize);
            }
            iterator.next_partition();
        }
        iterator.save_to(&mut self.state);
    }

    pub unsafe fn update_partition(&mut self, partition: &Partition) {
        debug_assert!(!partition.is_free());
        let mut iterator = PartitionIterator::load_from(partition, &self.state);
        while iterator.current_object().is_some() {
            let object = iterator.current_object().unwrap();
            self.update_object(object);
            if self.time.is_over() {
                // Resume updating the same object later.
                break;
            }
            iterator.next_object();
        }
        iterator.save_to(&mut self.state);
    }

    unsafe fn update_object(&mut self, object: *mut Obj) {
        debug_assert!(object.is_marked());
        debug_assert!(!object.is_forwarded());
        if self.updates_needed {
            self.update_fields(object);
        }
        if self.time.is_over() {
            // Keep mark bit and later resume updating more slices of this array.
            return;
        }
        object.unmark();
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
