use crate::{
    gc::incremental::{
        partitioned_heap::{
            HeapIteratorState, Partition, PartitionIterator, PartitionedHeap,
            PartitionedHeapIterator,
        },
        time::BoundedTime,
        State,
    },
    mem_utils::memcpy_words,
    memory::Memory,
    types::*,
};

pub struct EvacuationIncrement<'a, M: Memory> {
    mem: &'a mut M,
    heap: &'a PartitionedHeap,
    iterator_state: &'a mut HeapIteratorState,
    time: &'a mut BoundedTime,
}

impl<'a, M: Memory + 'a> EvacuationIncrement<'a, M> {
    pub unsafe fn start_phase(state: &mut State) {
        state.iterator_state = HeapIteratorState::new();
        state.heap.plan_evacuations();
    }

    pub unsafe fn evacuation_completed(state: &State) -> bool {
        state.iterator_state.completed()
    }

    pub unsafe fn instance(
        mem: &'a mut M,
        state: &'a mut State,
        time: &'a mut BoundedTime,
    ) -> EvacuationIncrement<'a, M> {
        let heap = &state.heap;
        let iterator_state = &mut state.iterator_state;
        EvacuationIncrement {
            mem,
            heap,
            iterator_state,
            time,
        }
    }

    pub unsafe fn run(&mut self) {
        let mut iterator = PartitionedHeapIterator::load_from(self.heap, &self.iterator_state);
        while iterator.current_partition().is_some() {
            let partition = iterator.current_partition().unwrap();
            if partition.to_be_evacuated() {
                self.evacuate_partition(partition);
                if self.time.is_over() {
                    // Resume evacuation of the same partition later.
                    break;
                }
            }
            iterator.next_partition();
        }
        iterator.save_to(&mut self.iterator_state);
    }

    pub unsafe fn evacuate_partition(&mut self, partition: &Partition) {
        debug_assert!(!partition.is_free());
        debug_assert!(!partition.has_large_content());
        if partition.marked_size() == 0 {
            return;
        }
        let mut iterator =
            PartitionIterator::load_from(partition, &self.iterator_state, &mut self.time);
        while iterator.current_object().is_some() && !self.time.is_over() {
            let object = iterator.current_object().unwrap();
            // Advance the iterator before evacuation since the debug mode clears the evacuating object.
            iterator.next_object(&mut self.time);
            self.evacuate_object(object);
        }
        iterator.save_to(&mut self.iterator_state);
    }

    unsafe fn evacuate_object(&mut self, original: *mut Obj) {
        debug_assert!(original.tag() >= TAG_OBJECT && original.tag() <= TAG_NULL);
        debug_assert!(!original.is_forwarded());
        debug_assert!(original.is_marked());
        let size = block_size(original as usize);
        let new_address = self.mem.alloc_words(size);
        let copy = new_address.get_ptr() as *mut Obj;
        memcpy_words(copy as usize, original as usize, size);
        (*copy).forward = new_address;
        (*original).forward = new_address;
        debug_assert!(!copy.is_forwarded());
        debug_assert!(original.is_forwarded());
        // The mark bit is necessary to ensure field updates in the copy.
        debug_assert!(copy.is_marked());
        // However, updating the marked size statistics of the target partition can be skipped,
        // since that partition will not be considered for evacuation during the current GC run.

        // Determined by measurements in comparison to the mark and update phases.
        const TIME_FRACTION_PER_WORD: usize = 3;
        self.time.advance(size.as_usize() / TIME_FRACTION_PER_WORD);

        #[cfg(feature = "memory_check")]
        Self::clear_object_content(original);
    }

    #[cfg(feature = "memory_check")]
    unsafe fn clear_object_content(original: *mut Obj) {
        let object_size = block_size(original as usize);
        let header_size = size_of::<Obj>();
        let payload_address = original as usize + header_size.to_bytes().as_usize();
        let payload_size = object_size - header_size;
        crate::mem_utils::memzero(payload_address, payload_size);
    }
}
