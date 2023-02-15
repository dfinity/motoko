use crate::{
    gc::incremental::{
        partitioned_heap::{
            HeapIteratorState, PartitionIterator, PartitionedHeap, PartitionedHeapIterator,
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
    heap: &'a mut PartitionedHeap,
    iterator_state: &'a mut HeapIteratorState,
    time: &'a mut BoundedTime,
}

impl<'a, M: Memory + 'a> EvacuationIncrement<'a, M> {
    pub unsafe fn start_phase(state: &mut State) {
        debug_assert!(state.iterator_state.is_none());
        state.iterator_state = Some(HeapIteratorState::new());
        state.partitioned_heap.as_mut().unwrap().plan_evacuations();
    }

    pub unsafe fn complete_phase(state: &mut State) {
        debug_assert!(Self::evacuation_completed(state));
        state.iterator_state = None;
    }

    pub unsafe fn evacuation_completed(state: &State) -> bool {
        state.iterator_state.as_ref().unwrap().completed()
    }

    pub unsafe fn instance(
        mem: &'a mut M,
        state: &'a mut State,
        time: &'a mut BoundedTime,
    ) -> EvacuationIncrement<'a, M> {
        let heap = state.partitioned_heap.as_mut().unwrap();
        let iterator_state = state.iterator_state.as_mut().unwrap();
        EvacuationIncrement {
            mem,
            heap,
            iterator_state,
            time,
        }
    }

    pub unsafe fn run(&mut self) {
        let mut iterator = PartitionedHeapIterator::load_from(&self.heap, &self.iterator_state);
        while iterator.has_partition() {
            let partition = iterator.current_partition(&self.heap);
            if partition.to_be_evacuated() {
                self.evacuate_partition(partition.get_index());
                if self.time.is_over() {
                    // Resume evacuation of the same partition later.
                    break;
                }
            }
            iterator.next_partition(&self.heap);
        }
        iterator.save_to(&mut self.iterator_state);
    }

    pub unsafe fn evacuate_partition(&mut self, partition_index: usize) {
        let partition = self.heap.get_partition(partition_index);
        debug_assert!(!partition.is_free());
        debug_assert!(!partition.has_large_content());
        let mut iterator = PartitionIterator::load_from(partition, &mut self.iterator_state);
        while iterator.has_object() && !self.time.is_over() {
            let object = iterator.current_object();
            // Advance the iterator before evacuation since the debug mode clears the evacuating object.
            iterator.next_object();
            self.evacuate_object(object);
        }
        iterator.save_to(&mut self.iterator_state);
    }

    unsafe fn evacuate_object(&mut self, original: *mut Obj) {
        debug_assert!(original.tag() >= TAG_OBJECT && original.tag() <= TAG_NULL);
        debug_assert!(!original.is_forwarded());
        let size = block_size(original as usize);
        let new_address = self.mem.alloc_words(size);
        let copy = new_address.get_ptr() as *mut Obj;
        memcpy_words(copy as usize, original as usize, size);
        (*copy).forward = new_address;
        (*original).forward = new_address;
        debug_assert!(!copy.is_forwarded());
        debug_assert!(original.is_forwarded());
        // The mark bit is necessary to ensure field updates in the copy.
        let unmarked_before = self.heap.mark_object(copy);
        debug_assert!(unmarked_before);

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
