use crate::{
    gc::incremental::{
        partitioned_heap::{
            HeapIteratorState, Partition, PartitionIterator, PartitionedHeap,
            PartitionedHeapIterator,
        },
        time::BoundedTime,
        PARTITIONED_HEAP,
    },
    mem_utils::memcpy_words,
    memory::Memory,
    types::*, constants::WORD_SIZE,
};

static mut EVACUATION_STATE: Option<HeapIteratorState> = None;

pub struct EvacuationIncrement<'a, M: Memory> {
    mem: &'a mut M,
    heap: &'a PartitionedHeap,
    state: &'a mut HeapIteratorState,
    time: &'a mut BoundedTime,
}

impl<'a, M: Memory + 'a> EvacuationIncrement<'a, M> {
    pub unsafe fn start_phase() {
        debug_assert!(EVACUATION_STATE.is_none());
        EVACUATION_STATE = Some(HeapIteratorState::new());
        PARTITIONED_HEAP.as_mut().unwrap().plan_evacuations();
    }

    pub unsafe fn complete_phase() {
        debug_assert!(Self::evacuation_completed());
        EVACUATION_STATE = None;
    }

    pub unsafe fn evacuation_completed() -> bool {
        EVACUATION_STATE.as_ref().unwrap().completed()
    }

    pub unsafe fn instance(
        mem: &'a mut M,
        time: &'a mut BoundedTime,
    ) -> EvacuationIncrement<'a, M> {
        let heap = PARTITIONED_HEAP.as_ref().unwrap();
        let state = EVACUATION_STATE.as_mut().unwrap();
        EvacuationIncrement {
            mem,
            heap,
            state,
            time,
        }
    }

    pub unsafe fn run(&mut self) {
        let mut iterator = PartitionedHeapIterator::load_from(self.heap, &self.state);
        while iterator.current_partition().is_some() {
            let partition = iterator.current_partition().unwrap();
            if partition.to_be_evacuated() {
                self.evacuate_partition(partition);
                if self.time.is_over() {
                    // Resume evacuation of the same partition later.
                    break;
                }
                self.time.advance(partition.dynamic_size() / WORD_SIZE as usize);
            }
            iterator.next_partition();
        }
        iterator.save_to(&mut self.state);
    }

    pub unsafe fn evacuate_partition(&mut self, partition: &Partition) {
        debug_assert!(!partition.is_free());
        debug_assert!(!partition.has_large_content());
        let mut iterator = PartitionIterator::load_from(partition, &self.state);
        while iterator.current_object().is_some() && !self.time.is_over() {
            let object = iterator.current_object().unwrap();
            // Advance the iterator before evacuation since the debug mode clears the evacuating object.
            iterator.next_object();
            self.evacuate_object(object);
        }
        iterator.save_to(&mut self.state);
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
        debug_assert!(copy.is_marked()); // Necessary to ensure field updates in the copy.
        self.time.advance(size.as_usize());

        #[cfg(debug_assertions)]
        Self::clear_object_content(original);
    }

    #[cfg(debug_assertions)]
    unsafe fn clear_object_content(original: *mut Obj) {
        let object_size = block_size(original as usize);
        let header_size = size_of::<Obj>();
        let payload_address = original as usize + header_size.to_bytes().as_usize();
        let payload_size = object_size - header_size;
        crate::mem_utils::memzero(payload_address, payload_size);
    }
}
