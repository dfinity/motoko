use crate::{
    gc::incremental::{
        partitioned_heap::{HeapIteratorState, PartitionedHeap, PartitionedHeapIterator},
        INCREMENT_LIMIT,
    },
    mem_utils::memcpy_words,
    memory::Memory,
    types::*,
};

pub struct EvacuationIncrement<'a, M: Memory> {
    mem: &'a mut M,
    steps: &'a mut usize,
    heap_iterator: PartitionedHeapIterator<'a>,
}

impl<'a, M: Memory + 'a> EvacuationIncrement<'a, M> {
    pub unsafe fn instance(
        mem: &'a mut M,
        steps: &'a mut usize,
        state: &'a mut HeapIteratorState,
        heap: &'a PartitionedHeap,
    ) -> EvacuationIncrement<'a, M> {
        EvacuationIncrement {
            mem,
            steps,
            heap_iterator: PartitionedHeapIterator::resume(heap, state),
        }
    }

    pub unsafe fn run(&mut self) {
        while self.heap_iterator.current_partition().is_some() {
            let partition = self.heap_iterator.current_partition().unwrap();
            if partition.to_be_evacuated() {
                self.evacuate_partition(partition.get_index());
                if *self.steps > INCREMENT_LIMIT {
                    return;
                }
            } else {
                self.heap_iterator.next_partition();
            }
        }
    }

    unsafe fn evacuate_partition(&mut self, partition_index: usize) {
        while self
            .heap_iterator
            .current_partition()
            .map(|partition| partition.get_index() == partition_index)
            .unwrap_or(false)
            && *self.steps <= INCREMENT_LIMIT
        {
            let original = self.heap_iterator.current_object().unwrap();
            if original.is_marked() {
                self.evacuate_object(original);
            }
            self.heap_iterator.next_object();
            *self.steps += 1;
        }
    }

    unsafe fn evacuate_object(&mut self, original: *mut Obj) {
        assert!(original.tag() >= TAG_OBJECT && original.tag() <= TAG_NULL);
        assert!(!original.is_forwarded());
        assert!(original.is_marked());
        let size = block_size(original as usize);
        let new_address = self.mem.alloc_words(size);
        let copy = new_address.get_ptr() as *mut Obj;
        memcpy_words(copy as usize, original as usize, size);
        (*copy).forward = new_address;
        (*original).forward = new_address;
        assert!(!copy.is_forwarded());
        assert!(original.is_forwarded());

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
