use crate::{
    gc::incremental::{
        partition_map::PartitionMapIterator, Phase, INCREMENT_LIMIT, PARTITION_MAP, PHASE,
    },
    mem_utils::memcpy_words,
    memory::Memory,
    types::*,
};

pub struct EvacuationIncrement<'a, M: Memory> {
    mem: &'a mut M,
    steps: &'a mut usize,
    partition_map_iterator: PartitionMapIterator<'a>,
    sweep_address: &'a mut Option<usize>,
}

impl<'a, M: Memory + 'a> EvacuationIncrement<'a, M> {
    pub unsafe fn instance(mem: &'a mut M, steps: &'a mut usize) -> EvacuationIncrement<'a, M> {
        if let Phase::Evacuate(state) = &mut PHASE {
            let partition_map_iterator = PARTITION_MAP.as_mut().unwrap().iterate();
            EvacuationIncrement {
                mem,
                steps,
                partition_map_iterator,
                sweep_address: &mut state.sweep_address,
            }
        } else {
            panic!("Invalid phase");
        }
    }

    pub unsafe fn initiate_evacuations(&mut self) {
        PARTITION_MAP.as_mut().unwrap().plan_evacuations();
    }

    pub unsafe fn run(&mut self) {
        while self.partition_map_iterator.current().is_some() {
            let partition = self.partition_map_iterator.current().unwrap();
            if partition.to_be_evacuated() {
                if self.sweep_address.is_none() {
                    *self.sweep_address = Some(partition.dynamic_space_start());
                }
                self.evacuate_partition();
                if *self.steps > INCREMENT_LIMIT {
                    return;
                }
            } else {
                assert!(self.sweep_address.is_none());
            }
            self.partition_map_iterator.next();
            *self.sweep_address = None;
        }
    }

    unsafe fn evacuate_partition(&mut self) {
        let end_address = self.partition_map_iterator.current().unwrap().dynamic_space_end();
        while self.sweep_address.unwrap() < end_address {
            let block = Value::from_ptr(self.sweep_address.unwrap());
            if block.is_obj() {
                let original = self.sweep_address.unwrap() as *mut Obj;
                if original.is_marked() {
                    self.evacuate_object(original);
                }
            }
            let size = block_size(self.sweep_address.unwrap());
            *self.sweep_address.as_mut().unwrap() += size.to_bytes().as_usize();
            assert!(self.sweep_address.unwrap() <= end_address);
            *self.steps += 1;
            if *self.steps > INCREMENT_LIMIT {
                return;
            }
        }
    }

    unsafe fn evacuate_object(&mut self, original: *mut Obj) {
        assert!(
            (original as usize)
                >= self
                    .partition_map_iterator
                    .current()
                    .unwrap()
                    .dynamic_space_start()
        );
        assert!((original as usize) < self.partition_map_iterator.current().unwrap().dynamic_space_end());
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
