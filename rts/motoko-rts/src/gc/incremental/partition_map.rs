use core::array::from_fn;

use crate::{
    constants::WORD_SIZE,
    types::{object_size, Bytes, FreeSpace, Obj, TAG_FREE_SPACE, TAG_ONE_WORD_FILLER},
};

pub const PARTITION_SIZE: usize = 128 * 1024 * 1024;
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub struct PartitionId {
    index: usize,
}

impl PartitionId {
    pub fn from_index(index: usize) -> PartitionId {
        assert!(index < MAX_PARTITIONS);
        PartitionId { index }
    }

    pub fn from_address(address: usize) -> PartitionId {
        Self::from_index(address / PARTITION_SIZE)
    }

    pub fn get_index(self) -> usize {
        self.index
    }

    pub fn first() -> PartitionId {
        PartitionId::from_index(0)
    }

    pub fn next(&self) -> Option<PartitionId> {
        let next_index = self.index + 1;
        if next_index < MAX_PARTITIONS {
            Some(PartitionId::from_index(next_index))
        } else {
            None
        }
    }
}

struct Partition {
    id: PartitionId,
    free: bool,
    marked_space: usize,
    static_space: usize,
}

impl Partition {
    fn start_address(&self) -> usize {
        self.id.get_index() * PARTITION_SIZE
    }

    fn end_address(&self) -> usize {
        self.start_address() + PARTITION_SIZE
    }

    fn covers_address(&self, address: usize) -> bool {
        address >= self.start_address() && address <= self.end_address()
    }

    unsafe fn close(&self, heap_pointer: usize) {
        assert!(self.covers_address(heap_pointer));
        let remaining_space = self.end_address() - heap_pointer;
        assert!(remaining_space % WORD_SIZE as usize == 0);
        let block = heap_pointer as *mut Obj;
        if remaining_space == WORD_SIZE as usize {
            block.initialize_tag(TAG_ONE_WORD_FILLER);
        } else {
            block.initialize_tag(TAG_FREE_SPACE);
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes(remaining_space as u32).to_words();
        }
    }

    fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_space;
        assert!(self.marked_space <= dynamic_heap_space);
        self.marked_space as f64 / dynamic_heap_space as f64
    }
}

pub struct PartitionMap {
    partitions: [Partition; MAX_PARTITIONS],
    allocation_target: PartitionId,
}

impl PartitionMap {
    pub fn new(heap_base: usize) -> PartitionMap {
        let allocation_target = PartitionId::from_address(heap_base);
        let partitions = from_fn(|index| {
            let id = PartitionId::from_index(index);
            Partition {
                id,
                free: id > allocation_target,
                marked_space: 0,
                static_space: if id < allocation_target {
                    PARTITION_SIZE
                } else if id == allocation_target {
                    heap_base % PARTITION_SIZE
                } else {
                    0
                },
            }
        });
        PartitionMap {
            partitions,
            allocation_target,
        }
    }

    fn allocate_free_partition(&mut self) -> &Partition {
        for partition in &mut self.partitions {
            if partition.free {
                partition.free = false;
                return partition;
            }
        }
        panic!("Out of memory, no free partition available");
    }

    pub fn evacuation_start_address(&self, id: PartitionId) -> usize {
        assert!(id != self.allocation_target);
        let partition = self.get_partition(id);
        partition.start_address() + partition.static_space
    }

    pub fn partition_end_address(&self, id: PartitionId) -> usize {
        assert!(id != self.allocation_target);
        let partition = self.get_partition(id);
        partition.end_address()
    }

    pub fn should_evacuate(&self, id: PartitionId) -> bool {
        const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;
        let partition = self.get_partition(id);
        id != self.allocation_target
            && !partition.free
            && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD
    }

    fn get_partition(&self, id: PartitionId) -> &Partition {
        let index = id.get_index();
        &self.partitions[index]
    }

    fn mutable_partition(&mut self, id: PartitionId) -> &mut Partition {
        let index = id.get_index();
        &mut self.partitions[index]
    }

    fn allocation_partition(&self) -> &Partition {
        self.get_partition(self.allocation_target)
    }

    pub fn occupied_size(&self, heap_pointer: usize) -> Bytes<u32> {
        let occupied_partitions = self
            .partitions
            .iter()
            .filter(|partition| !partition.free)
            .count();
        let allocation_partition = self.allocation_partition();
        assert!(allocation_partition.covers_address(heap_pointer));
        assert!(!allocation_partition.free);
        let occupied_size = if allocation_partition.start_address() == heap_pointer {
            (occupied_partitions - 1) * PARTITION_SIZE
        } else {
            occupied_partitions * PARTITION_SIZE - heap_pointer % PARTITION_SIZE
        };
        Bytes(occupied_size as u32)
    }

    pub unsafe fn record_marked_space(&mut self, object: *mut Obj) {
        let address = object as usize;
        let size = object_size(address);
        let id = PartitionId::from_address(address);
        let partition = self.mutable_partition(id);
        partition.marked_space += size.to_bytes().as_usize();
    }

    pub unsafe fn prepare_allocation_partition(
        &mut self,
        heap_pointer: &mut u32,
        allocation_size: Bytes<u32>,
    ) {
        if allocation_size.as_usize() > PARTITION_SIZE {
            panic!("Allocation exceeds partition size");
        }
        let partition = self.allocation_partition();
        assert!(!partition.free);
        assert!(partition.covers_address(*heap_pointer as usize));
        assert!(allocation_size.as_usize() < partition.end_address());
        if *heap_pointer as usize >= partition.end_address() - allocation_size.as_usize() {
            self.open_new_allocation_partition(heap_pointer);
        }
    }

    unsafe fn open_new_allocation_partition(&mut self, heap_pointer: &mut u32) {
        let old_partition = self.allocation_partition();
        old_partition.close(*heap_pointer as usize);
        let new_partition = self.allocate_free_partition();
        *heap_pointer = new_partition.start_address() as u32;
        self.allocation_target = new_partition.id;
    }
}
