use core::array::from_fn;

use crate::{constants::WORD_SIZE, types::*};

pub const PARTITION_SIZE: usize = 128 * 1024 * 1024;
pub const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

pub struct Partition {
    index: usize,
    free: bool,
    marked_space: usize,
    static_space: usize,
    evacuate: bool,
}

impl Partition {
    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn start_address(&self) -> usize {
        self.index * PARTITION_SIZE
    }

    pub fn end_address(&self) -> usize {
        self.start_address() + PARTITION_SIZE
    }

    pub fn dynamic_space_start(&self) -> usize {
        self.start_address() + self.static_space
    }

    pub fn is_free(&self) -> bool {
        self.free
    }

    pub fn to_be_evacuated(&self) -> bool {
        self.evacuate
    }

    fn covers_address(&self, address: usize) -> bool {
        address >= self.start_address() && address <= self.end_address()
    }

    unsafe fn close(&self, free_pointer: usize) {
        assert!(self.covers_address(free_pointer));
        assert!(free_pointer >= self.dynamic_space_start());
        let remaining_space = self.end_address() - free_pointer;
        assert!(remaining_space % WORD_SIZE as usize == 0);
        let block = free_pointer as *mut Obj;
        if remaining_space == WORD_SIZE as usize {
            block.initialize_tag(TAG_ONE_WORD_FILLER);
        } else {
            block.initialize_tag(TAG_FREE_SPACE);
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes(remaining_space as u32).to_words() - size_of::<Obj>();

            #[cfg(debug_assertions)]
            Self::clear_free_space(free_space, remaining_space);
        }
    }

    #[cfg(debug_assertions)]
    unsafe fn clear_free_space(free_space: *mut FreeSpace, size: usize) {
        let header_size = size_of::<FreeSpace>().to_bytes().as_usize();
        let start = free_space as usize + header_size;
        let length = size - header_size;
        crate::mem_utils::memzero(start, Words(length as u32));
    }

    pub unsafe fn free(&mut self) {
        assert!(!self.free);
        assert!(self.evacuate);
        assert_eq!(self.marked_space, 0);
        assert!(self.dynamic_space_start() < self.end_address());
        self.close(self.dynamic_space_start());
        self.free = true;
        self.evacuate = false;
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_space;
        assert!(self.marked_space <= dynamic_heap_space);
        self.marked_space as f64 / dynamic_heap_space as f64
    }
}

pub struct PartitionMap {
    partitions: [Partition; MAX_PARTITIONS],
    allocation_index: usize, // index of the partition to allocate in
}

impl PartitionMap {
    pub fn new(heap_base: usize) -> PartitionMap {
        let allocation_index = heap_base / PARTITION_SIZE;
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            marked_space: 0,
            static_space: if index < allocation_index {
                PARTITION_SIZE
            } else if index == allocation_index {
                heap_base % PARTITION_SIZE
            } else {
                0
            },
            evacuate: false,
        });
        PartitionMap {
            partitions,
            allocation_index,
        }
    }

    pub fn plan_evacuations(&mut self) {
        const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;
        for partition in &mut self.partitions {
            assert!(!partition.evacuate);
            partition.evacuate = self.allocation_index != partition.index
                && !partition.is_free()
                && partition.dynamic_space_start() < partition.end_address()
                && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD
        }
    }

    pub unsafe fn free_evacuated_partitions(&mut self) {
        for partition in &mut self.partitions {
            partition.marked_space = 0;
            if partition.to_be_evacuated() {
                assert!(partition.index != self.allocation_index);
                partition.free();
            }
        }
    }

    fn allocation_partition(&self) -> &Partition {
        &self.partitions[self.allocation_index]
    }

    pub fn is_allocation_partition(&self, index: usize) -> bool {
        self.allocation_index == index
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

    pub fn get_partition(&self, index: usize) -> &Partition {
        &self.partitions[index]
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
        let size = block_size(address);
        let partition = &mut self.partitions[address / PARTITION_SIZE];
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
        let allocation_partition = self.allocation_partition();
        assert!(!allocation_partition.free);
        assert!(allocation_partition.covers_address(*heap_pointer as usize));
        assert!(allocation_size.as_usize() < allocation_partition.end_address());
        if *heap_pointer as usize >= allocation_partition.end_address() - allocation_size.as_usize()
        {
            self.open_new_allocation_partition(heap_pointer);
        }
    }

    unsafe fn open_new_allocation_partition(&mut self, heap_pointer: &mut u32) {
        let old_partition = self.allocation_partition();
        old_partition.close(*heap_pointer as usize);
        let new_partition = self.allocate_free_partition();
        *heap_pointer = new_partition.dynamic_space_start() as u32;
        self.allocation_index = new_partition.index;
    }
}
