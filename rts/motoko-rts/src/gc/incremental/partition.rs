use crate::{
    constants::WORD_SIZE,
    types::{object_size, Bytes, FreeSpace, Obj, TAG_FREE_SPACE, TAG_ONE_WORD_FILLER},
};

const PARTITION_SIZE: usize = 128 * 1024 * 1024;
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

#[derive(PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
struct Partition {
    index: usize,
}

impl Partition {
    pub fn from_index(index: usize) -> Partition {
        Partition { index }
    }

    pub fn from_address(address: usize) -> Partition {
        Self::from_index(address / PARTITION_SIZE)
    }

    pub fn get_index(self) -> usize {
        self.index
    }

    pub fn start_address(self) -> usize {
        self.index * PARTITION_SIZE
    }

    pub fn end_address(self) -> usize {
        (self.index + 1) * PARTITION_SIZE
    }

    pub fn covers_address(self, address: usize) -> bool {
        address >= self.start_address() && address <= self.end_address()
    }

    pub unsafe fn finish_allocations(self, heap_pointer: usize) {
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
}

struct PartitionMap {
    is_free: [bool; MAX_PARTITIONS],
    marked_space: [usize; MAX_PARTITIONS],
    allocation_partition: Partition,
}

impl PartitionMap {
    pub fn initialize(&mut self, heap_base: usize) {
        let base_partition = Partition::from_address(heap_base);
        for index in base_partition.get_index() + 1..MAX_PARTITIONS {
            self.is_free[index] = true;
        }
        self.allocation_partition = base_partition;
    }

    pub fn is_free(&self, partition: Partition) -> bool {
        self.is_free[partition.get_index()]
    }

    pub fn allocate_free_partition(&mut self) -> Partition {
        for index in 0..MAX_PARTITIONS {
            if self.is_free[index] {
                self.is_free[index] = false;
                return Partition::from_index(index);
            }
        }
        panic!("Out of memory, no free partition available");
    }

    pub fn occupied_size(&self, heap_pointer: usize) -> usize {
        assert!(self.allocation_partition.covers_address(heap_pointer));
        assert!(!self.is_free(self.allocation_partition));
        let occupied = self.is_free.iter().filter(|free| !**free).count() * PARTITION_SIZE;
        if self.allocation_partition.start_address() == heap_pointer {
            occupied - PARTITION_SIZE
        } else {
            occupied - heap_pointer % PARTITION_SIZE
        }
    }

    pub fn record_marked_space(&mut self, partition: Partition, size: usize) {
        self.marked_space[partition.get_index()] += size;
    }
}

static mut PARTITION_MAP: PartitionMap = PartitionMap {
    is_free: [false; MAX_PARTITIONS],
    marked_space: [0; MAX_PARTITIONS],
    allocation_partition: Partition { index: 0 },
};

pub static mut USE_PARTITIONING: bool = false;

pub unsafe fn initialize_partitions(heap_base: usize) {
    PARTITION_MAP.initialize(heap_base);
    USE_PARTITIONING = true;
}

pub unsafe fn prepare_allocation_partition(heap_pointer: &mut u32, allocation_size: Bytes<u32>) {
    assert!(USE_PARTITIONING);
    if allocation_size.as_usize() > PARTITION_SIZE {
        panic!("Allocation exceeds partition size");
    }
    let partition = PARTITION_MAP.allocation_partition;
    assert!(!PARTITION_MAP.is_free(partition));
    assert!(partition.covers_address(*heap_pointer as usize));
    assert!(allocation_size.as_usize() < partition.end_address());
    if *heap_pointer as usize >= partition.end_address() - allocation_size.as_usize() {
        partition.finish_allocations(*heap_pointer as usize);
        let partition = PARTITION_MAP.allocate_free_partition();
        PARTITION_MAP.allocation_partition = partition;
        *heap_pointer = partition.start_address() as u32;
    }
}

pub unsafe fn record_marked_space(object: *mut Obj) {
    assert!(USE_PARTITIONING);
    let address = object as usize;
    let size = object_size(address);
    PARTITION_MAP.record_marked_space(Partition::from_address(address), size.to_bytes().as_usize());
}

pub unsafe fn occupied_size(heap_pointer: u32) -> Bytes<u32> {
    assert!(USE_PARTITIONING);
    Bytes(PARTITION_MAP.occupied_size(heap_pointer as usize) as u32)
}
