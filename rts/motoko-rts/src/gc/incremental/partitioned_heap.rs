//! Partitioned heap used in incremental GC for compacting evacuation.
//! The heap is divided in equal sized partitions of a large size `PARTITION_SIZE`.
//! The first partition(s) may contains a static heap space with static objects that are never moved.
//! Beyond the static objects of a partition, the dyanmic heap space starts with `dynamic_size`.
//!
//! Heap layout, with N = `MAX_PARTITIONS`:
//! ┌───────────────┬───────────────┬───────────────┬───────────────┐
//! │  partition 0  │  partition 1  |      ...      | partition N-1 |
//! └───────────────┴───────────────┴───────────────┴───────────────┘
//!
//! Partition layout:
//! ┌───────────────┬───────────────┬───────────────┐
//! │ static_space  │ dynamic_space |  free_space   |
//! └───────────────┴───────────────┴───────────────┘
//!
//! The heap defines an allocation partition that is the target for subsequent object allocations
//! by using efficient bump allocation inside the allocation partition.
//! Whenever a partition is full or has insufficient space to accomodate a new allocation,
//! a new empty partition is selected for allocation.
//!
//! On garbage collection, the high-garbage partitions are selected for evacuation, such that
//! their live objects are moved out to other remaining partitions (through allocation).
//! Therefore, allocation partitions must not be evacuated.
//!
//! Open aspect:
//! * Huge objects: Large allocations > `PARTITION_SIZE` are not yet supported.

use core::array::from_fn;

use crate::{memory::Memory, types::*};

pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;
// For simplicity, leave the last partition unused, to avoid partition end address overflow
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;

/// Heap partition of size `PARTITION_SIZE`.
pub struct Partition {
    index: usize,
    free: bool,
    marked_space: usize,
    static_size: usize,
    dynamic_size: usize,
    evacuate: bool,
}

impl Partition {
    pub fn get_index(&self) -> usize {
        self.index
    }

    fn start_address(&self) -> usize {
        self.index * PARTITION_SIZE
    }

    fn end_address(&self) -> usize {
        self.start_address() + PARTITION_SIZE
    }

    pub fn dynamic_space_start(&self) -> usize {
        self.start_address() + self.static_size
    }

    pub fn dynamic_space_end(&self) -> usize {
        self.dynamic_space_start() + self.dynamic_size
    }

    pub fn free_space(&self) -> usize {
        self.end_address() - self.dynamic_space_end()
    }

    pub fn is_free(&self) -> bool {
        self.free
    }

    pub fn to_be_evacuated(&self) -> bool {
        self.evacuate
    }

    #[cfg(debug_assertions)]
    unsafe fn clear_free_remainder(&self) {
        use crate::constants::WORD_SIZE;
        debug_assert!(self.dynamic_space_end() <= self.end_address());
        let remaining_space = self.end_address() - self.dynamic_space_end();
        debug_assert!(remaining_space % WORD_SIZE as usize == 0);
        if remaining_space == 0 {
            return;
        }
        let block = self.dynamic_space_end() as *mut Tag;
        if remaining_space == WORD_SIZE as usize {
            *block = TAG_ONE_WORD_FILLER;
        } else {
            *block = TAG_FREE_SPACE;
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes(remaining_space as u32).to_words(); // includes header
                                                                            // Clear the remainder of the free space.
            let header_size = size_of::<FreeSpace>().to_bytes().as_usize();
            let clear_start = free_space as usize + header_size;
            let clear_length = Bytes((remaining_space - header_size) as u32);
            crate::mem_utils::memzero(clear_start, clear_length.to_words());
        }
    }

    pub unsafe fn free(&mut self) {
        debug_assert!(!self.free);
        debug_assert!(self.evacuate);
        debug_assert_eq!(self.marked_space, 0);
        self.free = true;
        self.dynamic_size = 0;
        self.evacuate = false;

        #[cfg(debug_assertions)]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        debug_assert!(self.marked_space <= dynamic_heap_space);
        self.marked_space as f64 / dynamic_heap_space as f64
    }
}

/// Iterator state that can be stored between GC increments.
pub struct HeapIteratorState {
    partition_index: usize,
    current_address: usize,
}

impl HeapIteratorState {
    pub fn new() -> HeapIteratorState {
        HeapIteratorState {
            partition_index: 0,
            current_address: 0,
        }
    }

    pub fn completed(&self) -> bool {
        debug_assert!(self.partition_index <= MAX_PARTITIONS);
        self.partition_index == MAX_PARTITIONS
    }
}

/// Instantiated per GC increment, operating on a stored `HeapIteratorState`.
pub struct PartitionedHeapIterator<'a> {
    heap: &'a PartitionedHeap,
    partition_index: &'a mut usize,
    current_address: &'a mut usize,
}

// Different to the standard iterator to allow resuming between GC increment pauses.
impl<'a> PartitionedHeapIterator<'a> {
    pub unsafe fn resume(
        heap: &'a PartitionedHeap,
        state: &'a mut HeapIteratorState,
    ) -> PartitionedHeapIterator<'a> {
        let mut iterator = PartitionedHeapIterator {
            heap,
            partition_index: &mut state.partition_index,
            current_address: &mut state.current_address,
        };
        iterator.start();
        iterator
    }

    unsafe fn start(&mut self) {
        if *self.partition_index == 0 && *self.current_address == 0 {
            *self.partition_index = self.heap.base_address() / PARTITION_SIZE;
            *self.current_address = self.heap.base_address();
        }
        if *self.partition_index < MAX_PARTITIONS {
            self.skip_free_space();
        }
    }

    pub fn current_partition(&self) -> Option<&Partition> {
        if *self.partition_index < MAX_PARTITIONS {
            Some(self.heap.get_partition(*self.partition_index))
        } else {
            debug_assert_eq!(*self.current_address, usize::MAX);
            None
        }
    }

    pub fn current_object(&self) -> Option<*mut Obj> {
        if *self.current_address < usize::MAX {
            Some(*self.current_address as *mut Obj)
        } else {
            debug_assert_eq!(*self.partition_index, MAX_PARTITIONS);
            None
        }
    }

    pub fn is_inside_partition(&self, partition_index: usize) -> bool {
        self.current_partition().is_some()
            && self.current_partition().unwrap().get_index() == partition_index
    }

    fn partition_scan_start(&self) -> usize {
        self.current_partition().unwrap().dynamic_space_start()
    }

    fn partition_scan_end(&self) -> usize {
        self.current_partition().unwrap().dynamic_space_end()
    }

    unsafe fn skip_free_space(&mut self) {
        self.skip_free_blocks();
        // Implicitly also skips free partitions as the dynamic size is zero.
        while *self.current_address == self.partition_scan_end() {
            self.start_next_partition();
            if *self.partition_index == MAX_PARTITIONS {
                return;
            }
            self.skip_free_blocks();
        }
    }

    unsafe fn skip_free_blocks(&mut self) {
        while *self.current_address < self.partition_scan_end()
            && !Value::from_ptr(*self.current_address).is_obj()
        {
            let size = block_size(*self.current_address);
            *self.current_address += size.to_bytes().as_usize();
        }
    }

    fn start_next_partition(&mut self) {
        debug_assert!(*self.partition_index < MAX_PARTITIONS);
        *self.partition_index += 1;
        if *self.partition_index < MAX_PARTITIONS {
            *self.current_address = self.partition_scan_start();
        } else {
            *self.current_address = usize::MAX
        }
    }

    pub unsafe fn next_partition(&mut self) {
        self.start_next_partition();
        self.skip_free_space();
    }

    pub unsafe fn next_object(&mut self) {
        debug_assert!(*self.current_address >= self.partition_scan_start());
        debug_assert!(*self.current_address < self.partition_scan_end());
        let size = block_size(*self.current_address);
        *self.current_address += size.to_bytes().as_usize();
        debug_assert!(*self.current_address <= self.partition_scan_end());
        self.skip_free_space();
    }
}

/// Partitioned heap used with the incremental GC.
pub struct PartitionedHeap {
    partitions: [Partition; MAX_PARTITIONS],
    heap_base: usize,
    allocation_index: usize, // Index of the partition currently used for allocations.
    evacuating: bool,
}

impl PartitionedHeap {
    pub unsafe fn new<M: Memory>(mem: &mut M, heap_base: usize) -> PartitionedHeap {
        let allocation_index = heap_base / PARTITION_SIZE;
        mem.grow_memory(((allocation_index + 1) * PARTITION_SIZE) as u64);
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            marked_space: 0,
            static_size: if index < allocation_index {
                PARTITION_SIZE
            } else if index == allocation_index {
                heap_base % PARTITION_SIZE
            } else {
                0
            },
            dynamic_size: 0,
            evacuate: false,
        });
        PartitionedHeap {
            partitions,
            heap_base,
            allocation_index,
            evacuating: false,
        }
    }

    pub fn base_address(&self) -> usize {
        self.heap_base
    }

    pub fn get_partition(&self, index: usize) -> &Partition {
        &self.partitions[index]
    }

    pub fn plan_evacuations(&mut self) {
        for partition in &mut self.partitions {
            debug_assert!(!partition.evacuate);
            partition.evacuate = self.allocation_index != partition.index
                && !partition.is_free()
                && partition.dynamic_space_start() < partition.end_address()
                && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD;
            self.evacuating |= partition.evacuate;
        }
    }

    pub unsafe fn free_evacuated_partitions(&mut self) {
        for partition in &mut self.partitions {
            partition.marked_space = 0;
            if partition.to_be_evacuated() {
                debug_assert!(partition.index != self.allocation_index);
                partition.free();
            }
        }
        self.evacuating = false;
    }

    pub fn updates_needed(&self) -> bool {
        self.evacuating
    }

    fn allocation_partition(&mut self) -> &mut Partition {
        &mut self.partitions[self.allocation_index]
    }

    pub fn is_allocation_partition(&self, index: usize) -> bool {
        self.allocation_index == index
    }

    fn allocate_free_partition(&mut self, allocation_size: Bytes<u32>) -> &Partition {
        for partition in &mut self.partitions {
            if partition.free && partition.free_space() >= allocation_size.as_usize() {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                return partition;
            }
        }
        panic!("Out of memory");
    }

    pub fn occupied_size(&self) -> Bytes<u32> {
        let occupied_size: usize = self
            .partitions
            .iter()
            .map(|partition| {
                if partition.free {
                    partition.static_size
                } else if partition.index == self.allocation_index {
                    partition.static_size + partition.dynamic_size
                } else {
                    PARTITION_SIZE
                }
            })
            .sum();
        Bytes(occupied_size as u32)
    }

    pub unsafe fn record_marked_space(&mut self, object: *mut Obj) {
        let address = object as usize;
        let size = block_size(address);
        let partition = &mut self.partitions[address / PARTITION_SIZE];
        debug_assert!(address >= partition.dynamic_space_start());
        debug_assert!(address + size.to_bytes().as_usize() <= partition.dynamic_space_end());
        partition.marked_space += size.to_bytes().as_usize();
    }

    pub unsafe fn allocate<M: Memory>(
        &mut self,
        mem: &mut M,
        allocation_size: Bytes<u32>,
    ) -> Value {
        if allocation_size.as_usize() > PARTITION_SIZE {
            panic!("Allocation exceeds partition size");
        }
        let mut allocation_partition = self.allocation_partition();
        debug_assert!(!allocation_partition.free);
        let mut heap_pointer = allocation_partition.dynamic_space_end();
        debug_assert!(allocation_size.as_usize() <= allocation_partition.end_address());
        if heap_pointer > allocation_partition.end_address() - allocation_size.as_usize() {
            self.open_new_allocation_partition(mem, allocation_size);
            allocation_partition = self.allocation_partition();
            heap_pointer = allocation_partition.dynamic_space_end();
        }
        (*allocation_partition).dynamic_size += allocation_size.as_usize();
        Value::from_ptr(heap_pointer)
    }

    unsafe fn open_new_allocation_partition<M: Memory>(
        &mut self,
        mem: &mut M,
        allocation_size: Bytes<u32>,
    ) {
        #[cfg(debug_assertions)]
        self.allocation_partition().clear_free_remainder();

        let new_partition = self.allocate_free_partition(allocation_size);
        let end_address = new_partition.end_address();
        self.allocation_index = new_partition.index;

        mem.grow_memory(end_address as u64);
    }
}
