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
//! Large allocations:
//! Huge objects with a size > PARTITION_SIZE are allocated across multiple contiguous free
//! partitions. For this purpose, a corresponding sequence of contiguous free partitions needs
//! to be searched. Huge objects stay in their partitions for their entire lifetime, i.e. they
//! are never evacuated. When becoming garbage, the underlying partitions of a huge blocks are
//! immediately freed. Large object allocation may be prone to external fragmentation problems,
//! i.e. that no sufficient contiguous free partitions are available on allocation. Currently,
//! this external fragmentation problem is not handled by moving other partitions which would
//! require a special blocking full GC collection. Moreover, for simplicity, the remainder
//! of the last partition of a huge object is not used for further small object allocations,
//! which implies limited internal fragmentation.

use core::array::from_fn;

use crate::{memory::Memory, rts_trap_with, types::*};

pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;
// For simplicity, leave the last partition unused, to avoid partition end address overflow
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.35;

/// Heap partition of size `PARTITION_SIZE`.
pub struct Partition {
    index: usize,        // Index of the partition 0..MAX_PARTITIONS.
    free: bool,          // Denotes a free partition (which may still contain static space).
    large_content: bool, // Specifies whether a large object is contained that spans multiple partitions.
    marked_space: usize, // Total amount marked object space in the dynamic space.
    static_size: usize,  // Size of the static space.
    dynamic_size: usize, // Size of the dynamic space.
    evacuate: bool,      // Specifies whether the partition is to be evacuated or being evacuated.
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
        debug_assert_eq!(remaining_space % WORD_SIZE as usize, 0);
        debug_assert!(remaining_space <= PARTITION_SIZE);
        if remaining_space == 0 {
            return;
        }
        let block = self.dynamic_space_end() as *mut Tag;
        if remaining_space == WORD_SIZE as usize {
            *block = TAG_ONE_WORD_FILLER;
        } else {
            *block = TAG_FREE_SPACE;
            let header_size = size_of::<FreeSpace>().to_bytes().as_usize();
            debug_assert!(remaining_space >= header_size);
            let free_space = block as *mut FreeSpace;
            (*free_space).words = Bytes((remaining_space - header_size) as u32).to_words();
            // Clear the remainder of the free space.
            let clear_start = free_space as usize + header_size;
            let clear_length = Bytes((remaining_space - header_size) as u32);
            crate::mem_utils::memzero(clear_start, clear_length.to_words());
            debug_assert_eq!(free_space.size().to_bytes().as_usize(), remaining_space);
        }
    }

    pub unsafe fn free(&mut self) {
        debug_assert!(!self.free);
        debug_assert!(self.evacuate || self.large_content);
        debug_assert_eq!(self.marked_space, 0);
        self.free = true;
        self.dynamic_size = 0;
        self.evacuate = false;
        self.large_content = false;

        #[cfg(debug_assertions)]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        debug_assert!(self.marked_space <= dynamic_heap_space);
        self.marked_space as f64 / dynamic_heap_space as f64
    }

    pub fn has_large_content(&self) -> bool {
        self.large_content
    }

    pub fn is_completely_free(&self) -> bool {
        self.free && self.free_space() == PARTITION_SIZE
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
/// Iterates over all marked objects.
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
            self.skip_unmarked_space();
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

    unsafe fn skip_unmarked_space(&mut self) {
        self.skip_unmarked_blocks();
        // Implicitly also skips free partitions as the dynamic size is zero.
        while *self.current_address == self.partition_scan_end() {
            self.skip_partitions(1);
            if *self.partition_index == MAX_PARTITIONS {
                return;
            }
            self.skip_unmarked_blocks();
        }
    }

    unsafe fn skip_unmarked_blocks(&mut self) {
        self.skip_unmarked_large_objects();
        let end_address = self.partition_scan_end();
        let mut address = *self.current_address;
        while address < end_address && !is_marked(*(address as *mut Tag)) {
            let size = block_size(address).to_bytes().as_usize();
            debug_assert!(size <= PARTITION_SIZE);
            address += size;
            debug_assert!(address <= end_address);
        }
        *self.current_address = address;
    }

    unsafe fn skip_unmarked_large_objects(&mut self) {
        while self.current_partition().unwrap().has_large_content()
            && !is_marked(*(*self.current_address as *mut Tag))
        {
            self.skip_object();
        }
    }

    unsafe fn skip_object(&mut self) {
        let size = block_size(*self.current_address).to_bytes().as_usize();
        if size <= PARTITION_SIZE {
            *self.current_address += size;
            debug_assert!(*self.current_address <= self.partition_scan_end());
        } else {
            let number_of_partitions = (size + PARTITION_SIZE - 1) / PARTITION_SIZE;
            self.skip_partitions(number_of_partitions);
        }
    }

    fn skip_partitions(&mut self, number_of_partitions: usize) {
        debug_assert!(*self.partition_index < MAX_PARTITIONS);
        *self.partition_index += number_of_partitions;
        if *self.partition_index < MAX_PARTITIONS {
            *self.current_address = self.partition_scan_start();
        } else {
            *self.current_address = usize::MAX
        }
    }

    pub unsafe fn next_partition(&mut self) {
        let number_of_partitions = if self.current_partition().unwrap().has_large_content() {
            let size = block_size(*self.current_address).to_bytes().as_usize();
            (size + PARTITION_SIZE - 1) / PARTITION_SIZE
        } else {
            1
        };
        self.skip_partitions(number_of_partitions);
        self.skip_unmarked_space();
    }

    pub unsafe fn next_object(&mut self) {
        debug_assert!(*self.current_address >= self.partition_scan_start());
        debug_assert!(*self.current_address < self.partition_scan_end());
        self.skip_object();
        self.skip_unmarked_space();
    }
}

/// Partitioned heap used with the incremental GC.
pub struct PartitionedHeap {
    partitions: [Partition; MAX_PARTITIONS],
    heap_base: usize,
    allocation_index: usize, // Index of the partition currently used for allocations.
    evacuating: bool,
    reclaimed: u64,
}

impl PartitionedHeap {
    pub unsafe fn new<M: Memory>(mem: &mut M, heap_base: usize) -> PartitionedHeap {
        let allocation_index = heap_base / PARTITION_SIZE;
        mem.grow_memory(((allocation_index + 1) * PARTITION_SIZE) as u64);
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            large_content: false,
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
            reclaimed: 0,
        }
    }

    pub fn base_address(&self) -> usize {
        self.heap_base
    }

    pub fn get_partition(&self, index: usize) -> &Partition {
        &self.partitions[index]
    }

    fn mutable_partition(&mut self, index: usize) -> &mut Partition {
        &mut self.partitions[index]
    }

    pub fn plan_evacuations(&mut self) {
        for partition in &mut self.partitions {
            debug_assert!(!partition.evacuate);
            partition.evacuate = self.allocation_index != partition.index
                && !partition.is_free()
                && !partition.has_large_content()
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
                self.reclaimed += partition.dynamic_size as u64;
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

    unsafe fn allocate_free_partition(&mut self, requested_space: usize) -> &mut Partition {
        for partition in &mut self.partitions {
            if partition.free && partition.free_space() >= requested_space {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                return partition;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    pub fn occupied_size(&self) -> Bytes<u32> {
        let occupied_size: usize = self
            .partitions
            .iter()
            .map(|partition| partition.static_size + partition.dynamic_size)
            .sum();
        Bytes(occupied_size as u32)
    }

    pub fn reclaimed_size(&self) -> Bytes<u64> {
        Bytes(self.reclaimed)
    }

    #[inline]
    pub unsafe fn record_marked_space(&mut self, object: *mut Obj) {
        let address = object as usize;
        let size = block_size(address).to_bytes().as_usize();
        if size <= PARTITION_SIZE {
            let partition = &mut self.partitions[address / PARTITION_SIZE];
            debug_assert!(address >= partition.dynamic_space_start());
            partition.marked_space += size;
            debug_assert!(address + size <= partition.dynamic_space_end());
        } else {
            self.mark_large_object(object);
        }
    }

    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M, words: Words<u32>) -> Value {
        let size = words.to_bytes().as_usize();
        if size <= PARTITION_SIZE {
            self.allocate_normal_object(mem, size)
        } else {
            self.allocate_large_object(mem, size)
        }
    }

    unsafe fn allocate_normal_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        debug_assert!(size <= PARTITION_SIZE);
        let mut allocation_partition = self.allocation_partition();
        debug_assert!(!allocation_partition.free);
        let heap_pointer = allocation_partition.dynamic_space_end();
        debug_assert!(size <= allocation_partition.end_address());
        if heap_pointer <= allocation_partition.end_address() - size {
            (*allocation_partition).dynamic_size += size;
            Value::from_ptr(heap_pointer)
        } else {
            self.allocate_in_new_partition(mem, size)
        }
    }

    pub unsafe fn start_new_allocation_partition<M: Memory>(&mut self, mem: &mut M) {
        self.allocate_in_new_partition(mem, 0);
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_in_new_partition<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        #[cfg(debug_assertions)]
        self.allocation_partition().clear_free_remainder();

        let new_partition = self.allocate_free_partition(size);
        mem.grow_memory(new_partition.end_address() as u64);
        let heap_pointer = new_partition.dynamic_space_end();
        new_partition.dynamic_size += size;
        self.allocation_index = new_partition.index;
        Value::from_ptr(heap_pointer)
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_large_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        if size > usize::MAX - PARTITION_SIZE {
            panic!("Too large allocation");
        }
        let number_of_partitions = (size + PARTITION_SIZE - 1) / PARTITION_SIZE;
        debug_assert!(number_of_partitions > 0);
        let first_index = self.find_large_space(number_of_partitions);
        let last_index = first_index + number_of_partitions - 1;
        let end_address = self.get_partition(last_index).end_address();
        mem.grow_memory(end_address as u64);
        for index in first_index..last_index + 1 {
            let partition = self.mutable_partition(index);
            debug_assert!(partition.free);
            debug_assert!(!partition.large_content);
            partition.free = false;
            partition.large_content = true;
            debug_assert_eq!(partition.static_size, 0);
            debug_assert_eq!(partition.dynamic_size, 0);
            if index == last_index {
                partition.dynamic_size = size - (number_of_partitions - 1) * PARTITION_SIZE;

                #[cfg(debug_assertions)]
                partition.clear_free_remainder();
            } else {
                partition.dynamic_size = PARTITION_SIZE;
            }
        }
        let first_partition = self.get_partition(first_index);
        Value::from_ptr(first_partition.dynamic_space_start())
    }

    unsafe fn find_large_space(&self, number_of_partitions: usize) -> usize {
        for index in 0..MAX_PARTITIONS {
            let mut count = 0;
            while count < number_of_partitions
                && index + count < MAX_PARTITIONS
                && self.get_partition(index + count).is_completely_free()
            {
                count += 1;
            }
            if count == number_of_partitions {
                return index;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    pub unsafe fn collect_large_objects(&mut self) {
        let mut index = 0;
        while index < MAX_PARTITIONS {
            let partition = self.get_partition(index);
            if partition.has_large_content() {
                let object = partition.dynamic_space_start() as *mut Obj;
                let size = block_size(object as usize).to_bytes().as_usize();
                debug_assert!(size > PARTITION_SIZE);
                let number_of_partitions = (size + PARTITION_SIZE - 1) / PARTITION_SIZE;
                if !object.is_marked() {
                    let start_partition = object as usize / PARTITION_SIZE;
                    self.free_large_space(start_partition, number_of_partitions);
                }
                index += number_of_partitions;
            } else {
                index += 1;
            }
        }
    }

    unsafe fn free_large_space(&mut self, start_partition: usize, number_of_partitions: usize) {
        for index in start_partition..start_partition + number_of_partitions {
            let partition = self.mutable_partition(index);
            debug_assert!(partition.large_content);
            let size = partition.dynamic_size;
            partition.free();
            self.reclaimed += size as u64;
        }
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn mark_large_object(&mut self, object: *mut Obj) {
        let object_size = block_size(object as usize).to_bytes().as_usize();
        debug_assert!(object_size > PARTITION_SIZE);
        let number_of_partitions = (object_size + PARTITION_SIZE - 1) / PARTITION_SIZE;
        debug_assert!(number_of_partitions > 1);
        let start_partition = object as usize / PARTITION_SIZE;
        let end_partition = start_partition + number_of_partitions - 1;
        for index in start_partition..end_partition {
            self.partitions[index].marked_space = PARTITION_SIZE;
        }
        self.partitions[end_partition].marked_space = object_size % PARTITION_SIZE;
    }
}
