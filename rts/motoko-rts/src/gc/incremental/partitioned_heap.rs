//! Partitioned heap used in incremental GC for compacting evacuation.
//! The heap is divided in equal sized partitions of a large size `PARTITION_SIZE`.
//! The first partition(s) may contains a static heap space with static objects that are never moved.
//! Beyond the static objects of a partition, the dynamic heap space starts with `dynamic_size`.
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
//! Whenever a partition is full or has insufficient space to accommodate a new allocation,
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

use core::{array::from_fn, ops::Range};

use crate::{
    gc::incremental::mark_bitmap::BITMAP_ITERATION_END, memory::Memory, rts_trap_with, types::*,
};

use super::mark_bitmap::{BitmapIterator, MarkBitmap, BITMAP_SIZE};

/// Size of each parition.
pub const PARTITION_SIZE: usize = 32 * 1024 * 1024;

/// Total number of partitions in the memory.
/// For simplicity, the last partition is left unused, to avoid a numeric overflow when
/// computing the end address of the last partition.
const MAX_PARTITIONS: usize = usize::MAX / PARTITION_SIZE;

/// Partitions are only evacuated if the space occupation of alive object in the partition
/// is greater than this threshold.
pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.85;

/// Heap partition of size `PARTITION_SIZE`.
pub struct Partition {
    index: usize,               // Index of the partition `0..MAX_PARTITIONS`.
    free: bool,                 // Denotes a free partition (which may still contain static space).
    large_content: bool, // Specifies whether a large object is contained that spans multiple partitions.
    marked_size: usize,  // Total amount of marked object space in the dynamic space.
    static_size: usize,  // Size of the static space.
    dynamic_size: usize, // Size of the dynamic space.
    bitmap: Option<MarkBitmap>, // Mark bitmap used for marking objects inside this partition.
    temporary: bool,     // Specifies a temporary partition used during a GC run to store bitmaps.
    evacuate: bool,      // Specifies whether the partition is to be evacuated or being evacuated.
    update: bool,        // Specifies whether the pointers in the partition have to be updated.
}

impl Partition {
    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn start_address(&self) -> usize {
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

    pub fn dynamic_size(&self) -> usize {
        self.dynamic_size
    }

    pub fn marked_size(&self) -> usize {
        self.marked_size
    }

    pub fn free_size(&self) -> usize {
        self.end_address() - self.dynamic_space_end()
    }

    pub fn is_free(&self) -> bool {
        self.free
    }

    pub fn to_be_evacuated(&self) -> bool {
        self.evacuate
    }

    pub fn to_be_updated(&self) -> bool {
        self.update
    }

    pub fn is_temporary(&self) -> bool {
        self.temporary
    }

    pub fn has_dynamic_space(&self) -> bool {
        !self.free && !self.temporary && self.static_size != PARTITION_SIZE
    }

    pub fn get_bitmap(&self) -> &MarkBitmap {
        self.bitmap.as_ref().unwrap()
    }

    pub fn mutable_bitmap(&mut self) -> &mut MarkBitmap {
        self.bitmap.as_mut().unwrap()
    }

    #[cfg(feature = "memory_check")]
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
        debug_assert!(self.evacuate || self.large_content || self.temporary);
        debug_assert_eq!(self.marked_size, 0);
        debug_assert!(self.bitmap.is_none());
        debug_assert!(!self.update);
        self.free = true;
        self.dynamic_size = 0;
        self.evacuate = false;
        self.large_content = false;
        self.temporary = false;

        #[cfg(feature = "memory_check")]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        debug_assert!(self.marked_size <= dynamic_heap_space);
        self.marked_size as f64 / dynamic_heap_space as f64
    }

    pub fn has_large_content(&self) -> bool {
        self.large_content
    }

    pub fn is_completely_free(&self) -> bool {
        self.free && self.free_size() == PARTITION_SIZE
    }
}

const NO_LARGE_OBJECT: usize = usize::MAX;

#[derive(Clone, Copy)]
struct LargeObjectIterator {
    current_address: usize,
    visited: bool,
}

impl LargeObjectIterator {
    fn none() -> LargeObjectIterator {
        LargeObjectIterator { 
            current_address: NO_LARGE_OBJECT, 
            visited: true 
        }
    }
}

/// Iterator state that can be stored between GC increments.
/// Keeps the state of a `PartitionedHeapIterator` with an inner `PartititionIterator`.
pub struct HeapIteratorState {
    partition_index: usize,
    bitmap_iterator: Option<BitmapIterator>,
    large_iterator: LargeObjectIterator,
}

impl HeapIteratorState {
    pub fn new() -> HeapIteratorState {
        HeapIteratorState {
            partition_index: 0,
            bitmap_iterator: None,
            large_iterator: LargeObjectIterator::none(),
        }
    }

    pub fn completed(&self) -> bool {
        debug_assert!(self.partition_index <= MAX_PARTITIONS);
        self.partition_index == MAX_PARTITIONS
    }
}

/// Iterates over all partitions, by skipping free partitions and the subsequent partitions
/// of large objects. Instantiated per GC increment, operating on a stored `HeapIteratorState`.
/// Due to borrow checker restrictions, the iterator does not directly reference the state
/// but needs to be explicitly loaded and stored from the state by the GC increments.
pub struct PartitionedHeapIterator {
    partition_index: usize,
}

impl PartitionedHeapIterator {
    pub fn load_from(heap: &PartitionedHeap, state: &HeapIteratorState) -> PartitionedHeapIterator {
        let partition_index = state.partition_index;
        let mut iterator = PartitionedHeapIterator { partition_index };
        iterator.skip_empty_partitions(heap);
        iterator
    }

    pub fn save_to(&self, state: &mut HeapIteratorState) {
        state.partition_index = self.partition_index;
    }

    fn skip_empty_partitions(&mut self, heap: &PartitionedHeap) {
        loop {
            if self.partition_index == MAX_PARTITIONS {
                return;
            }
            let partition = heap.get_partition(self.partition_index);
            if partition.has_dynamic_space() {
                return;
            }
            self.partition_index += 1;
        }
    }

    pub fn has_partition(&self) -> bool {
        self.partition_index < MAX_PARTITIONS
    }

    pub fn current_partition<'a>(&self, heap: &'a PartitionedHeap) -> &'a Partition {
        debug_assert!(self.partition_index < MAX_PARTITIONS);
        heap.get_partition(self.partition_index)
    }

    pub unsafe fn next_partition(&mut self, heap: &PartitionedHeap) {
        debug_assert!(self.partition_index < MAX_PARTITIONS);
        let partition = heap.get_partition(self.partition_index);
        let number_of_partitions = if partition.has_large_content() {
            let large_object = partition.dynamic_space_start() as *mut Obj;
            PartitionedHeap::partitions_length(large_object)
        } else {
            1
        };
        self.partition_index += number_of_partitions;
        self.skip_empty_partitions(heap);
        debug_assert!(self.partition_index <= MAX_PARTITIONS);
    }
}

/// Iterates over the marked objects in the partition.
/// Instantiated per GC increment, operating on a stored `HeapIteratorState`.
pub struct PartitionIterator {
    partition_start: usize,
    bitmap_iterator: Option<BitmapIterator>,
    large_iterator: LargeObjectIterator,
}

impl PartitionIterator {
    pub unsafe fn load_from(
        partition: &Partition,
        state: &mut HeapIteratorState,
    ) -> PartitionIterator {
        let partition_start = partition.start_address();
        let bitmap_iterator = if partition.has_large_content() {
            debug_assert!(state.bitmap_iterator.is_none());
            None
        } else if state.bitmap_iterator.is_none()
            || !state
                .bitmap_iterator
                .as_ref()
                .unwrap()
                .belongs_to(partition.bitmap.as_ref().unwrap())
        {
            Some(partition.get_bitmap().iterate())
        } else {
            state.bitmap_iterator.take()
        };
        let large_iterator = if !partition.has_large_content() || partition.marked_size() == 0
        {
            LargeObjectIterator::none()
        } else if partition_start != state.large_iterator.current_address {
            LargeObjectIterator {
                current_address: partition_start,
                visited: false
            }
        } else {
            state.large_iterator 
        };
        PartitionIterator {
            partition_start,
            bitmap_iterator,
            large_iterator,
        }
    }

    pub fn save_to(&mut self, state: &mut HeapIteratorState) {
        state.bitmap_iterator = self.bitmap_iterator.take();
        state.large_iterator = self.large_iterator;
    }

    pub fn has_object(&self) -> bool {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            offset != BITMAP_ITERATION_END
        } else {
            debug_assert_ne!(self.large_iterator.current_address, NO_LARGE_OBJECT);
            !self.large_iterator.visited
        }
    }

    pub fn current_object(&self) -> *mut Obj {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            debug_assert_ne!(offset, BITMAP_ITERATION_END);
            let address = self.partition_start + offset;
            address as *mut Obj
        } else {
            debug_assert_ne!(self.large_iterator.current_address, NO_LARGE_OBJECT);
            debug_assert!(!self.large_iterator.visited);
            self.large_iterator.current_address as *mut Obj
        }
    }   

    pub fn next_object(&mut self) {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_mut().unwrap();
            debug_assert_ne!(iterator.current_marked_offset(), BITMAP_ITERATION_END);
            iterator.next();
        } else {
            debug_assert_ne!(self.large_iterator.current_address, NO_LARGE_OBJECT);
            debug_assert!(!self.large_iterator.visited);
            self.large_iterator.visited = true;
        }
    }
}

/// Partitioned heap used by the incremental GC.
pub struct PartitionedHeap {
    partitions: [Partition; MAX_PARTITIONS],
    heap_base: usize,
    allocation_index: usize, // Index of the partition currently used for allocations.
    evacuating: bool,
    reclaimed: u64,
    bitmap_pointer: usize, // Allocation pointer for mark bitmaps.
    gc_running: bool,      // Create bitmaps for partitions whn allocated during active GC.
}

impl PartitionedHeap {
    pub unsafe fn new<M: Memory>(mem: &mut M, heap_base: usize) -> PartitionedHeap {
        let allocation_index = heap_base / PARTITION_SIZE;
        mem.grow_memory(((allocation_index + 1) * PARTITION_SIZE) as u64);
        let partitions = from_fn(|index| Partition {
            index,
            free: index > allocation_index,
            large_content: false,
            marked_size: 0,
            static_size: if index < allocation_index {
                PARTITION_SIZE
            } else if index == allocation_index {
                heap_base % PARTITION_SIZE
            } else {
                0
            },
            dynamic_size: 0,
            bitmap: None,
            temporary: false,
            evacuate: false,
            update: false,
        });
        PartitionedHeap {
            partitions,
            heap_base,
            allocation_index,
            evacuating: false,
            reclaimed: 0,
            bitmap_pointer: 0,
            gc_running: false,
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

    unsafe fn allocate_temporary_partition(&mut self) -> &mut Partition {
        for partition in &mut self.partitions {
            if partition.free && partition.is_completely_free() {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                partition.temporary = true;
                return partition;
            }
        }
        rts_trap_with("Cannot grow memory");
    }

    unsafe fn allocate_bitmap<M: Memory>(&mut self, mem: &mut M) -> MarkBitmap {
        if self.bitmap_pointer % PARTITION_SIZE == 0 {
            let partition = self.allocate_temporary_partition();
            mem.grow_memory(partition.end_address() as u64);
            self.bitmap_pointer = partition.start_address();
        }
        let bitmap = MarkBitmap::allocate(self.bitmap_pointer);
        self.bitmap_pointer += BITMAP_SIZE;
        bitmap
    }

    // Optimization: Returns true if the object transitioned from unmarked to marked.
    pub unsafe fn mark_object(&mut self, object: *mut Obj) -> bool {
        let address = object as usize;
        let partition_index = address / PARTITION_SIZE;
        let partition = self.mutable_partition(partition_index);
        if partition.has_large_content() {
            return self.mark_large_object(object);
        }
        let bitmap = partition.mutable_bitmap();
        let offset = address % PARTITION_SIZE;
        if bitmap.is_marked(offset) {
            return false;
        }
        bitmap.mark(offset);
        partition.marked_size += block_size(address).to_bytes().as_usize();
        true
    }

    pub unsafe fn start_collection<M: Memory>(&mut self, mem: &mut M) {
        debug_assert_eq!(self.bitmap_pointer, 0);
        debug_assert!(!self.gc_running);
        self.gc_running = true;
        for partition_index in 0..MAX_PARTITIONS {
            let partition = self.get_partition(partition_index);
            if partition.has_dynamic_space() && !partition.has_large_content() {
                let bitmap = self.allocate_bitmap(mem);
                self.mutable_partition(partition_index).bitmap = Some(bitmap);
            }
        }
        if self.allocation_partition().dynamic_size > PARTITION_SIZE / 2 {
            // Allow reclaiming objects in current allocation partition.
            self.start_new_allocation_partition(mem);
        }
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

    pub fn plan_updates(&mut self) {
        for partition in &mut self.partitions {
            debug_assert!(!partition.update);
            partition.update = !partition.is_free() && !partition.evacuate;
        }
    }

    pub unsafe fn complete_collection(&mut self) {
        for partition in &mut self.partitions {
            let marked_size = partition.marked_size;
            partition.update = false;
            partition.marked_size = 0;
            partition.bitmap = None;
            if partition.to_be_evacuated() {
                debug_assert!(partition.index != self.allocation_index);
                debug_assert!(partition.dynamic_size >= marked_size);
                self.reclaimed += (partition.dynamic_size - marked_size) as u64;
                partition.free();
            } else if partition.temporary {
                partition.free();
            }
        }
        self.evacuating = false;
        self.bitmap_pointer = 0;
        debug_assert!(self.gc_running);
        self.gc_running = false;
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

    unsafe fn allocate_free_partition<M: Memory>(
        &mut self,
        mem: &mut M,
        requested_space: usize,
    ) -> &mut Partition {
        let bitmap = if self.gc_running {
            Some(self.allocate_bitmap(mem))
        } else {
            None
        };
        for partition in &mut self.partitions {
            if partition.free && partition.free_size() >= requested_space {
                debug_assert_eq!(partition.dynamic_size, 0);
                partition.free = false;
                if bitmap.is_some() {
                    partition.bitmap = bitmap;
                }
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
        #[cfg(feature = "memory_check")]
        self.allocation_partition().clear_free_remainder();

        let new_partition = self.allocate_free_partition(mem, size);
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
            debug_assert!(partition.bitmap.is_none());
            debug_assert_eq!(partition.static_size, 0);
            debug_assert_eq!(partition.dynamic_size, 0);
            if index == last_index {
                partition.dynamic_size = size - (number_of_partitions - 1) * PARTITION_SIZE;

                #[cfg(feature = "memory_check")]
                partition.clear_free_remainder();
            } else {
                partition.dynamic_size = PARTITION_SIZE;
            }
        }
        let first_partition = self.mutable_partition(first_index);
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

    unsafe fn occupied_partition_range(large_object: *mut Obj) -> Range<usize> {
        debug_assert_eq!(large_object as usize % PARTITION_SIZE, 0);
        let start_partition = large_object as usize / PARTITION_SIZE;
        let number_of_partitions = Self::partitions_length(large_object);
        start_partition..start_partition + number_of_partitions
    }

    unsafe fn partitions_length(large_object: *mut Obj) -> usize {
        let size = block_size(large_object as usize).to_bytes().as_usize();
        debug_assert!(size > PARTITION_SIZE);
        (size + PARTITION_SIZE - 1) / PARTITION_SIZE
    }

    pub unsafe fn collect_large_objects(&mut self) {
        let mut index = 0;
        while index < MAX_PARTITIONS {
            let partition = self.get_partition(index);
            if partition.has_large_content() {
                debug_assert!(!partition.free);
                let object = partition.dynamic_space_start() as *mut Obj;
                let number_of_partitions = Self::partitions_length(object);
                if !object.is_marked() {
                    self.free_large_object(object);
                }
                index += number_of_partitions;
            } else {
                index += 1;
            }
        }
    }

    unsafe fn free_large_object(&mut self, object: *mut Obj) {
        for index in Self::occupied_partition_range(object) {
            let partition = self.mutable_partition(index);
            debug_assert!(partition.large_content);
            let size = partition.dynamic_size;
            partition.update = false;
            partition.bitmap = None;
            partition.free();
            self.reclaimed += size as u64;
        }
    }

    // Significant performance gain by not inlining.
    // Optimization: Returns true if it has not yet been marked before.
    #[inline(never)]
    unsafe fn mark_large_object(&mut self, object: *mut Obj) -> bool {
        let range = Self::occupied_partition_range(object);
        if self.partitions[range.start].marked_size > 0 {
            return false;
        }
        for index in range.start..range.end - 1 {
            self.partitions[index].marked_size = PARTITION_SIZE;
        }
        let object_size = block_size(object as usize).to_bytes().as_usize();
        self.partitions[range.end - 1].marked_size = object_size % PARTITION_SIZE;
        true
    }
}
