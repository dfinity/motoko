//! Partitioned heap used in incremental GC for compacting evacuation.
//! The heap is divided in equal sized partitions of a large size `PARTITION_SIZE`.
//! The first partition(s) may contain a static heap space with static objects that are never moved.
//! Each partition contains a descriptor at its beginning, except for the first partitions where
//! the descriptor follows after the static space. The descriptor is followed by the dynamic space
//! and potential free space. The partition descriptors contains information about the partition,
//! such as its dynamic space size, whether it stores a large object etc.
//!
//! Heap layout (with a dynamic number of partitions):
//! ┌───────────────┬───────────────┬───────────────┬───────────────┐
//! │  partition 0  │  partition 1  |  partition 2  |      ...      |
//! └───────────────┴───────────────┴───────────────┴───────────────┘
//!
//! First partition (with static space < PARTITION_CAPACITY):
//! ┌───────────────┬───────────────┬───────────────┬───────────────┐
//! │ static_space  │  descriptor   | dynamic_space |  free_space   |
//! └───────────────┴───────────────┴───────────────┴───────────────┘
//!
//! Subsequent partitions (for object sizes <= PARTITION_CAPACITY):
//! ┌───────────────┬───────────────────────────────┬───────────────┐
//! │  descriptor   |         dynamic_space         |  free_space   |
//! └───────────────┴───────────────────────────────┴───────────────┘
//!
//! The heap defines an allocation partition that is the target for subsequent object allocations
//! by using efficient bump allocation inside the allocation partition.
//! Whenever a partition is full or has insufficient space to accommodate a new allocation,
//! a new empty partition is selected for allocation.
//!
//! On garbage collection, partitions are selected for evacuation by prioritizing high-garbage
//! partitions. The live objects of the partitions selected for evacuation are moved out to
//! other remaining partitions (through allocation). Thereby, objects from different evacuated
//! partitions can be allocated to a common partition. Allocation partitions must not be evacuated.
//!
//! To prevent that the evacuation phase runs out of free space, the number of evacuated
//! partitions is limited to the free space that is available and needed for the evacuations.
//!
//! Large allocations:
//! Large objects with a size > PARTITION_SIZE are allocated across multiple contiguous free
//! partitions. For this purpose, a corresponding sequence of contiguous free partitions needs
//! to be searched. Only the first partition of a large allocation carries the descriptor, i.e.
//! subsequent overflow partitions contain only dynamic space with potential free space in the
//! last partition. Large objects stay in their partitions for their entire lifetime, i.e. they
//! are never evacuated. When becoming garbage, the underlying partitions of a large object are
//! immediately freed. Large object allocation may be prone to external fragmentation problems,
//! i.e. that no sufficient contiguous free partitions are available on allocation. Currently,
//! this external fragmentation problem is not handled by moving other partitions which would
//! require a special blocking full GC collection. Moreover, for simplicity, the remainder
//! of the last partition of a huge object is not used for further small object allocations,
//! which implies limited internal fragmentation.
//!
//! Large allocation: Series of contiguous partitions:
//! ┌───────────────┬───────────────────────────────────────────────┐
//! │  descriptor   |                dynamic_space                  |
//! └───────────────┴───────────────────────────────────────────────┘
//! ┌───────────────────────────────────────────────────────────────┐
//! │                        dynamic_space                          |
//! └───────────────────────────────────────────────────────────────┘
//!  ...
//! ┌───────────────────────────────────────────────┬───────────────┐
//! │                  dynamic_space                |  free_space   |
//! └───────────────────────────────────────────────┴───────────────┘
//!
//! Simplification:
//! * The last partition in the address space is not used to avoid an overflow when computing
//!   the partition end address.
//!
use core::{iter::Iterator, ptr::null_mut};

use crate::{
    constants::MB, gc::incremental::mark_bitmap::BITMAP_ITERATION_END, memory::Memory,
    rts_trap_with, types::*,
};

use super::{
    mark_bitmap::{BitmapIterator, MarkBitmap, BITMAP_SIZE},
    time::BoundedTime,
};

/// Size of each partition.
/// Select the size of the power of two with the smallest WASM memory size in the benchmark.
/// -> Small partitions below 32 MB are inefficient in terms of both memory and runtime costs
///    due to the increased frequency of large object handling.
/// -> Large partitions above 32 MB are a waste for small programs, since the WASM memory is
///    allocated in that granularity and GC is then triggered later.
pub const PARTITION_SIZE: usize = 32 * MB;

/// Partitions are only evacuated if the space occupation of alive objects in the partition
/// is less than this threshold.
/// Based on benchmark measurements, this rate is tuned to optimize the following metrics
/// in the order of occurrence:
/// 1. Lowest heap size (most reclamation).
/// 2. Lowest WASM memory size (i.e. for minimum heap size).
pub const SURVIVAL_RATE_THRESHOLD: f64 = 0.85;

/// Stored at the beginning of a partition, except for:
/// * First partition: The descriptors follows the static space.
/// * Large allocations: The overflow partitions of a large allocations does not contain a descriptor.
#[repr(C)]
pub struct PartitionDescriptor {
    free: bool,          // Denotes a free partition (which may still contain static space).
    large_content: bool, // Specifies whether a large object is contained that spans multiple partitions.
    temporary: bool,     // Specifies a temporary partition used during a GC run to store bitmaps.
    evacuate: bool,      // Specifies whether the partition is to be evacuated or being evacuated.
    update: bool,        // Specifies whether the pointers in the partition have to be updated.
    marked_size: usize, // Total amount of marked object space in the dynamic space. For a large object, this can be larger than the partition capacity.
    dynamic_size: usize, // Size of the dynamic space. For a large object, this can be larger than the partition capacity.
    bitmap: MarkBitmap,  // Mark bitmap used for marking objects inside this partition.
                         // Unused reserve for future extensions, up to `DESCRIPTOR_SIZE`.
}

impl PartitionDescriptor {
    pub fn new(free: bool) -> PartitionDescriptor {
        PartitionDescriptor {
            free,
            large_content: false,
            temporary: false,
            evacuate: false,
            update: false,
            marked_size: 0,
            dynamic_size: 0,
            bitmap: MarkBitmap::new(),
        }
    }
}

pub const PARTITION_DESCRIPTOR_SIZE: usize = 6 * core::mem::size_of::<usize>();

const _: () = assert!(core::mem::size_of::<PartitionDescriptor>() <= PARTITION_DESCRIPTOR_SIZE);

pub const PARTITION_CAPACITY: usize = PARTITION_SIZE - PARTITION_DESCRIPTOR_SIZE;

/// Heap partition of size `PARTITION_SIZE`.
#[repr(C)]
pub struct Partition {
    index: usize,       // Index of the partition.
    static_size: usize, // Size of the static space in the partition.
}

impl Partition {
    // Not to be used for:
    // * First partitions that only contain static space.
    // * Overflow partitions of a large allocation.
    pub fn get(index: usize, heap_base: usize) -> Partition {
        // Do not access potential first partitions that only contain static space.
        debug_assert!(index >= (heap_base + PARTITION_DESCRIPTOR_SIZE) / PARTITION_SIZE);
        let start_address = index * PARTITION_SIZE;
        // The last partition in the address space is not used, see above.
        debug_assert!(start_address <= usize::MAX - PARTITION_SIZE);
        let static_size = if heap_base > start_address {
            heap_base - start_address
        } else {
            0
        };
        debug_assert!(static_size + PARTITION_DESCRIPTOR_SIZE <= PARTITION_SIZE);
        Partition { index, static_size }
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn start_address(&self) -> usize {
        self.index * PARTITION_SIZE
    }

    fn end_address(&self) -> usize {
        self.start_address() + PARTITION_SIZE
    }

    fn get_descriptor(&self) -> *mut PartitionDescriptor {
        debug_assert!(self.static_size + PARTITION_DESCRIPTOR_SIZE <= PARTITION_SIZE);
        let descriptor_address = self.start_address() + self.static_size;
        descriptor_address as *mut PartitionDescriptor
    }

    pub fn initialize(&self, initial_value: PartitionDescriptor) {
        let descriptor = self.get_descriptor();
        unsafe {
            *descriptor = initial_value;
        }
    }

    pub fn dynamic_space_start(&self) -> usize {
        self.get_descriptor() as usize + PARTITION_DESCRIPTOR_SIZE
    }

    pub fn dynamic_space_end(&self) -> usize {
        self.dynamic_space_start() + self.dynamic_size()
    }

    pub fn dynamic_size(&self) -> usize {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).dynamic_size }
    }

    pub fn set_dynamic_size(&self, dynamic_size: usize) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).dynamic_size = dynamic_size;
        }
    }

    pub fn occuped_size(&self) -> usize {
        self.dynamic_size() + self.static_size
    }

    pub fn marked_size(&self) -> usize {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).marked_size }
    }

    pub fn set_marked_size(&self, marked_size: usize) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).marked_size = marked_size;
        }
    }

    pub fn free_size(&self) -> usize {
        self.end_address() - self.dynamic_space_end()
    }

    pub fn garbage_amount(&self) -> usize {
        debug_assert!(self.marked_size() <= self.dynamic_size());
        self.dynamic_size() - self.marked_size()
    }

    pub fn is_free(&self) -> bool {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).free }
    }

    pub fn set_free(&self, is_free: bool) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).free = is_free;
        }
    }

    pub fn is_evacuation_candidate(&self, survival_rate: f64) -> bool {
        !self.is_free()
            && !self.to_be_evacuated()
            && !self.has_large_content()
            && self.dynamic_size() > 0
            && self.survival_rate() <= survival_rate
    }

    pub fn to_be_evacuated(&self) -> bool {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).evacuate }
    }

    pub fn set_to_be_evacuated(&self, evacuate: bool) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).evacuate = evacuate;
        }
    }

    pub fn to_be_updated(&self) -> bool {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).update }
    }

    pub fn set_to_be_updated(&self, update: bool) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).update = update;
        }
    }

    pub fn is_temporary(&self) -> bool {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).temporary }
    }

    pub fn set_temporary(&self, temporary: bool) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).temporary = temporary;
        }
    }

    pub fn has_dynamic_space(&self) -> bool {
        !self.is_free()
            && !self.is_temporary()
            && self.static_size + PARTITION_DESCRIPTOR_SIZE <= PARTITION_SIZE
    }

    pub fn get_bitmap(&self) -> &mut MarkBitmap {
        let descriptor = self.get_descriptor();
        unsafe { &mut (*descriptor).bitmap }
    }

    pub fn set_bitmap(&self, bitmap: MarkBitmap) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).bitmap = bitmap;
        }
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

    pub unsafe fn reclaim(&self) {
        println!(100, "RECLAIM PARTITION {}", self.index);
        debug_assert!(!self.is_free());
        debug_assert!(self.to_be_evacuated() || self.has_large_content() || self.is_temporary());
        debug_assert_eq!(self.marked_size(), 0);
        debug_assert!(!self.to_be_updated());

        self.set_free(true);
        self.set_dynamic_size(0);
        self.set_to_be_evacuated(false);
        self.set_large_content(false);
        self.set_temporary(false);

        #[cfg(feature = "memory_check")]
        self.clear_free_remainder();
    }

    pub fn survival_rate(&self) -> f64 {
        let dynamic_heap_space = PARTITION_SIZE - self.static_size;
        debug_assert!(self.marked_size() <= dynamic_heap_space);
        self.marked_size() as f64 / dynamic_heap_space as f64
    }

    pub fn has_large_content(&self) -> bool {
        let descriptor = self.get_descriptor();
        unsafe { (*descriptor).large_content }
    }

    pub fn set_large_content(&self, has_large_content: bool) {
        let descriptor = self.get_descriptor();
        unsafe {
            (*descriptor).large_content = has_large_content;
        }
    }

    pub fn is_completely_free(&self) -> bool {
        self.is_free() && self.free_size() == PARTITION_CAPACITY
    }

    pub fn next_partition_index(&self) -> usize {
        let consecutive_partitions = if self.has_large_content() {
            let large_object = self.dynamic_space_start() as *mut Obj;
            PartitionedHeap::consecutive_partitions(large_object)
        } else {
            1
        };
        self.index + consecutive_partitions
    }
}

/// Iterates over all partitions and their contained marked objects, by skipping
/// free partitions, the subsequent partitions of large objects, and unmarked objects.
pub struct PartitionedHeapIterator {
    partition_index: usize,
    number_of_partitions: usize,
    bitmap_iterator: Option<BitmapIterator>,
    visit_large_object: bool,
}

impl PartitionedHeapIterator {
    pub fn new(heap: &PartitionedHeap) -> PartitionedHeapIterator {
        let mut iterator = PartitionedHeapIterator {
            partition_index: heap.first_partition_index(),
            number_of_partitions: heap.number_of_partitions(),
            bitmap_iterator: None,
            visit_large_object: false,
        };
        iterator.skip_empty_partitions(heap);
        iterator.start_object_iteration(heap);
        iterator
    }

    fn skip_empty_partitions(&mut self, heap: &PartitionedHeap) {
        loop {
            if self.partition_index >= self.number_of_partitions {
                return;
            }
            let partition = heap.get_partition(self.partition_index);
            unsafe {
                println!(
                    100,
                    " CHECK PARTITION {} {}",
                    partition.index,
                    partition.is_temporary()
                );
            }
            if partition.has_dynamic_space() {
                return;
            }
            self.partition_index = partition.next_partition_index();
        }
    }

    pub fn has_partition(&self) -> bool {
        self.partition_index < self.number_of_partitions
    }

    pub fn current_partition<'a>(&self, heap: &'a PartitionedHeap) -> Partition {
        debug_assert!(self.partition_index < self.number_of_partitions);
        heap.get_partition(self.partition_index)
    }

    pub unsafe fn next_partition(&mut self, heap: &PartitionedHeap) {
        debug_assert!(self.partition_index < self.number_of_partitions);
        let partition = heap.get_partition(self.partition_index);
        self.partition_index = partition.next_partition_index();
        unsafe {
            println!(
                100,
                "NEXT ITERATION {} {}",
                partition.index,
                partition.has_large_content()
            );
        }
        self.skip_empty_partitions(heap);
        self.start_object_iteration(heap)
    }

    fn start_object_iteration(&mut self, heap: &PartitionedHeap) {
        debug_assert!(self.partition_index <= self.number_of_partitions);
        if self.partition_index >= self.number_of_partitions {
            self.bitmap_iterator = None;
            self.visit_large_object = false;
        } else {
            let partition = heap.get_partition(self.partition_index);
            unsafe {
                println!(
                    100,
                    "START ITERATION {} {}",
                    partition.index,
                    partition.has_large_content()
                );
            }
            if partition.has_large_content() {
                self.bitmap_iterator = None;
                self.visit_large_object = partition.marked_size() > 0
            } else {
                self.bitmap_iterator = Some(partition.get_bitmap().iterate());
                self.visit_large_object = false;
            }
        }
    }

    pub fn has_object(&self) -> bool {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            offset != BITMAP_ITERATION_END
        } else {
            self.visit_large_object
        }
    }

    pub fn current_object(&self) -> *mut Obj {
        let partition_start = self.partition_index * PARTITION_SIZE;
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_ref().unwrap();
            let offset = iterator.current_marked_offset();
            debug_assert_ne!(offset, BITMAP_ITERATION_END);
            let address = partition_start + offset;
            address as *mut Obj
        } else {
            debug_assert!(self.visit_large_object);
            (partition_start + PARTITION_DESCRIPTOR_SIZE) as *mut Obj
        }
    }

    pub fn next_object(&mut self) {
        if self.bitmap_iterator.is_some() {
            let iterator = self.bitmap_iterator.as_mut().unwrap();
            debug_assert_ne!(iterator.current_marked_offset(), BITMAP_ITERATION_END);
            iterator.next();
        } else {
            debug_assert!(self.visit_large_object);
            self.visit_large_object = false;
        }
    }
}

/// Partitioned heap used by the incremental GC.
pub struct PartitionedHeap {
    heap_base: usize,                 // Address of the dynamic heap start.
    memory_size: usize,               // Allocated Wasm memory space (in bytes).
    allocation_index: usize,          // Index of the partition currently used for allocations.
    free_partitions: usize,           // Number of free partitions.
    evacuating: bool,                 // Evacuation phase is in progress.
    reclaimed: u64, // Total amount of reclaimed memory since installation or upgrade.
    bitmap_allocation_pointer: usize, // Free pointer for allocating the next mark bitmap.
    gc_running: bool, // Create bitmaps for partitions when allocated during active GC.
    precomputed_heap_size: usize, // Occupied heap size, excluding the dynamic heap in the allocation partition.
    evacuated_size: usize, // Size of all evacuated objects during a GC run. Serves for accurate total allocation statistics.
}

/// Iterates over all partitions in the heap, regardless of whether they are free or occuppied.
/// Overflow partitions of large allocations are skipped.
struct PartitionIterator {
    partition_index: usize,
    heap_base: usize,
    memory_size: usize,
}

impl Iterator for PartitionIterator {
    type Item = Partition;

    fn next(&mut self) -> Option<Self::Item> {
        if self.has_partition() {
            let result = Some(self.current_partition());
            self.next_partition();
            result
        } else {
            None
        }
    }
}

impl PartitionIterator {
    pub fn new(heap: &PartitionedHeap) -> PartitionIterator {
        PartitionIterator {
            partition_index: heap.first_partition_index(),
            heap_base: heap.heap_base,
            memory_size: heap.memory_size,
        }
    }

    fn number_of_partitions(&self) -> usize {
        debug_assert_eq!(self.memory_size % PARTITION_SIZE, 0);
        self.memory_size / PARTITION_SIZE
    }

    pub fn has_partition(&self) -> bool {
        unsafe {
            println!(
                100,
                "ITERATOR {} {}",
                self.partition_index,
                self.number_of_partitions()
            );
        }

        self.partition_index < self.number_of_partitions()
    }

    pub fn current_partition(&self) -> Partition {
        Partition::get(self.partition_index, self.heap_base)
    }

    pub fn next_partition(&mut self) {
        let partition = self.current_partition();
        self.partition_index = partition.next_partition_index();
    }
}

const NO_BITMAP_ALLOCATION_POINTER: usize = 0;

/// Optimization: Avoiding `Option` or `LazyCell`.
pub const UNINITIALIZED_HEAP: PartitionedHeap = PartitionedHeap {
    heap_base: 0,
    memory_size: 0,
    allocation_index: 0,
    free_partitions: 0,
    evacuating: false,
    reclaimed: 0,
    bitmap_allocation_pointer: NO_BITMAP_ALLOCATION_POINTER,
    gc_running: false,
    precomputed_heap_size: 0,
    evacuated_size: 0,
};

impl PartitionedHeap {
    pub fn new<M: Memory>(mem: &mut M, heap_base: usize) -> PartitionedHeap {
        let allocation_index = (heap_base + PARTITION_DESCRIPTOR_SIZE) / PARTITION_SIZE;
        let memory_size = (allocation_index + 1) * PARTITION_SIZE;
        unsafe {
            mem.grow_memory(memory_size as u64);
        }
        let first_partition = Partition::get(allocation_index, heap_base);
        first_partition.initialize(PartitionDescriptor::new(false));
        unsafe {
            println!(100, "NEW HEAP PARTITION {}", first_partition.index);
        }
        PartitionedHeap {
            heap_base,
            memory_size,
            allocation_index,
            free_partitions: 0,
            evacuating: false,
            reclaimed: 0,
            bitmap_allocation_pointer: 0,
            gc_running: false,
            precomputed_heap_size: heap_base,
            evacuated_size: 0,
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.memory_size > 0
    }

    pub fn base_address(&self) -> usize {
        self.heap_base
    }

    // First partition with dynamic space.
    pub fn first_partition_index(&self) -> usize {
        (self.heap_base + PARTITION_DESCRIPTOR_SIZE) / PARTITION_SIZE
    }

    pub fn number_of_partitions(&self) -> usize {
        debug_assert_eq!(self.memory_size % PARTITION_SIZE, 0);
        self.memory_size / PARTITION_SIZE
    }

    // Must not be called for overflow partitions of a large allocation.
    pub fn get_partition(&self, index: usize) -> Partition {
        Partition::get(index, self.heap_base)
    }

    fn extend_memory<M: Memory>(&mut self, mem: &mut M) -> Partition {
        if self.memory_size >= usize::MAX - PARTITION_SIZE {
            unsafe {
                rts_trap_with("Cannot grow memory");
            }
        }
        let partition_index = self.number_of_partitions();
        self.memory_size += PARTITION_SIZE;
        unsafe {
            mem.grow_memory(self.memory_size as u64);
        }
        let partition = Partition::get(partition_index, self.heap_base);
        partition.initialize(PartitionDescriptor::new(true));
        self.free_partitions += 1;
        unsafe {
            println!(100, "EXTEND MEMORY {}", self.free_partitions);
        }
        partition
    }

    fn partitions(&self) -> PartitionIterator {
        PartitionIterator::new(self)
    }

    fn find_free_temporary_partition<M: Memory>(&mut self, mem: &mut M) -> Partition {
        for partition in self.partitions() {
            if partition.is_completely_free() {
                return partition;
            }
        }
        self.extend_memory(mem)
    }

    fn allocate_temporary_partition<M: Memory>(&mut self, mem: &mut M) -> Partition {
        let partition = self.find_free_temporary_partition(mem);
        unsafe {
            println!(100, "TEMPORARY PARTITION {}", partition.index);
        }
        debug_assert_eq!(partition.dynamic_size(), 0);
        partition.set_free(false);
        partition.set_temporary(true);
        debug_assert!(self.free_partitions > 0);
        self.free_partitions -= 1;
        partition
    }

    /// The returned bitmap address is guaranteed to be 64-bit-aligned.
    /// Note: The bitmap allocator must not overwrite the descriptor.
    fn allocate_bitmap<M: Memory>(&mut self, mem: &mut M) -> *mut u8 {
        if self.bitmap_allocation_pointer == NO_BITMAP_ALLOCATION_POINTER
            || self.bitmap_allocation_pointer % PARTITION_SIZE + BITMAP_SIZE > PARTITION_SIZE
        {
            let partition = self.allocate_temporary_partition(mem);
            self.bitmap_allocation_pointer = partition.dynamic_space_start();
        }
        let bitmap_address = self.bitmap_allocation_pointer as *mut u8;
        self.bitmap_allocation_pointer += BITMAP_SIZE;
        debug_assert_eq!(
            bitmap_address as usize % size_of::<u64>().to_bytes().as_usize(),
            0
        );
        bitmap_address
    }

    // Optimization: Returns true if the object transitioned from unmarked to marked.
    pub unsafe fn mark_object(&mut self, object: *mut Obj) -> bool {
        let address = object as usize;
        let partition_index = address / PARTITION_SIZE;
        let partition = self.get_partition(partition_index);
        if partition.has_large_content() {
            return self.mark_large_object(object);
        }
        let bitmap = partition.get_bitmap();
        let offset = address % PARTITION_SIZE;
        if bitmap.is_marked(offset) {
            return false;
        }
        bitmap.mark(offset);
        partition
            .set_marked_size(partition.marked_size() + block_size(address).to_bytes().as_usize());
        true
    }

    #[cfg(debug_assertions)]
    pub unsafe fn is_object_marked(&self, object: *mut Obj) -> bool {
        let address = object as usize;
        let partition_index = address / PARTITION_SIZE;
        let partition = self.get_partition(partition_index);
        if partition.has_large_content() {
            return self.is_large_object_marked(object);
        }
        let bitmap = partition.get_bitmap();
        let offset = address % PARTITION_SIZE;
        bitmap.is_marked(offset)
    }

    pub unsafe fn start_collection<M: Memory>(&mut self, mem: &mut M, time: &mut BoundedTime) {
        self.check_occupied_size();
        debug_assert_eq!(self.bitmap_allocation_pointer, NO_BITMAP_ALLOCATION_POINTER);
        debug_assert!(!self.gc_running);
        self.gc_running = true;
        for partition in self.partitions() {
            if partition.has_dynamic_space() && !partition.has_large_content() {
                let bitmap_address = self.allocate_bitmap(mem);
                partition.get_bitmap().assign(bitmap_address);
                time.advance(Bytes(BITMAP_SIZE as u32).to_words().as_usize());
            }
        }
    }

    pub fn plan_evacuations(&mut self) {
        debug_assert_eq!(
            self.partitions()
                .filter(|partition| partition.is_free())
                .count(),
            self.free_partitions
        );
        // Do not use all free partitions for evacuation.
        // Leave a reserve for mutator allocations during a GC run.
        const EVACUATION_FRACTION: usize = 2;
        let reserved_partitions =
            (self.free_partitions + EVACUATION_FRACTION - 1) / EVACUATION_FRACTION;
        let mut evacuation_space = reserved_partitions * PARTITION_SIZE;
        // To avoid sorting a dynamic number of partitions, evacuations are planned
        // in fixed classes of relative occupied space (survival rate classes).
        // First, partitions with 15% or less occupied space are selected for evacuation.
        // Second, partitions with 35% or less occupied space are selected for evacuation.
        // And so on, until the survival rate threshold of 85% has been reached.
        // This approach is less precise than exact sorting, but it does not require
        // a dynamic array allocation which is difficult during garbage collection.
        let mut survival_rate = 0.15;
        while survival_rate <= SURVIVAL_RATE_THRESHOLD
            && self.select_evacuations(survival_rate, &mut evacuation_space)
        {
            survival_rate += 0.2;
        }
    }

    /// Returns true if more partitions could be selected for evacuations.
    fn select_evacuations(&mut self, survival_rate: f64, evacuation_space: &mut usize) -> bool {
        for partition in self.partitions() {
            if partition.index != self.allocation_index
                && partition.is_evacuation_candidate(survival_rate)
            {
                if *evacuation_space < partition.marked_size() {
                    // Limit the evacuations to the available free space for the current GC run.
                    return false;
                }
                *evacuation_space -= partition.marked_size();
                partition.set_to_be_evacuated(true);
                self.evacuating = true;
                debug_assert_eq!(self.evacuated_size, 0);
            }
        }
        true
    }

    pub fn plan_updates(&mut self) {
        for partition in self.partitions() {
            debug_assert!(!partition.to_be_updated());
            partition.set_to_be_updated(!partition.is_free() && !partition.to_be_evacuated());
        }
    }

    pub unsafe fn complete_collection(&mut self) {
        for partition in self.partitions() {
            let marked_size = partition.marked_size();
            partition.set_to_be_updated(false);
            partition.set_marked_size(0);
            partition.get_bitmap().release();
            if partition.to_be_evacuated() {
                debug_assert_ne!(partition.index, self.allocation_index);
                debug_assert!(partition.dynamic_size() >= marked_size);
                self.reclaimed += (partition.dynamic_size() - marked_size) as u64;
            }
            if partition.to_be_evacuated() || partition.is_temporary() {
                self.precomputed_heap_size -= partition.dynamic_size();
                partition.reclaim();
                self.free_partitions += 1;
            }
        }
        self.evacuating = false;
        self.evacuated_size = 0;
        self.bitmap_allocation_pointer = NO_BITMAP_ALLOCATION_POINTER;
        debug_assert!(self.gc_running);
        self.gc_running = false;
        self.check_occupied_size();
    }

    pub fn updates_needed(&self) -> bool {
        self.evacuating
    }

    fn allocation_partition(&self) -> Partition {
        self.get_partition(self.allocation_index)
    }

    pub fn is_allocation_partition(&self, index: usize) -> bool {
        self.allocation_index == index
    }

    fn find_free_partition<M: Memory>(&mut self, mem: &mut M, requested_space: usize) -> Partition {
        for partition in self.partitions() {
            if partition.is_free() && partition.free_size() >= requested_space {
                return partition;
            }
        }
        self.extend_memory(mem)
    }

    unsafe fn allocate_free_partition<M: Memory>(
        &mut self,
        mem: &mut M,
        requested_space: usize,
    ) -> Partition {
        let bitmap_address = if self.gc_running {
            self.allocate_bitmap(mem)
        } else {
            null_mut()
        };
        let partition = self.find_free_partition(mem, requested_space);
        println!(
            100,
            "ALLOCATE FREE PARTITION {} {}", partition.index, self.gc_running
        );
        debug_assert_eq!(partition.dynamic_size(), 0);
        partition.set_free(false);
        debug_assert!(self.free_partitions > 0);
        self.free_partitions -= 1;
        if bitmap_address != null_mut() {
            partition.get_bitmap().assign(bitmap_address);
        }
        partition
    }

    fn check_occupied_size(&self) {
        debug_assert_eq!(
            self.heap_base
                + self
                    .partitions()
                    .map(|partition| partition.dynamic_size())
                    .sum::<usize>(),
            self.occupied_size().as_usize()
        );
    }

    pub fn occupied_size(&self) -> Bytes<u32> {
        Bytes((self.precomputed_heap_size + self.allocation_partition().dynamic_size()) as u32)
    }

    pub fn reclaimed_size(&self) -> Bytes<u64> {
        Bytes(self.reclaimed)
    }

    pub fn increase_evacuated_size(&mut self, size: Words<u32>) {
        self.evacuated_size += size.to_bytes().as_usize();
    }

    pub fn total_allocated_size(&self) -> Bytes<u64> {
        debug_assert!(self.evacuated_size <= self.occupied_size().as_usize());
        let heap_size_without_evacuations = self.occupied_size().as_usize() - self.evacuated_size;
        Bytes(heap_size_without_evacuations as u64) + self.reclaimed_size()
    }

    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M, words: Words<u32>) -> Value {
        let size = words.to_bytes().as_usize();
        if size <= PARTITION_CAPACITY {
            self.allocate_normal_object(mem, size)
        } else {
            self.allocate_large_object(mem, size)
        }
    }

    unsafe fn allocate_normal_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        debug_assert!(size <= PARTITION_CAPACITY);
        let allocation_partition = self.allocation_partition();
        debug_assert!(!allocation_partition.is_free());
        let heap_pointer = allocation_partition.dynamic_space_end();
        debug_assert!(size <= allocation_partition.end_address());
        if heap_pointer <= allocation_partition.end_address() - size {
            allocation_partition.set_dynamic_size(allocation_partition.dynamic_size() + size);
            Value::from_ptr(heap_pointer)
        } else {
            self.allocate_in_new_partition(mem, size)
        }
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_in_new_partition<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        #[cfg(feature = "memory_check")]
        self.allocation_partition().clear_free_remainder();

        self.precomputed_heap_size += self.allocation_partition().dynamic_size();

        let new_partition = self.allocate_free_partition(mem, size);
        let heap_pointer = new_partition.dynamic_space_end();
        new_partition.set_dynamic_size(new_partition.dynamic_size() + size);
        self.allocation_index = new_partition.index;
        Value::from_ptr(heap_pointer)
    }

    // Significant performance gain by not inlining.
    #[inline(never)]
    unsafe fn allocate_large_object<M: Memory>(&mut self, mem: &mut M, size: usize) -> Value {
        debug_assert!(size > PARTITION_CAPACITY);
        if size > usize::MAX - PARTITION_SIZE {
            panic!("Too large allocation");
        }
        let number_of_partitions = Self::partitions_for_large_size(size);
        debug_assert!(number_of_partitions > 1);

        let partition = self.allocate_large_space(mem, number_of_partitions);
        partition.set_free(false);
        partition.set_large_content(true);
        partition.set_dynamic_size(size);

        println!(
            100,
            "ALLOCATE LARGE PARTITION {} {}", partition.index, number_of_partitions
        );

        debug_assert!(self.free_partitions >= number_of_partitions);
        self.free_partitions -= number_of_partitions;
        self.precomputed_heap_size += size;

        Value::from_ptr(partition.dynamic_space_start())
    }

    unsafe fn allocate_large_space<M: Memory>(
        &mut self,
        mem: &mut M,
        number_of_partitions: usize,
    ) -> Partition {
        let mut start_of_free_range = 0;
        for partition in self.partitions() {
            println!(
                100,
                " CHECK LARGE {} {} {}",
                partition.index,
                partition.is_free(),
                self.free_partitions
            );
            // Invariant: [start_of_free_range .. partition.index) contains only free partitions.
            if partition.is_completely_free() {
                if partition.index + 1 - start_of_free_range >= number_of_partitions {
                    return self.get_partition(start_of_free_range);
                }
            } else {
                start_of_free_range = partition.next_partition_index();
            }
        }
        let current_length = self.number_of_partitions() - start_of_free_range;
        println!(
            100,
            " CURRENT LARGE {current_length} {} {}",
            self.number_of_partitions(),
            start_of_free_range
        );

        let missing_partitions = number_of_partitions - current_length;
        println!(
            100,
            " EXTEND LARGE {start_of_free_range} {missing_partitions} {}", self.free_partitions
        );
        for _ in 0..missing_partitions {
            self.extend_memory(mem);
        }
        self.get_partition(start_of_free_range)
    }

    fn partition_of(&self, large_object: *mut Obj) -> Partition {
        debug_assert_eq!(
            large_object as usize % PARTITION_SIZE,
            PARTITION_DESCRIPTOR_SIZE
        );
        let start_partition = large_object as usize / PARTITION_SIZE;
        self.get_partition(start_partition)
    }

    fn partitions_for_large_size(object_size: usize) -> usize {
        debug_assert!(object_size > PARTITION_CAPACITY);
        (object_size + PARTITION_DESCRIPTOR_SIZE + PARTITION_SIZE - 1) / PARTITION_SIZE
    }

    fn consecutive_partitions(large_object: *mut Obj) -> usize {
        let size = unsafe { block_size(large_object as usize).to_bytes().as_usize() };
        Self::partitions_for_large_size(size)
    }

    pub unsafe fn collect_large_objects(&mut self) {
        for partition in self.partitions() {
            if partition.has_large_content() {
                debug_assert!(!partition.is_free());
                let object = partition.dynamic_space_start() as *mut Obj;
                if partition.marked_size() == 0 {
                    self.free_large_object(object);
                }
            }
        }
    }

    unsafe fn free_large_object(&mut self, object: *mut Obj) {
        let first_partition = self.partition_of(object);
        debug_assert!(first_partition.has_large_content());
        let consecutive_partitions = Self::consecutive_partitions(object);
        println!(
            100,
            "FREE PARTITIONS {} {}", first_partition.index, consecutive_partitions
        );
        let size = first_partition.dynamic_size();
        debug_assert!(size > PARTITION_CAPACITY);
        first_partition.set_to_be_updated(false);
        first_partition.reclaim();

        let overflow_start = first_partition.index + 1;
        let overflow_end = first_partition.index + consecutive_partitions;
        for overflow_index in overflow_start..overflow_end {
            let overflow_partition = self.get_partition(overflow_index);
            overflow_partition.initialize(PartitionDescriptor::new(true));
        }

        self.reclaimed += size as u64;
        self.precomputed_heap_size -= size;
        self.free_partitions += consecutive_partitions;
    }

    // Significant performance gain by not inlining.
    // Optimization: Returns true if it has not yet been marked before.
    #[inline(never)]
    unsafe fn mark_large_object(&mut self, object: *mut Obj) -> bool {
        let object_size = block_size(object as usize).to_bytes().as_usize();
        let partition = self.partition_of(object);
        debug_assert!(partition.has_large_content());
        debug_assert_eq!(partition.dynamic_size(), object_size);
        if partition.marked_size() > 0 {
            return false;
        }
        partition.set_marked_size(object_size);
        true
    }

    #[cfg(debug_assertions)]
    unsafe fn is_large_object_marked(&self, object: *mut Obj) -> bool {
        let partition = self.partition_of(object);
        partition.marked_size() > 0
    }
}
