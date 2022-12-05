//! Segregated free list used by the incremental garbage collector.
//! Will be later replaced by moving evacuating collection.
//!
//! Constant amount of free lists, each with a defined size class.
//!
//! Memory representation per free list.
//! Doubly linking for fast list removal when merging free neighours.
//!
//! first ──> ┌────────┬────────┬──────────┬───────────────┐
//!           │ header │  next  | previous |    (free)     |
//!           └────────┴────────┴──────────┴───────────────┘
//!
//! The header encodes the block size with a special added tag:
//!   `TAG_FREE_BLOCK_MIN + word_size`.
//! Free blocks must be unmarked.
//!
//! The smallest free block size is 3 words (12 bytes).
//!
//! * `header` is useful for the sweep phase to identify old free blocks.
//!    i.e. blocks already freed by a previous GC increment.
//!    The header also encodes the block size (TAG_FREE_BLOCK_MIN + size).
//!    This helps to determine the remainder that is split off on allocation.
//!    Moreover, the size information supports heap traversal during the GC
//!    sweep phase.
//! * `next` denotes the next block in the corresponding free list.
//!    This supports fast removal of the first list item on allocation.
//! * `previous` denotes the previous block in the corresponding free list
//!    This enables fast free neighbor block merging.
//!
//! Allocation first consults the free list of the next higher size class.
//! E.g. `allocate(100)` looks in the free list of size class [128, 256].
//! *  If the free list is non-empty, the first block is a match and returned.
//! *  The remainder is cut off and replaced into the corresponding free list.
//! *  Small remainders (less than smallest size class) remain unused
//!    and each unused word is identified by the tag `TAG_ONE_WORD_FILLER`.
//! *  If a free list is empty, allocation advances to the next larger list
//!    until reaching the last list.
//! *  The last list constitutes an overflow list for large memory blocks
//!    and requires linear search.
//! *  If no more free memory, try to reserve more free WASM space.
//!
//! Deallocation determines the free list with the size class that includes
//! the freed block size.
//! *  Free neighbors are merged by first removing the neighbor from its list.
//!
//! Performance per block operation (`n` = number of blocks):
//! *  Allocation by the mutator: `O(1)` unless overflow list `O(n)`.
//! *  Free by the GC: `O(1)`, including merging.
//!
//! Fragmentation:
//! *  External fragmentation: Free space can be fragmented in many small
//!    blocks that are each insufficient for a larger allocation.
//!    Free neighbor merging helps to reduce external fragmentation.
//! *  Internal fragmentation: Useless small remainders can be split off
//!    from allocated free blocks that are too small for being a free block.
//!    Such internal fragmentation is identified by `TAG_ONE_WORD_FILLER`.
//!
//! The GC mark phase never visits free blocks.
//! The GC sweep phase retains or merges free blocks.
//!

use core::{array::from_fn, cmp::max, ptr::null_mut};

use crate::{
    constants::WORD_SIZE,
    memory::Memory,
    types::{
        is_marked, object_size, size_of, unmark, Bytes, Obj, Value, Words, TAG_FREE_BLOCK_MIN,
        TAG_ONE_WORD_FILLER,
    },
};

/// Free block. Must have a size >= `min_size()`.
#[repr(C)] // See note in `types.rs`
pub(crate) struct FreeBlock {
    /// Object tag and size, encoded as `TAG_FREE_BLOCK_MIN + size`. Mark bit never set.
    /// The size includes the free block header, i.e. denotes the entire block size.
    pub header: Obj,
    /// Next free block in the corresponding free list. `null_mut()` if last block.
    pub next: *mut FreeBlock,
    /// Prevuous free block in the corresponding free list. `null_mut()` if first block.
    pub previous: *mut FreeBlock,
}

impl FreeBlock {
    /// Minimum block size. Smaller free space is represented by one word fillers.
    /// See `write_free_filler`.
    pub fn min_size() -> Bytes<u32> {
        size_of::<Self>().to_bytes()
    }

    /// Initialize a new free block. Overwriting the existing content.
    /// Zeros block space in debug mode.
    pub unsafe fn initialize(self: *mut Self, size: Bytes<u32>) {
        debug_assert_ne!(self, null_mut());
        debug_assert!(size >= Self::min_size());
        debug_assert_eq!(size.as_u32() % WORD_SIZE, 0);
        let words = size.to_words();
        #[cfg(debug_assertions)]
        crate::mem_utils::memzero(self as usize, words);
        debug_assert!(words.as_u32() <= u32::MAX - TAG_FREE_BLOCK_MIN);
        (*self).header.raw_tag = unmark(TAG_FREE_BLOCK_MIN + words.as_u32());
        debug_assert!(!is_marked((*self).header.raw_tag));
        (*self).next = null_mut();
        (*self).previous = null_mut();
    }

    /// Size of the free entire free block (includes object header)
    pub unsafe fn size(self: *mut Self) -> Bytes<u32> {
        debug_assert_ne!(self, null_mut());
        debug_assert!(!is_marked((*self).header.raw_tag));
        debug_assert!((*self).header.raw_tag >= TAG_FREE_BLOCK_MIN);
        let words = (*self).header.raw_tag - TAG_FREE_BLOCK_MIN;
        Words(words).to_bytes()
    }

    /// Remainder block is split off and returned to the free unless it is too small.
    pub unsafe fn split(self: *mut Self, size: Bytes<u32>) -> *mut FreeBlock {
        debug_assert_ne!(self, null_mut());
        debug_assert_eq!(size.as_u32() % WORD_SIZE, 0);
        debug_assert!(size <= self.size());
        let remainder_address = self as usize + size.as_usize();
        let remainder_size = self.size() - size;
        debug_assert_eq!(remainder_size.as_u32() % WORD_SIZE, 0);
        #[cfg(debug_assertions)]
        crate::mem_utils::memzero(self as usize, size.to_words());
        if remainder_size < Self::min_size() {
            Self::write_free_filler(remainder_address, remainder_size);
            null_mut()
        } else {
            let remainder = remainder_address as *mut FreeBlock;
            remainder.initialize(remainder_size);
            remainder
        }
    }

    /// Unused small free block remainder. Contributes to internal fragmentation.
    pub unsafe fn write_free_filler(start_address: usize, size: Bytes<u32>) {
        debug_assert_eq!(size.as_u32() % WORD_SIZE, 0);
        for word in 0..size.as_u32() / WORD_SIZE {
            let current_address = start_address as u32 + word * WORD_SIZE;
            *(current_address as *mut u32) = TAG_ONE_WORD_FILLER;
        }
    }
}

#[derive(Clone, Copy)]
struct Range {
    lower: usize, // inclusive
    upper: usize, // exclusive
}

impl Range {
    pub fn new(lower: usize, upper: usize) -> Range {
        debug_assert!(lower <= upper);
        Range { lower, upper }
    }

    pub fn lower(&self) -> usize {
        self.lower
    }

    pub fn upper(&self) -> usize {
        self.upper
    }

    pub fn includes(&self, value: usize) -> bool {
        self.lower <= value && value < self.upper
    }
}

struct FreeList {
    size_class: Range,
    first: *mut FreeBlock,
}

impl FreeList {
    pub fn new(size_class: Range) -> FreeList {
        debug_assert!(size_class.lower() >= size_of::<FreeBlock>().to_bytes().as_usize());
        FreeList {
            size_class,
            first: null_mut(),
        }
    }

    unsafe fn fits(&self, block: *mut FreeBlock) -> bool {
        self.size_class.includes(block.size().as_usize())
    }

    pub fn size_class(&self) -> Range {
        self.size_class
    }

    pub fn is_overflow_list(&self) -> bool {
        self.size_class.upper == usize::MAX
    }

    pub fn is_empty(&self) -> bool {
        self.first == null_mut()
    }

    pub unsafe fn insert(&mut self, block: *mut FreeBlock) {
        debug_assert_ne!(block, null_mut());
        debug_assert!(self.fits(block));
        debug_assert_eq!((*block).next, null_mut());
        debug_assert_eq!((*block).previous, null_mut());
        (*block).next = self.first;
        if self.first != null_mut() {
            (*self.first).previous = block;
        }
        self.first = block;
        debug_assert!(self.fits(block));
    }

    pub unsafe fn remove(&mut self, block: *mut FreeBlock) {
        debug_assert_ne!(block, null_mut());
        debug_assert!(self.fits(block));
        let previous = (*block).previous;
        let next = (*block).next;
        if previous != null_mut() {
            (*previous).next = next;
        }
        if next != null_mut() {
            (*next).previous = previous;
        }
        if block == self.first {
            debug_assert_eq!(previous, null_mut());
            self.first = next;
        }
        (*block).next = null_mut();
        (*block).previous = null_mut();
    }

    // Linear search is only used for overflow list.
    pub unsafe fn first_fit(&mut self, size: Bytes<u32>) -> *mut FreeBlock {
        debug_assert!(self.is_overflow_list());
        let mut block = self.first;
        while block != null_mut() && block.size() < size {
            block = (*block).next;
        }
        block
    }
}

const KB: usize = 1024;
const MB: usize = 1024 * KB;
const LIST_COUNT: usize = 16;
const SIZE_CLASSES: [usize; LIST_COUNT] = [
    12,
    32,
    64,
    128,
    256,
    512,
    KB,
    4 * KB,
    16 * KB,
    64 * KB,
    256 * KB,
    MB,
    4 * MB,
    16 * MB,
    64 * MB,
    256 * MB,
];

pub struct SegregatedFreeList {
    lists: [FreeList; LIST_COUNT],
    // Search optimization: Fast forward to the next non-empty list.
    // If all higher lists are empty, forward to the overflow list.
    next_available: [usize; LIST_COUNT],
    total_size: Bytes<u32>,
}

impl SegregatedFreeList {
    pub fn new() -> SegregatedFreeList {
        Self::new_specific(&SIZE_CLASSES)
    }

    pub fn new_specific(size_classes: &[usize]) -> SegregatedFreeList {
        let lists = from_fn(|index| Self::free_list(size_classes, index));
        let next_available = from_fn(|_| lists.len() - 1);
        debug_assert!(lists[lists.len() - 1].is_overflow_list());
        SegregatedFreeList {
            lists,
            next_available,
            total_size: Bytes(0),
        }
    }

    pub fn total_size(&self) -> Bytes<u32> {
        self.total_size
    }

    fn free_list(size_classes: &[usize], index: usize) -> FreeList {
        let lower = size_classes[index];
        let upper = if index + 1 < size_classes.len() {
            size_classes[index + 1]
        } else {
            usize::MAX
        };
        FreeList::new(Range::new(lower, upper))
    }

    fn allocation_list(&mut self, size: Bytes<u32>) -> &mut FreeList {
        let mut index = self.list_index(size);
        let list = &self.lists[index];
        // Round up the size class to guarantee that first fit (except for the overflow list).
        if !list.is_overflow_list() && size.as_usize() > list.size_class().lower() {
            index += 1;
        }
        let available = self.next_available[index];
        debug_assert!(index <= available);
        &mut self.lists[available]
    }

    fn list_index(&mut self, size: Bytes<u32>) -> usize {
        let mut left = 0;
        let mut right = self.lists.len() - 1;
        while left < right {
            let middle = (left + right) / 2;
            if size.as_usize() >= self.lists[middle].size_class().upper() {
                left = middle + 1;
            } else {
                right = middle;
            }
        }
        debug_assert!(left < self.lists.len());
        debug_assert!(
            size < FreeBlock::min_size() && left == 0
                || self.lists[left].size_class().includes(size.as_usize())
        );
        left
    }

    /// Returned memory chunk has no header and can be smaller than the minimum free block size.
    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M, size: Bytes<u32>) -> Value {
        let mut block: *mut FreeBlock = null_mut();
        if size <= self.total_size() {
            let list = self.allocation_list(size);
            debug_assert!(list.is_overflow_list() || size.as_usize() <= list.size_class.lower());
            block = if list.is_overflow_list() {
                list.first_fit(size)
            } else {
                debug_assert!(size.as_usize() <= list.size_class.lower());
                list.first
            };    
        }
        if block == null_mut() {
            block = Self::grow_heap(mem, size);
        } else {
            self.remove_block(block);
        }
        debug_assert!(block != null_mut());
        if block.size() > size {
            let remainder = block.split(size);
            if remainder != null_mut() {
                self.add_block(remainder);
            }
        }
        let address = block as usize;
        #[cfg(debug_assertions)]
        crate::mem_utils::memzero(address, size.to_words());
        #[cfg(debug_assertions)]
        self.sanity_check();
        Value::from_ptr(address)
    }

    /// Merge the free space to one free block (if large enough).
    /// Checks the free space consists of garbage objects and/or old free blocks.
    pub unsafe fn free_space(&mut self, start_address: usize, length: Bytes<u32>) {
        let end_address = start_address + length.as_usize();
        let mut address = start_address;
        while address < end_address {
            let object = address as *mut Obj;
            debug_assert_ne!(object, null_mut());
            debug_assert!(!object.is_marked());
            if object.tag() >= TAG_FREE_BLOCK_MIN {
                let block = object as *mut FreeBlock;
                let block_size = block.size();
                if address == start_address && block_size == length {
                    // Optimization: The free space is exactly one old free block.
                    return;
                }
                self.remove_block(block);
                address += block_size.as_usize();
            } else {
                address += object_size(address).to_bytes().as_usize();
            };
            debug_assert!(address <= end_address);
        }
        if length >= FreeBlock::min_size() {
            let block = start_address as *mut FreeBlock;
            block.initialize(length);
            self.add_block(block)
        } else {
            FreeBlock::write_free_filler(start_address, length);
        }
    }

    unsafe fn remove_block(&mut self, block: *mut FreeBlock) {
        debug_assert_ne!(block, null_mut());
        let removal_index = self.list_index(block.size());
        let list = &mut self.lists[removal_index];
        debug_assert!(list.size_class.includes(block.size().as_usize()));
        list.remove(block);
        self.total_size -= block.size();
        if list.is_empty() && !list.is_overflow_list() {
            let mut index = removal_index;
            let next_index = self.next_available[removal_index + 1];
            debug_assert!(removal_index < next_index);
            while self.next_available[index] == removal_index {
                self.next_available[index] = next_index;
                if index > 0 {
                    index -= 1;
                }
            }
        }
    }

    unsafe fn add_block(&mut self, block: *mut FreeBlock) {
        debug_assert_ne!(block, null_mut());
        let insertion_index = self.list_index(block.size());
        let list = &mut self.lists[insertion_index];
        debug_assert!(list.size_class.includes(block.size().as_usize()));
        list.insert(block);
        self.total_size += block.size();
        let mut index = insertion_index;
        while self.next_available[index] > insertion_index {
            self.next_available[index] = insertion_index;
            if index > 0 {
                index -= 1;
            }
        }
    }

    unsafe fn grow_heap<M: Memory>(mem: &mut M, size: Bytes<u32>) -> *mut FreeBlock {
        let size = max(size, FreeBlock::min_size());
        debug_assert_eq!(size.as_u32() % WORD_SIZE, 0);
        let value = mem.grow_heap(size.to_words());
        let block = value.get_ptr() as *mut FreeBlock;
        block.initialize(size);
        block
    }

    #[cfg(debug_assertions)]
    pub unsafe fn sanity_check(&self) {
        let mut total_size = Bytes(0);
        for list in &self.lists {
            let mut previous: *mut FreeBlock = null_mut();
            let mut block = list.first;
            while block != null_mut() {
                assert!(list.fits(block));
                assert_eq!((*block).previous, previous);
                total_size += block.size();
                previous = block;
                block = (*block).next;
            }
        }
        assert_eq!(self.total_size, total_size);
        self.check_fast_forwarding();
    }

    #[cfg(debug_assertions)]
    fn check_fast_forwarding(&self) {
        let mut index = 0;
        while index < self.lists.len() - 1 {
            let next = self.next_available[index];
            assert!(index <= next);
            for test in index..next {
                assert!(self.lists[test].is_empty());
            }
            assert!(next == self.lists.len() - 1 || !self.lists[next].is_empty());
            assert_eq!(self.next_available[next], next);
            index += next + 1;
        }
        assert_eq!(
            self.next_available[self.lists.len() - 1],
            self.lists.len() - 1
        );
    }
}
