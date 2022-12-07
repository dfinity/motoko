//! Segregated free list used by the incremental garbage collector.
//! Will be later replaced by moving evacuating collection.
//!
//! Constant amount of free lists, each with a defined size class.
//!
//! Memory representation per free list.
//! Doubly linking for fast list removal when merging free neighours.
//!
//! first ──> ┌────────┬────────┬────────────────┐
//!           │ header │  next  |     (free)     |
//!           └────────┴────────┴────────────────┘
//!
//! The header encodes the block size with a special added tag:
//!   `TAG_FREE_BLOCK_MIN + word_size`.
//! Free blocks must be unmarked.
//!
//! The smallest free block size is 2 words (8 bytes), equal to the minimum
//! object size.
//!
//! * `header` is useful for the sweep phase to identify old free blocks.
//!    i.e. blocks already freed by a previous GC increment.
//!    The header also encodes the block size (TAG_FREE_BLOCK_MIN + size).
//!    This helps to determine the remainder that is split off on allocation.
//!    Moreover, the size information supports heap traversal during the GC
//!    sweep phase.
//! * `next` denotes the next block in the corresponding free list.
//!    This supports fast removal of the first list item on allocation.
//!
//! (A previous pointer would allow fast random access removal of free blocks
//! during merging. However, the GC rebuilds the free lists on sweep to save
//! the previous pointer and allow a minimum free block size of 8 bytes.)
//!
//! Allocation first consults the free list of the next higher size class.
//! E.g. `allocate(100)` looks in the free list of size class [KB, 32 * MB].
//! *  If the free list is non-empty, the first block is a match and returned.
//! *  The remainder is cut off and replaced into the corresponding free list.
//! *  Small remainders (less than smallest size class, i.e. 4 bytes) remain
//!    unused and the unused words are identified by `TAG_ONE_WORD_FILLER`.
//! *  If a free list is empty, allocation advances to the next larger list
//!    until reaching the last list.
//! *  The last list constitutes an overflow list for large memory blocks
//!    and requires linear search.
//! *  If no more free memory, try to reserve more free memory space by
//!    extending the heap.
//!
//! The sweep phase rebuilds the free lists and merges free neighbor blocks.
//!
//! Performance per block operation (`n` = number of blocks):
//! *  Allocation by the mutator: `O(1)` unless overflow list `O(n)`.
//! *  Free by the GC: `O(1)`, merging neighbor free space and inserting it
//!    to the free list (cleared on sweep start).
//!
//! Fragmentation:
//! *  External fragmentation: Free space can be fragmented in many small
//!    blocks that are each insufficient for a larger allocation.
//!    Free neighbor merging helps to reduce external fragmentation.
//! *  Internal fragmentation: Useless small 4 byte remainders can be split
//!    off from allocated free blocks that are less than minimum free block
//!    size. Such remainders are identified by `TAG_ONE_WORD_FILLER`.
//!
//! The GC mark phase never visits free blocks.
//! The GC sweep phase rebuilds the free lists and merges free neighbors.
//!

use core::{array::from_fn, ptr::null_mut};

use crate::{
    constants::WORD_SIZE,
    memory::Memory,
    types::{
        is_marked, size_of, unmark, Bytes, Obj, Value, Words, TAG_FREE_BLOCK_MIN,
        TAG_ONE_WORD_FILLER,
    },
};

/// Free block. Must have a size >= `min_size()`.
#[repr(C)] // See note in `types.rs`
pub struct FreeBlock {
    /// Object tag and size, encoded as `TAG_FREE_BLOCK_MIN + size`. Mark bit never set.
    /// The size includes the free block header, i.e. denotes the entire block size.
    pub header: Obj,
    /// Next free block in the corresponding free list. `null_mut()` if last block.
    pub next: *mut FreeBlock,
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

    pub fn clear(&mut self) {
        self.first = null_mut();
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
        (*block).next = self.first;
        self.first = block;
        debug_assert!(self.fits(block));
    }

    // Return the first free block from the non-empty list.
    pub unsafe fn remove_first(&mut self) -> *mut FreeBlock {
        debug_assert_ne!(self.first, null_mut());
        let block = self.first;
        self.first = (*block).next;
        #[cfg(debug_assertions)]
        {
            (*block).next = null_mut();
        }
        block
    }

    // Linear search is only used for overflow list. Returns null if no fit.
    pub unsafe fn remove_first_fit(&mut self, size: Bytes<u32>) -> *mut FreeBlock {
        debug_assert!(self.is_overflow_list());
        let mut previous: *mut FreeBlock = null_mut();
        let mut block = self.first;
        while block != null_mut() && block.size() < size {
            previous = block;
            block = (*block).next;
        }
        if block != null_mut() {
            if block == self.first {
                debug_assert_eq!(previous, null_mut());
                self.first = (*block).next;
            } else {
                debug_assert_ne!(previous, null_mut());
                (*previous).next = (*block).next;
            }
        }
        #[cfg(debug_assertions)]
        {
            (*block).next = null_mut();
        }
        block
    }
}

const KB: usize = 1024;
const MB: usize = 1024 * KB;
const LIST_COUNT: usize = 4;
const SIZE_CLASSES: [usize; LIST_COUNT] = [8, 32, KB, 32 * MB];

pub struct SegregatedFreeList {
    lists: [FreeList; LIST_COUNT],
    total_size: Bytes<u32>,
}

impl SegregatedFreeList {
    pub fn new() -> SegregatedFreeList {
        Self::new_specific(&SIZE_CLASSES)
    }

    pub fn new_specific(size_classes: &[usize]) -> SegregatedFreeList {
        let lists = from_fn(|index| Self::free_list(size_classes, index));
        debug_assert!(lists[lists.len() - 1].is_overflow_list());
        SegregatedFreeList {
            lists,
            total_size: Bytes(0),
        }
    }

    pub fn total_size(&self) -> Bytes<u32> {
        self.total_size
    }

    /// Clear on sweep start. Recollect and merge free space as new blocks.
    pub fn clear(&mut self) {
        for list in self.lists.iter_mut() {
            list.clear();
        }
        self.total_size = Bytes(0);
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
        let mut index = 0;
        while size.as_usize() > self.lists[index].size_class().lower()
            && index < self.lists.len() - 1
        {
            index += 1;
        }
        while self.lists[index].is_empty() && index < self.lists.len() - 1 {
            index += 1;
        }
        &mut self.lists[index]
    }

    fn insertion_list(&mut self, size: Bytes<u32>) -> &mut FreeList {
        let mut index = self.lists.len() - 1;
        while index > 0 && size.as_usize() < self.lists[index].size_class().lower() {
            index -= 1;
        }
        debug_assert!(self.lists[index].size_class().includes(size.as_usize()));
        &mut self.lists[index]
    }

    /// Returned memory chunk has no header and exactly fits the size.
    pub unsafe fn allocate<M: Memory>(&mut self, mem: &mut M, size: Bytes<u32>) -> Value {
        let mut block: *mut FreeBlock = null_mut();
        if size <= self.total_size() {
            let list = self.allocation_list(size);
            if list.is_overflow_list() {
                block = list.remove_first_fit(size);
            } else {
                debug_assert!(size.as_usize() <= list.size_class.lower());
                block = list.remove_first();
            };
        }
        if block == null_mut() {
            mem.grow_heap(size.to_words())
        } else {
            self.total_size -= block.size();
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
    }

    /// Initialize free space and return as a free block if large enough.
    pub unsafe fn create_free_space(start_address: usize, length: Bytes<u32>) -> *mut FreeBlock {
        if length >= FreeBlock::min_size() {
            let block = start_address as *mut FreeBlock;
            block.initialize(length);
            block
        } else {
            FreeBlock::write_free_filler(start_address, length);
            null_mut()
        }
    }

    /// Add allocation remainder or merged free block during GC sweep phase.
    pub unsafe fn add_block(&mut self, block: *mut FreeBlock) {
        debug_assert_ne!(block, null_mut());
        let list = self.insertion_list(block.size());
        list.insert(block);
        self.total_size += block.size();
    }

    #[cfg(debug_assertions)]
    pub unsafe fn sanity_check(&self) {
        let mut total_size = Bytes(0);
        for list in &self.lists {
            let mut block = list.first;
            while block != null_mut() {
                assert!(list.fits(block));
                total_size += block.size();
                block = (*block).next;
            }
        }
        assert_eq!(self.total_size, total_size);
    }
}
