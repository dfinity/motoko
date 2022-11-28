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
//! Free blocks are never visited by the GC mark phase.
//!

use core::{array::from_fn, cmp::max, ptr::null_mut};

use crate::{
    constants::WORD_SIZE,
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
        assert_ne!(self, null_mut());
        assert!(size >= Self::min_size());
        assert_eq!(size.as_u32() % WORD_SIZE, 0);
        let words = size.to_words();
        #[cfg(debug_assertions)]
        crate::mem_utils::memzero(self as usize, words);
        assert!(words.as_u32() <= u32::MAX - TAG_FREE_BLOCK_MIN);
        (*self).header.raw_tag = unmark(TAG_FREE_BLOCK_MIN + words.as_u32());
        assert!(!is_marked((*self).header.raw_tag));
        (*self).next = null_mut();
        (*self).previous = null_mut();
    }

    /// Size of the free entire free block (includes object header)
    pub unsafe fn size(self: *mut Self) -> Bytes<u32> {
        assert_ne!(self, null_mut());
        assert!(!is_marked((*self).header.raw_tag));
        assert!((*self).header.raw_tag >= TAG_FREE_BLOCK_MIN);
        let words = (*self).header.raw_tag - TAG_FREE_BLOCK_MIN;
        Words(words).to_bytes()
    }

    /// Remainder block is split off and returned to the free unless it is too small.
    pub unsafe fn split(self: *mut Self, size: Bytes<u32>) -> *mut FreeBlock {
        assert_ne!(self, null_mut());
        assert_eq!(size.as_u32() % WORD_SIZE, 0);
        assert!(size <= self.size());
        let remainder_address = self as usize + size.as_usize();
        let remainder_size = self.size() - size;
        assert_eq!(remainder_size.as_u32() % WORD_SIZE, 0);
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
        assert_eq!(size.as_u32() % WORD_SIZE, 0);
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
        assert!(lower <= upper);
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
        assert!(size_class.lower() >= size_of::<FreeBlock>().to_bytes().as_usize());
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
        assert_ne!(block, null_mut());
        assert!(self.fits(block));
        assert_eq!((*block).next, null_mut());
        assert_eq!((*block).previous, null_mut());
        (*block).next = self.first;
        if self.first != null_mut() {
            (*self.first).previous = block;
        }
        self.first = block;
        assert!(self.fits(block));
    }

    /// returns null if empty
    pub unsafe fn remove_first(&mut self) -> *mut FreeBlock {
        let block = self.first;
        if block != null_mut() {
            assert!(self.fits(block));
            assert!((*block).previous == null_mut());
            let next = (*block).next;
            if next != null_mut() {
                (*next).previous = null_mut();
            }
            self.first = next;
            (*block).next = null_mut();
        }
        assert!(self.fits(block));
        block
    }

    pub unsafe fn remove(&mut self, block: *mut FreeBlock) {
        assert_ne!(block, null_mut());
        assert!(self.fits(block));
        let previous = (*block).previous;
        let next = (*block).next;
        if previous != null_mut() {
            (*previous).next = next;
        }
        if next != null_mut() {
            (*next).previous = previous;
        }
        if block == self.first {
            assert_eq!(previous, null_mut());
            self.first = next;
        }
        (*block).next = null_mut();
        (*block).previous = null_mut();
    }

    // Linear search is only used for overflow list.
    pub unsafe fn remove_first_fit(&mut self, size: Bytes<u32>) -> *mut FreeBlock {
        assert!(self.is_overflow_list());
        let mut block = self.first;
        while block != null_mut() && block.size() < size {
            block = (*block).next;
        }
        if block != null_mut() {
            self.remove(block);
        }
        block
    }
}

pub trait Heap {
    unsafe fn grow_heap(&mut self, size: Words<u32>) -> Value;
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
}

impl SegregatedFreeList {
    pub fn new() -> SegregatedFreeList {
        Self::new_specific(&SIZE_CLASSES)
    }

    pub fn new_specific(size_classes: &[usize]) -> SegregatedFreeList {
        let lists = from_fn(|index| Self::free_list(size_classes, index));
        assert!(lists[lists.len() - 1].is_overflow_list());
        SegregatedFreeList { lists }
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
        for index in 0..self.lists.len() {
            let list = &self.lists[index];
            if size.as_usize() <= list.size_class().lower() && !list.is_empty() {
                return &mut self.lists[index];
            }
        }
        return &mut self.lists[self.lists.len() - 1]; // overflow list
    }

    fn insertion_list(&mut self, size: Bytes<u32>) -> &mut FreeList {
        for index in 0..self.lists.len() {
            let list = &self.lists[index];
            if list.size_class().includes(size.as_usize()) {
                return &mut self.lists[index];
            }
        }
        panic!("No matching free list");
    }

    /// Returned memory chunk has no header and can be smaller than the minimum free block size.
    pub unsafe fn allocate<H: Heap>(&mut self, heap: &mut H, size: Bytes<u32>) -> Value {
        let list = self.allocation_list(size);
        assert!(list.is_overflow_list() || size.as_usize() <= list.size_class.lower());
        let mut block = if list.is_overflow_list() {
            list.remove_first_fit(size)
        } else {
            assert!(size.as_usize() <= list.size_class.lower());
            list.remove_first()
        };
        if block == null_mut() {
            block = Self::grow_heap(heap, size);
        }
        assert!(block != null_mut());
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
            assert_ne!(object, null_mut());
            assert!(!object.is_marked());
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
            assert!(address <= end_address);
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
        assert_ne!(block, null_mut());
        let list = self.insertion_list(block.size());
        assert!(list.size_class.includes(block.size().as_usize()));
        list.remove(block);
    }

    unsafe fn add_block(&mut self, block: *mut FreeBlock) {
        assert_ne!(block, null_mut());
        let list = self.insertion_list(block.size());
        assert!(list.size_class.includes(block.size().as_usize()));
        list.insert(block);
    }

    unsafe fn grow_heap<H: Heap>(heap: &mut H, size: Bytes<u32>) -> *mut FreeBlock {
        let size = max(size, FreeBlock::min_size());
        assert_eq!(size.as_u32() % WORD_SIZE, 0);
        let value = heap.grow_heap(size.to_words());
        let block = value.get_ptr() as *mut FreeBlock;
        block.initialize(size);
        block
    }

    #[cfg(debug_assertions)]
    pub unsafe fn sanity_check(&self) {
        for list in &self.lists {
            let mut previous: *mut FreeBlock = null_mut();
            let mut block = list.first;
            while block != null_mut() {
                assert!(list.fits(block));
                assert_eq!((*block).previous, previous);
                previous = block;
                block = (*block).next;
            }
        }
    }
}
