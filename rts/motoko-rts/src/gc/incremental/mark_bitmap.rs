//! Mark bitmap in the incremental GC.
//!
//! A separate mark bitmap is associated for each non-free parition that does not host large objects.
//! The bitmaps are allocated in one or multiple separate bitmap partitions, that are freed on completion
//! of a GC run.
//!
//! A bitmap is raw memory (no object header, no blob) of the fixed size `PARTITION_SIZE / WORD_SIZE / u8::bits`,
//! which is `PARTITION_SIZE /32` bytes. For simplicity, the bitmaps is not shortened for partitions that
//! also accomodate static space besides dynamic space.
//!
//! The access a mark bit of an object, the corresponding bitmap and address offset inside the object's partition
//! needs to be first determined. The corresponding bit is then accessed at the byte with index
//! `offset / WORD_SIZE / u8::BITS` and at the bit index `offset % u8::BITS`.
//!
//! If an object is marked, the corresponding bit is set to `1`. Otherwise, if the object is not marked or
//! the bit does not denote the start of an object, it is `0`.
//!
//! The mark bitmap must be entirely cleared on allocation at the beginning of each mark phase.
//!
//! The mark bitmap serves for fast traversal of marked objects in a partition with few marked objects
//! (and many garbage objects).

use core::mem::size_of;

use crate::{constants::WORD_SIZE, mem_utils::memzero, types::Bytes};

use super::partitioned_heap::PARTITION_SIZE;

const BITMAP_FRACTION: usize = (WORD_SIZE * u8::BITS) as usize;

pub const BITMAP_SIZE: usize = PARTITION_SIZE / BITMAP_FRACTION;

/// Partition-associated mark bitmap.
pub struct MarkBitmap {
    pointer: *mut u8,
}

impl MarkBitmap {
    /// Allocate and initializes the bitmap at the defined memory address.
    pub unsafe fn allocate(bitmap_address: usize) -> MarkBitmap {
        let mut bitmap = MarkBitmap {
            pointer: bitmap_address as *mut u8,
        };
        bitmap.clear();
        bitmap
    }

    unsafe fn clear(&mut self) {
        memzero(self.pointer as usize, Bytes(BITMAP_SIZE as u32).to_words());
    }

    fn word_index(&self, offset_in_partition: usize) -> usize {
        debug_assert_eq!(offset_in_partition % WORD_SIZE as usize, 0);
        debug_assert!(offset_in_partition < PARTITION_SIZE);
        offset_in_partition / (WORD_SIZE as usize)
    }

    /// Check whether the object at defined address offset in the partition is marked.
    pub unsafe fn is_marked(&self, offset_in_partition: usize) -> bool {
        let word_index = self.word_index(offset_in_partition);
        let byte_index = word_index / u8::BITS as usize;
        let bit_index = word_index % u8::BITS as usize;
        let byte = self.pointer.add(byte_index);
        (*byte >> bit_index) & 0b1 != 0
    }

    /// Mark an object at the defined address offset in the partition.
    pub unsafe fn mark(&mut self, offset_in_partition: usize) {
        let word_index = self.word_index(offset_in_partition);
        let byte_index = word_index / u8::BITS as usize;
        let bit_index = word_index % u8::BITS as usize;
        let byte = self.pointer.add(byte_index);
        *byte |= 0b1 << bit_index;
    }

    /// Obtain a new iterator for the bitmap.
    pub fn iterate(&self) -> BitmapIterator {
        BitmapIterator::new(self.pointer)
    }
}

/// Adopted and adjusted from `mark_compact/bitmap.rs`.
/// The iterator separates advancing `next()` from inspection `current_marked_offset()`
/// to better support the incremental evacuation and update GC increments.
pub struct BitmapIterator {
    /// Start address of the mark bitmap.
    bitmap_pointer: *mut u8,
    /// Index of next bit to continue iteration in the bitmap.
    next_bit_index: usize,
    /// Current 64-bit word in the bitmap that we are iterating.
    /// Optimization: Reading in 64-bit chunks to check as many bits as
    /// possible with a single `word != 0`.
    current_word: u64,
    /// Number of leading bits that are initially zeroed in `current_word`.
    leading_zeros: usize,
}

/// End-of-iteration sentinel, faster than using an `Option` with `None`.
/// `usize::MAX` is not word-aligned and thus not a valid object address.
pub const BITMAP_ITERATION_END: usize = usize::MAX;

const BIT_INDEX_END: usize = BITMAP_SIZE * u8::BITS as usize;

impl BitmapIterator {
    fn new(bitmap_pointer: *mut u8) -> BitmapIterator {
        debug_assert_eq!(PARTITION_SIZE % size_of::<u64>(), 0);
        debug_assert_eq!(bitmap_pointer as usize % size_of::<u64>(), 0);
        let mut iterator = BitmapIterator {
            bitmap_pointer,
            next_bit_index: 0,
            current_word: 0,
            leading_zeros: 0,
        };
        // Move to first marked bit or to the bitmap end if no bit is set.
        iterator.next();
        iterator
    }

    pub fn belongs_to(&self, bitmap: &MarkBitmap) -> bool {
        self.bitmap_pointer == bitmap.pointer
    }

    /// Returns the next marked address offset in the partition,
    /// or `BITMAP_ITERATION_END` if there are no more bits set.
    pub fn current_marked_offset(&self) -> usize {
        assert!(self.next_bit_index > 0);
        if self.next_bit_index == BIT_INDEX_END {
            return BITMAP_ITERATION_END;
        } else {
            (self.next_bit_index - 1) * WORD_SIZE as usize
        }
    }

    /// Advance the iterator to the next marked offset.
    pub fn next(&mut self) {
        debug_assert!(self.next_bit_index <= BIT_INDEX_END);

        if self.next_bit_index == BIT_INDEX_END {
            return;
        }

        // Outer loop iterates the 64-bit words.
        loop {
            // Examine the least significant bit(s) in the current word.
            if self.current_word != 0 {
                let shift = self.current_word.trailing_zeros() as usize;
                self.current_word >>= shift;
                self.current_word >>= 1;
                let bit_index = self.next_bit_index + shift;
                self.next_bit_index = bit_index + 1;
                return;
            }

            // Move on to next word, always within a 64-bit boundary.
            self.next_bit_index += self.leading_zeros;
            if self.next_bit_index == BIT_INDEX_END {
                return;
            }
            let word64_index = self.next_bit_index / size_of::<u64>();
            self.current_word = unsafe { *(self.bitmap_pointer.add(word64_index) as *const u64) };
            self.leading_zeros = self.current_word.leading_zeros() as usize;
        }
    }
}
