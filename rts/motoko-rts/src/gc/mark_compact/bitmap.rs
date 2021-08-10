use crate::mem_utils::memzero;
use crate::types::Bytes;
use crate::ALLOC;

use core::alloc::{GlobalAlloc, Layout};
use core::convert::TryFrom;

/// Current bitmap
static mut BITMAP_PTR: *mut u8 = core::ptr::null_mut();

/// Layout of the memory pointed by `BITMAP_PTR`. Used to get bitmap size and for deallocating.
static mut BITMAP_LAYOUT: Layout = unsafe { Layout::from_size_align_unchecked(0, 1) };

pub unsafe fn alloc_bitmap(heap_size: Bytes<u32>) {
    // We will have at most this many objects in the heap, each requiring a bit
    let n_bits = heap_size.to_words().0;

    // Each byte will hold 8 bits.
    let bitmap_bytes = (n_bits + 7) / 8;

    // Also round allocation up to 8-bytes to make iteration efficient. We want to be able to read
    // 64 bits in a single read and check as many bits as possible with a single `word != 0`.
    let bitmap_bytes = Bytes(((bitmap_bytes + 7) / 8) * 8);

    BITMAP_LAYOUT = Layout::from_size_align_unchecked(bitmap_bytes.as_usize(), 1);
    BITMAP_PTR = ALLOC.alloc(BITMAP_LAYOUT);

    assert!(!BITMAP_PTR.is_null());
}

pub unsafe fn free_bitmap() {
    ALLOC.dealloc(BITMAP_PTR, BITMAP_LAYOUT);
    // TODO: How is `null_mut` compiled to Wasm? 0 is a valid address and won't cause a trap
    BITMAP_PTR = core::ptr::null_mut();
}

pub unsafe fn get_bit(idx: u32) -> bool {
    let byte_idx = idx / 8;
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    let bit_idx = idx % 8;
    (byte >> bit_idx) & 0b1 == 0b1
}

pub unsafe fn set_bit(idx: u32) {
    let byte_idx = idx / 8;
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    let bit_idx = idx % 8;
    let new_byte = byte | (0b1 << bit_idx);
    *BITMAP_PTR.add(byte_idx as usize) = new_byte;
}

pub struct BitmapIter {
    /// Size of the bitmap, in 64-bit words words. Does not change after initialization.
    size: u32,

    /// Current 64-bit word index
    current_word_idx: u32,

    /// Current 64-bit word in the bitmap that we're iterating. We read in 64-bit chunks to be able
    /// to check as many bits as possible with a single `word != 0`.
    current_word: u64,

    /// Bits left in the current 64-bit word. Used to compute index of a bit in the bitmap. We
    /// can't use a global index here as we don't know how much to bump it when `current_word` is
    /// 0 and we move to the next 64-bit word.
    bits_left: u32,
}

pub unsafe fn iter_bits() -> BitmapIter {
    let bitmap_bytes = BITMAP_LAYOUT.size();

    debug_assert_eq!(bitmap_bytes % 8, 0);

    let blob_len_64bit_words = bitmap_bytes / 8;

    let current_word = if blob_len_64bit_words == 0 {
        0
    } else {
        *(BITMAP_PTR as *const u64)
    };

    // `try_from` below disappears on Wasm as usize and u32 are the same
    BitmapIter {
        size: u32::try_from(blob_len_64bit_words).unwrap(),
        current_word_idx: 0,
        current_word,
        bits_left: 64,
    }
}

/// This value marks the end-of-stream in `BitmapIter`. Using this value instead of `None` for
/// end-of-stream reduces Wasm instructions executed by ~2.7% in some cases.
//
// Heap is 4GiB and each 32-bit word gets a bit, so this is one larger than the bit for the last
// word in heap.
//
// (We actually need less bits than that as when the heap is full we can't allocate bitmap and mark
// stack and can't do GC)
pub const BITMAP_ITER_END: u32 = 1024 * 1024 * 1024;

impl BitmapIter {
    /// Returns the next bit, or `BITMAP_ITER_END` if there are no more bits set.
    pub fn next(&mut self) -> u32 {
        debug_assert!(self.current_word_idx <= self.size);

        // Outer loop iterates 64-bit words
        loop {
            if self.current_word == 0 && self.current_word_idx == self.size {
                return BITMAP_ITER_END;
            }

            // Inner loop iterates bits in the current word
            while self.current_word != 0 {
                if self.current_word & 0b1 == 0b1 {
                    let bit_idx = (self.current_word_idx * 64) + (64 - self.bits_left);
                    self.current_word >>= 1;
                    self.bits_left -= 1;
                    return bit_idx;
                } else {
                    let shift_amt = self.current_word.trailing_zeros();
                    self.current_word >>= shift_amt;
                    self.bits_left -= shift_amt;
                }
            }

            // Move on to next word
            self.current_word_idx += 1;
            if self.current_word_idx == self.size {
                return BITMAP_ITER_END;
            }
            self.current_word =
                unsafe { *(BITMAP_PTR as *const u64).add(self.current_word_idx as usize) };
            self.bits_left = 64;
        }
    }
}
