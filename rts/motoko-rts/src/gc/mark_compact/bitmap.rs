use crate::mem_utils::memzero;
use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Bytes, Obj};

/// Current bitmap
static mut BITMAP_PTR: *mut u8 = core::ptr::null_mut();

pub unsafe fn alloc_bitmap<M: Memory>(mem: &mut M, heap_size: Bytes<u32>) {
    // We will have at most this many objects in the heap, each requiring a bit
    let n_bits = heap_size.to_words().as_u32();
    // Each byte will hold 8 bits.
    let bitmap_bytes = (n_bits + 7) / 8;
    // Also round allocation up to 8-bytes to make iteration efficient. We want to be able to read
    // 64 bits in a single read and check as many bits as possible with a single `word != 0`.
    let bitmap_bytes = Bytes(((bitmap_bytes + 7) / 8) * 8);
    // Allocating an actual object here as otherwise dump_heap gets confused
    let blob = alloc_blob(mem, bitmap_bytes).get_ptr() as *mut Blob;
    memzero(blob.payload_addr() as usize, bitmap_bytes.to_words());

    BITMAP_PTR = blob.payload_addr()
}

pub unsafe fn free_bitmap() {
    BITMAP_PTR = core::ptr::null_mut();
}

/// Move the bitmap base such that accessing `idx`
/// will touch the beginning of the bitmap.
pub unsafe fn translate_bitmap(idx: u32) {
    let byte_idx = idx / 8;
    BITMAP_PTR = BITMAP_PTR.sub(byte_idx as usize)
}

pub unsafe fn get_bit(idx: u32) -> bool {
    let (byte_idx, bit_idx) = (idx / 8, idx % 8);
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    (byte >> bit_idx) & 0b1 == 0b1
}

pub unsafe fn set_bit(idx: u32) {
    let (byte_idx, bit_idx) = (idx / 8, idx % 8);
    let byte = *BITMAP_PTR.add(byte_idx as usize);
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
    let blob_len_bytes = (BITMAP_PTR.sub(size_of::<Blob>().to_bytes().as_usize()) as *mut Obj)
        .as_blob()
        .len()
        .as_u32();

    debug_assert_eq!(blob_len_bytes % 8, 0);

    let blob_len_64bit_words = blob_len_bytes / 8;

    let current_word = if blob_len_64bit_words == 0 {
        0
    } else {
        *(BITMAP_PTR as *const u64)
    };

    BitmapIter {
        size: blob_len_64bit_words,
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
