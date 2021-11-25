use crate::mem_utils::memzero;
use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Bytes, Obj};

/* How the Wasm-heap maps to the bitmap

  +---- Rts stack ----+---- Motoko statics ----+---- Dynamic heap --~~--+ Heap end
                  (prefix words)               | BM |   <- bitmap lives here
                                              /      \
                                             /        \
                                            /...bits...\ each bit corresponds to a word
                                                         in the dynamic heap

When marking, we pass an absolute pointer (i.e. address space relative), for speed.
Internally the bitmap is kept in a (non-moving) blob at the start of the dynamic heap (DH).
To efficiently mark the right bit in the bitmap, we maintain a pointer that points
_before the start of the bitmap_ such that using the `/%8`-operation on the DH
absolute word number will address the right bit:

       +---- BITMAP_FORBIDDEN_PTR
       v
       |          (forbidden)             |...................... bitmap ................|
       |   heap_prefix_words / 8 bytes    |                 heap_size / 32 bytes
       |       BITMAP_COMPENSATION        |                     BITMAP_SIZE

Debug assertions guard the forbidden bytes from access, as this area physically overlaps
with the Motoko static heap.

## The alignment caveat

For this scheme to work, it is essential that the start of the DH is an address that
is divisible by 32 (implying that `heap_prefix_words % 8 == 0`). Otherwise the
`/%8`-operation on the DH's starting address will not yield the least significant bit
in the BM, and thus the sweep operation will be off.

## Example calculation

Assume the DH is at 0x80000. The BM thus could be at 0x80004.
Since the heap_prefix_words is 0x20000, BITMAP_FORBIDDEN_PTR = 0x80004 - 0x20000 / 8 = 0x7c004.

Now let's mark the address 0x80548 in the DH. Its absolute word number is 0x20152.
The `(0x20152 / 8, 0x20152 % 8)`-rule gives a bit position 2 with byte offset 0x402a,
thus we mark bit 2 in byte 0x7c004+0x402a = 0x8002e, which is physically in the BM.

 */

/// Current bitmap
static mut BITMAP_FORBIDDEN_PTR: *mut u8 = core::ptr::null_mut();
static mut BITMAP_COMPENSATION: usize = 0;
static mut BITMAP_SIZE: u32 = 0;

#[cfg(debug_assertions)]
fn get_bitmap_forbidden_size() -> usize {
    unsafe { BITMAP_COMPENSATION }
}

pub unsafe fn alloc_bitmap<M: Memory>(mem: &mut M, heap_size: Bytes<u32>, heap_prefix_words: u32) {
    // See Note "How the Wasm-heap maps to the bitmap" above
    debug_assert_eq!(heap_prefix_words % 8, 0);
    // We will have at most this many objects in the heap, each requiring a bit
    let n_bits = heap_size.to_words().as_u32();
    // Each byte will hold 8 bits.
    BITMAP_SIZE = (n_bits + 7) / 8;
    // Also round allocation up to 8-bytes to make iteration efficient. We want to be able to read
    // 64 bits in a single read and check as many bits as possible with a single `word != 0`.
    let bitmap_bytes = Bytes(((BITMAP_SIZE + 7) / 8) * 8);
    // Allocating an actual object here as otherwise dump_heap gets confused
    let blob = alloc_blob(mem, bitmap_bytes).get_ptr() as *mut Blob;
    memzero(blob.payload_addr() as usize, bitmap_bytes.to_words());

    BITMAP_COMPENSATION = (heap_prefix_words / 8) as usize;
    BITMAP_FORBIDDEN_PTR = blob.payload_addr().sub(BITMAP_COMPENSATION)
}

pub unsafe fn free_bitmap() {
    BITMAP_FORBIDDEN_PTR = core::ptr::null_mut();
    BITMAP_SIZE = 0;
    BITMAP_COMPENSATION = 0
}

pub unsafe fn get_bit(idx: u32) -> bool {
    let (byte_idx, bit_idx) = (idx / 8, idx % 8);
    #[cfg(debug_assertions)]
    debug_assert!(byte_idx as usize >= get_bitmap_forbidden_size());
    #[cfg(debug_assertions)]
    debug_assert!(get_bitmap_forbidden_size() + BITMAP_SIZE as usize > byte_idx as usize);
    let byte = *BITMAP_FORBIDDEN_PTR.add(byte_idx as usize);
    (byte >> bit_idx) & 0b1 != 0
}

pub unsafe fn set_bit(idx: u32) {
    let (byte_idx, bit_idx) = (idx / 8, idx % 8);
    #[cfg(debug_assertions)]
    debug_assert!(byte_idx as usize >= get_bitmap_forbidden_size());
    #[cfg(debug_assertions)]
    debug_assert!(get_bitmap_forbidden_size() + BITMAP_SIZE as usize > byte_idx as usize);
    let byte = *BITMAP_FORBIDDEN_PTR.add(byte_idx as usize);
    let new_byte = byte | (0b1 << bit_idx);
    *BITMAP_FORBIDDEN_PTR.add(byte_idx as usize) = new_byte;
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
    let bitmap = BITMAP_FORBIDDEN_PTR.add(BITMAP_COMPENSATION);
    let blob_len_bytes = (bitmap.sub(size_of::<Blob>().to_bytes().as_usize()) as *mut Obj)
        .as_blob()
        .len()
        .as_u32();

    debug_assert_eq!(blob_len_bytes % 8, 0);

    let blob_len_64bit_words = blob_len_bytes / 8;

    let current_word = if blob_len_64bit_words == 0 {
        0
    } else {
        *(bitmap as *const u64)
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
        let bitmap = unsafe { BITMAP_FORBIDDEN_PTR.add(BITMAP_COMPENSATION) };

        // Outer loop iterates 64-bit words
        loop {
            if self.current_word == 0 && self.current_word_idx == self.size {
                return BITMAP_ITER_END;
            }

            // Inner loop iterates bits in the current word
            while self.current_word != 0 {
                if self.current_word & 0b1 != 0 {
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
                unsafe { *(bitmap as *const u64).add(self.current_word_idx as usize) };
            self.bits_left = 64;
        }
    }
}
