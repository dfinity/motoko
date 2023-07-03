use crate::constants::WORD_SIZE;
use crate::mem_utils::memzero;
use crate::memory::{alloc_blob, Memory};
use crate::types::{size_of, Blob, Bytes, Obj};

/* How the Wasm-heap maps to the bitmap

  +---- RTS stack ----+---- Motoko statics ----+---- Dynamic heap ------+ Heap limit
                  (prefix words)                   bitmap lives here -> | BM |
                                                                       /      \
                                                                      /        \
                                       each bit represents a word -> /...bits...\
                                          in the dynamic heap

## Marking with absolute addresses

When marking, we need to map an address to a bit in the bitmap. Internally the
bitmap is kept in a (non-moving) blob after the dynamic heap (DH). To
efficiently mark the right bit in the bitmap, we maintain a pointer that points
_before the start of the bitmap_ such that `address / u8::BITS` and `address % u8::BITS`
will address the right bit:


       +---- BITMAP_FORBIDDEN_PTR              +---- BITMAP_PTR
       v                                       v
       ,          (forbidden)                  ,...................... bitmap ..........~~....,
       |   heap_prefix_words / u8::BITS bytes  |          heap_size / usize::BITS bytes       |
       |   get_bitmap_forbidden_size()         |                   BITMAP_SIZE                |
       ^                                       ^                                              ^
      /           ^^^^^^^^^^^                  \                                             |
     /            corresponds                   \                 ^^^^^^^^^^^               /
    /                                            \                corresponds              /
   /                                              \                                       /
  +---- Rts stack ----+---- Motoko statics --------+--------- Dynamic heap ----------~~--+
                                                   !
                                                   ! aligned to `usize::BITS` bytes


Debug assertions guard the forbidden bytes from access, as this area potentially
overlaps with the Motoko dynamic heap.

## The alignment caveat

For this scheme to work, it is essential that the start of the DH is an address that
is divisible by `usize::BITS` (`heap_prefix_words % u8::BITS == 0`). Otherwise the `address / u8::BITS`
and `address % u8::BITS` operations on the DH's starting address will not yield the
least significant bit in the BM.

## Example calculation

Assume the DH is at 0x80000. Assume heap limit being at 0xB0000. Then the BM thus
could be placed at 0xB0008. Since the heap_prefix_words is 0x20000,
BITMAP_FORBIDDEN_PTR = 0xB0008 - 0x20000 / 8 = 0xAC008.

Now let's mark the address 0x80548 in the DH. Its absolute word number is 0x20152.
The `(0x20152 / 8, 0x20152 % 8)`-rule gives a bit position 2 with byte offset 0x402A,
thus we mark bit 2 in byte 0xAC004 + 0x402A = 0xB002E, which is in the BM.

 */

/// Current bitmap
static mut BITMAP_FORBIDDEN_PTR: *mut u8 = core::ptr::null_mut();
static mut BITMAP_PTR: *mut u8 = core::ptr::null_mut();
static mut BITMAP_SIZE: usize = 0;

const BITS_PER_BYTE: usize = u8::BITS as usize;

unsafe fn get_bitmap_forbidden_size() -> usize {
    BITMAP_PTR as usize - BITMAP_FORBIDDEN_PTR as usize
}

pub unsafe fn alloc_bitmap<M: Memory>(
    mem: &mut M,
    heap_size: Bytes<usize>,
    heap_prefix_words: usize,
) {
    // See Note "How the Wasm-heap maps to the bitmap" above
    debug_assert_eq!(heap_prefix_words % BITS_PER_BYTE, 0);
    // We will have at most this many objects in the heap, each requiring a bit
    let n_bits = heap_size.to_words().as_usize();
    BITMAP_SIZE = (n_bits + (BITS_PER_BYTE - 1)) / BITS_PER_BYTE;
    // Also round allocation up to word size to make iteration efficient.
    let bitmap_bytes = Bytes(((BITMAP_SIZE + WORD_SIZE - 1) / WORD_SIZE) * WORD_SIZE);
    // Allocating an actual object here as otherwise dump_heap gets confused
    // No post allocation barrier as this RTS-internal blob will be collected by the GC.
    let blob = alloc_blob(mem, bitmap_bytes).get_ptr() as *mut Blob;
    memzero(blob.payload_addr() as usize, bitmap_bytes.to_words());

    BITMAP_PTR = blob.payload_addr();
    BITMAP_FORBIDDEN_PTR = BITMAP_PTR.sub(heap_prefix_words as usize / BITS_PER_BYTE)
}

pub unsafe fn free_bitmap() {
    BITMAP_PTR = core::ptr::null_mut();
    BITMAP_FORBIDDEN_PTR = core::ptr::null_mut();
    BITMAP_SIZE = 0;
}

pub unsafe fn get_bit(idx: usize) -> bool {
    let (byte_idx, bit_idx) = (idx / BITS_PER_BYTE, idx % BITS_PER_BYTE);
    debug_assert!(byte_idx as usize >= get_bitmap_forbidden_size());
    debug_assert!(get_bitmap_forbidden_size() + BITMAP_SIZE > byte_idx);
    let byte = *BITMAP_FORBIDDEN_PTR.add(byte_idx);
    (byte >> bit_idx) & 0b1 != 0
}

pub unsafe fn set_bit(idx: usize) {
    let (byte_idx, bit_idx) = (idx / BITS_PER_BYTE, idx % BITS_PER_BYTE);
    debug_assert!(byte_idx as usize >= get_bitmap_forbidden_size());
    debug_assert!(get_bitmap_forbidden_size() + BITMAP_SIZE > byte_idx);
    let byte = *BITMAP_FORBIDDEN_PTR.add(byte_idx);
    let new_byte = byte | (0b1 << bit_idx);
    *BITMAP_FORBIDDEN_PTR.add(byte_idx) = new_byte;
}

pub struct BitmapIter {
    /// Size of the bitmap, in bits. Does not change after initialization.
    size: usize,
    /// Current bit index
    current_bit_idx: usize,
    /// Current word in the bitmap that we're iterating.
    current_word: usize,
    /// How many leading bits are initially zeroed in the current_word
    leading_zeros: usize,
}

pub unsafe fn iter_bits() -> BitmapIter {
    let blob_len_bytes = (BITMAP_PTR.sub(size_of::<Blob>().to_bytes().as_usize()) as *mut Obj)
        .as_blob()
        .len()
        .as_usize();

    debug_assert_eq!(blob_len_bytes % size_of::<usize>().to_bytes().as_usize(), 0);

    let blob_len_words = blob_len_bytes / size_of::<usize>().to_bytes().as_usize();

    let current_word = if blob_len_words == 0 {
        0
    } else {
        let bitmap_ptr = BITMAP_PTR as *const usize;
        *bitmap_ptr
    };

    debug_assert!(BITMAP_PTR as usize >= BITMAP_FORBIDDEN_PTR as usize);
    let forbidden_bits = get_bitmap_forbidden_size() * BITS_PER_BYTE;

    BitmapIter {
        size: blob_len_bytes * BITS_PER_BYTE + forbidden_bits,
        current_bit_idx: forbidden_bits,
        current_word,
        leading_zeros: current_word.leading_zeros() as usize,
    }
}

/// This value marks the end-of-stream in `BitmapIter`. Using this value instead of `None` for
/// end-of-stream reduces Wasm instructions executed by ~2.7% in some cases.
//
// Heap is 4GiB and each word gets a bit, so this is one larger than the bit for the last
// word in heap.
//
// (We actually need less bits than that as when the heap is full we can't allocate bitmap and mark
// stack and can't do GC)
pub const BITMAP_ITER_END: usize = usize::MAX;

impl BitmapIter {
    /// Returns the next bit, or `BITMAP_ITER_END` if there are no more bits set.
    pub fn next(&mut self) -> usize {
        debug_assert!(self.current_bit_idx <= self.size);

        if self.current_bit_idx == self.size {
            return BITMAP_ITER_END;
        }

        // Outer loop iterates over the words
        loop {
            // Examine the least significant bit(s) in the current word
            if self.current_word != 0 {
                let shift_amt = self.current_word.trailing_zeros() as usize;
                self.current_word >>= shift_amt;
                self.current_word >>= 1;
                let bit_idx = self.current_bit_idx + shift_amt;
                self.current_bit_idx = bit_idx + 1;
                return bit_idx;
            }

            // Move on to next word
            self.current_bit_idx += self.leading_zeros;
            unsafe {
                debug_assert_eq!(
                    (self.current_bit_idx - get_bitmap_forbidden_size() * BITS_PER_BYTE)
                        % usize::BITS as usize,
                    0
                )
            }
            if self.current_bit_idx == self.size {
                return BITMAP_ITER_END;
            }
            self.current_word = unsafe {
                let ptr = BITMAP_FORBIDDEN_PTR.add(self.current_bit_idx as usize / BITS_PER_BYTE)
                    as *const usize;
                *ptr
            };
            self.leading_zeros = self.current_word.leading_zeros() as usize;
        }
    }
}
