//! Mark bitmap in the heap traversal of the data inspection.
//!
//! The bitmap is stored in a blob and maps the entire heap space.
//!
//! To access a mark bit of an object, the corresponding bit needs to be determined:
//! * Byte index: `(object_address - heap_base) / WORD_SIZE / u8::BITS`
//! * Bit index: `(object_address - heap_base) / WORD_SIZE % u8::BITS`.
//!
//! If an object is marked, the corresponding bit is set to `1`.
//! Otherwise, if the object is not marked or the bit does not denote the start of an
//! object, it is `0`.
//!
//! The mark bitmap memory must be entirely cleared (zeroed) before used.
//!
//! NOTES:
//! * The bitmap can only used within a message as the object addresses and the
//!   heap size change between GC increments.
//! * The bitmap can be reclaimed in the next GC run.

use crate::{
    constants::WORD_SIZE,
    mem_utils::memzero,
    memory::{alloc_blob, Memory},
    types::{Bytes, Value, NULL_POINTER, TAG_BLOB_B},
};

const BITS_PER_BYTE: usize = u8::BITS as usize;
const BITMAP_FRACTION: usize = WORD_SIZE * BITS_PER_BYTE;

#[repr(C)]
pub struct MarkBitmap {
    heap_base: usize,
    blob: Value,
}

impl MarkBitmap {
    pub unsafe fn new<M: Memory>(mem: &mut M, heap_base: usize, heap_end: usize) -> MarkBitmap {
        assert!(heap_base <= heap_end);
        assert_eq!(heap_base % WORD_SIZE, 0);
        let heap_size = heap_end - heap_base;
        let bitmap_size = (heap_size + BITMAP_FRACTION - 1) / BITMAP_FRACTION;
        // No post allocation barrier as the blob is only used temporarily during a message.
        let blob = alloc_blob(mem, TAG_BLOB_B, Bytes(bitmap_size));
        Self::zero_initialize(blob, bitmap_size);
        MarkBitmap {
            heap_base,
            blob: alloc_blob(mem, TAG_BLOB_B, Bytes(bitmap_size)),
        }
    }

    unsafe fn zero_initialize(blob: Value, size: usize) {
        let address = blob.as_blob_mut().payload_addr();
        assert_eq!(size % WORD_SIZE, 0);
        memzero(address as usize, Bytes(size).to_words());
    }

    fn word_index(&self, object_address: usize) -> usize {
        debug_assert_eq!(object_address % WORD_SIZE, 0);
        assert!(object_address >= self.heap_base);
        (object_address - self.heap_base) / WORD_SIZE
    }

    unsafe fn byte_with_bit_index(&self, object: Value) -> (*mut u8, usize) {
        // Resolve the forwarding pointer of the incremental GC.
        let object = object.forward();
        debug_assert_ne!(object, NULL_POINTER);
        let word_index = self.word_index(object.get_ptr());
        let byte_index = word_index / BITS_PER_BYTE;
        let bit_index = word_index % BITS_PER_BYTE;
        let byte = self.blob.as_blob_mut().payload_addr().add(byte_index);
        (byte, bit_index)
    }

    // Check if the objects is marked in the bitmap. Resolves GC forwarding pointer.
    pub unsafe fn is_marked(&self, object: Value) -> bool {
        let (byte, bit_index) = self.byte_with_bit_index(object);
        (*byte >> bit_index) & 0b1 != 0
    }

    // Set the mark bit for the object in the bitmap. Resolves GC forwarding pointer.
    pub unsafe fn mark(&mut self, object: Value) {
        let (byte, bit_index) = self.byte_with_bit_index(object);
        *byte |= 0b1 << bit_index;
    }
}
