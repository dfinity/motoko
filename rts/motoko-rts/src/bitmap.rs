use crate::alloc::alloc_blob;
use crate::mem::memzero;
use crate::types::{Blob, Bytes, WORD_SIZE};

/// Current bitmap
/// NOTE: This is public for testing purposes, do not read or modify this directly, use the
/// functions below
pub static mut BITMAP_PTR: *mut u8 = core::ptr::null_mut();

#[cfg(feature = "gc")]
pub(crate) unsafe fn alloc_bitmap() {
    // One bit per object. An object can be at most 1 word.
    let heap_size = crate::gc::HP - crate::gc::get_heap_base();
    // We will have at most this many objects in the heap, each requiring a bit
    let max_objects = heap_size / WORD_SIZE;

    let bitmap_bytes = Bytes(max_objects / 8);

    // Allocating an actual object here as otherwise dump_heap gets confused by this stuff
    let ptr = alloc_blob(bitmap_bytes).unskew();
    // alloc_blob already rounds up to words so this is fine
    memzero(ptr, bitmap_bytes.to_words());

    BITMAP_PTR = (ptr as *mut Blob).payload_addr()
}

#[cfg(feature = "gc")]
pub(crate) unsafe fn free_bitmap() {
    BITMAP_PTR = core::ptr::null_mut();
}

pub unsafe fn get_bit(idx: u32) -> bool {
    let byte_idx = (idx + 7) / 8;
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    let bit_idx = idx % 8;
    (byte >> bit_idx) & 0b1 == 0b1
}

pub unsafe fn set_bit(idx: u32) {
    let byte_idx = (idx + 7) / 8;
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    let bit_idx = idx % 8;
    let new_byte = byte | (0b1 << bit_idx);
    *BITMAP_PTR.add(byte_idx as usize) = new_byte;
}

/// Get mark bit of an object, set it if it's not set. More efficient version of
///
/// ````
/// let current_mark = get_bit(x);
/// if !current_mark {
///     set_bit(x);
/// }
/// ```
pub unsafe fn get_and_set_bit(idx: u32) -> bool {
    let byte_idx = (idx + 7) / 8;
    let byte = *BITMAP_PTR.add(byte_idx as usize);
    let bit_idx = idx % 8;
    // Get current mark
    let marked = (byte >> bit_idx) & 0b1 == 0b1;
    // Mark it
    let new_byte = byte | (0b1 << bit_idx);
    *BITMAP_PTR.add(byte_idx as usize) = new_byte;
    // Return old mark
    marked
}
