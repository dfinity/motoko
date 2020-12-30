use crate::alloc::alloc_words;
use crate::mem::memzero;
use crate::types::Words;

/// Initial bitmap size
const INITIAL_BITMAP_SIZE: Words<u32> = Words(100);

/// Current size of the bitmap
static mut BITMAP_SIZE: Words<u32> = Words(0);

/// Current bitmap
static mut BITMAP_PTR: *mut u8 = core::ptr::null_mut();

pub unsafe fn alloc_bitmap() {
    let ptr = alloc_words(INITIAL_BITMAP_SIZE).unskew();
    memzero(ptr, INITIAL_BITMAP_SIZE);
    BITMAP_SIZE = INITIAL_BITMAP_SIZE;
    BITMAP_PTR = ptr as *mut u8;
}

pub unsafe fn free_bitmap() {
    BITMAP_SIZE = Words(0);
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
