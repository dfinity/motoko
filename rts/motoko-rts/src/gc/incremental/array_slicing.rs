use crate::types::*;

/// Slice an array during GC.
/// Returns the actual slice length.
/// This helps:
/// * Ensure bounded increments when visiting fields on large arrays.
/// * Prevent mark stack overflows on large arrays.
pub unsafe fn slice_array(array: *mut Array) -> u32 {
    const SLICE_INCREMENT: u32 = 128;
    debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
    let tag = (*array).header.tag;
    let slice_start = if tag >= TAG_ARRAY_SLICE_MIN { tag } else { 0 };
    if array.len() - slice_start > SLICE_INCREMENT {
        let new_start = slice_start + SLICE_INCREMENT;
        (*array).header.tag = new_start;
        new_start
    } else {
        (*array).header.tag = TAG_ARRAY;
        array.len()
    }
}
