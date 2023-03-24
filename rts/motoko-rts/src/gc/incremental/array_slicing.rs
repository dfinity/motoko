use crate::types::*;

/// Slice an array during GC.
/// Returns the slice end.
/// This helps:
/// * Ensure bounded increments when visiting fields on large arrays.
/// * Prevent mark stack overflows on large arrays.
pub unsafe fn slice_array(array: *mut Array, slice_start: u32) -> u32 {
    const SLICE_INCREMENT: u32 = 128;
    debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
    if array.len() - slice_start > SLICE_INCREMENT {
        let new_start = slice_start + SLICE_INCREMENT;
        new_start
    } else {
        array.len()
    }
}
