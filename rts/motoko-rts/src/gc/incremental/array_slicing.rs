use crate::types::*;

/// Slice an array during GC.
/// Returns the actual slice length.
/// This helps:
/// * Ensure bounded increments when visiting fields on large arrays.
/// * Prevent mark stack overflows on large arrays.
pub unsafe fn slice_array(array: *mut Array) -> usize {
    const SLICE_INCREMENT: usize = 128;
    debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
    let (base_tag, slice_start) = array.get_slice_start();
    if array.len() - slice_start > SLICE_INCREMENT {
        let new_start = slice_start + SLICE_INCREMENT;
        array.set_slice_start(base_tag, new_start);
        new_start
    } else {
        array.restore_tag(base_tag);
        array.len()
    }
}
