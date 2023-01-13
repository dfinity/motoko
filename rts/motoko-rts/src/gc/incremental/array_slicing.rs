use crate::types::*;

/// Slice an array during GC.
/// Preserves the mark flag on the array.
/// Returns the actual slice length.
/// This helps:
/// * Ensure bounded increments when visiting fields on large arrays.
/// * Prevent mark stack overflows on large arrays.
pub unsafe fn slice_array(array: *mut Array) -> u32 {
    let is_marked = array.is_marked();
    const SLICE_INCREMENT: u32 = 128;
    assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
    let tag = array.tag();
    let slice_start = if tag >= TAG_ARRAY_SLICE_MIN { tag } else { 0 };
    if array.len() - slice_start > SLICE_INCREMENT {
        let new_start = slice_start + SLICE_INCREMENT;
        (*array).header.raw_tag = tag_with_mark(new_start, is_marked);
        new_start
    } else {
        (*array).header.raw_tag = tag_with_mark(TAG_ARRAY, is_marked);
        array.len()
    }
}

fn tag_with_mark(tag: Tag, to_be_marked: bool) -> Tag {
    assert!(!is_marked(tag));
    if to_be_marked {
        mark(tag)
    } else {
        tag
    }
}
