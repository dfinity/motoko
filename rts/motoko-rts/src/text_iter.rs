//! Text iterators need to point to a specific position in the tree
//!
//! This is currently a simple triple (arraY):
//!
//! 1. a pointer to a current leaf (must be a BLOB)
//! 2. Position in that blob (shifted by two for GC's sake)
//! 3. 0, or a pointer to a linked list of non-empty text values to do next
//!
//! The linked list (text_cont_t) is a tuple (array) with
//! 1. a pointer to the text
//! 2. 0, or a pointer to the next list entry

use crate::alloc::alloc_words;
use crate::rts_trap_with;
use crate::types::{size_of, Array, SkewedPtr, Words, TAG_ARRAY, TAG_BLOB, TAG_CONCAT};

const TODO_TEXT_IDX: u32 = 0;
const TODO_LINK_IDX: u32 = 1;

/// Find the left-most leaf of a text, putting all the others onto a list. Used to enforce the
/// invariant about TEXT_ITER_BLOB to be a blob.
#[no_mangle]
unsafe extern "C" fn find_leaf(mut text: SkewedPtr, todo: *mut SkewedPtr) -> SkewedPtr {
    while text.tag() == TAG_CONCAT {
        let concat = text.as_concat();

        // Add right node to TODOs
        let c = alloc_words(size_of::<Array>() + Words(2));
        let c_array = c.unskew() as *mut Array;
        (*c_array).header.tag = TAG_ARRAY;
        (*c_array).len = 2;
        c_array.set(TODO_TEXT_IDX, (*concat).text2);
        c_array.set(TODO_LINK_IDX, *todo);
        *todo = c;

        // Follow left node
        text = (*concat).text1;
    }

    debug_assert_eq!(text.tag(), TAG_BLOB);
    text
}

const ITER_BLOB_IDX: u32 = 0;
const ITER_POS_IDX: u32 = 1;
const ITER_TODO_IDX: u32 = 2;

/// Returns a new iterator for the text
#[no_mangle]
unsafe extern "C" fn text_iter(text: SkewedPtr) -> SkewedPtr {
    let iter = alloc_words(size_of::<Array>() + Words(3));
    let array = iter.unskew() as *mut Array;
    (*array).header.tag = TAG_ARRAY;
    (*array).len = 3;

    // Initialize the TODO field first, to be able to use it use the location to `find_leaf`
    let todo_addr = array.payload_addr().add(ITER_TODO_IDX as usize) as *mut _;
    *todo_addr = SkewedPtr(0);

    // Initialize position field
    array.set(ITER_POS_IDX, SkewedPtr(0));

    // Initialize blob field
    array.set(ITER_BLOB_IDX, find_leaf(text, todo_addr as *mut _));

    iter
}

/// Returns whether the iterator is finished
#[no_mangle]
unsafe extern "C" fn text_iter_done(iter: SkewedPtr) -> u32 {
    let array = iter.as_array();
    let pos = array.get(ITER_POS_IDX).0 >> 2;
    let blob = array.get(ITER_BLOB_IDX).as_blob();
    let todo = array.get(ITER_TODO_IDX);

    if pos >= blob.len().0 as usize && todo == SkewedPtr(0) {
        1
    } else {
        0
    }
}

extern "C" {
    fn decode_code_point(s: *const u8, n: *mut u32) -> u32;
}

/*
/// Returns next character in the iterator, advances the iterator
#[no_mangle]
unsafe extern "C" fn text_iter_next(iter: SkewedPtr) -> u32 {
    let iter_array = iter.as_array();

    let pos = (iter_array.get(ITER_POS_IDX).0 >> 2) as u32;
    let blob = iter_array.get(ITER_BLOB_IDX).as_blob();

    // If we are at the end of the current blob, find the next blob
    if pos >= blob.len().0 {
        let todo = iter_array.get(ITER_TODO_IDX);

        if todo == SkewedPtr(0) {
            // TODO (osa): Are we assuming the caller should've checked with `text_iter_done`?
            rts_trap_with("text_iter_next: Iter already done\0".as_ptr());
        }

        let todo_array = todo.as_array();

        let next = iter_array.get(TODO_LINK_IDX);

        if next.tag() == TAG_CONCAT {
            // If next one is a concat node re-use both the iterator and the todo objects (avoids
            // allocation)
            let concat = next.as_concat();
            todo_array.set(TODO_TEXT_IDX, (*concat).text2);
            iter_array.set(ITER_POS_IDX, SkewedPtr(0));
            let todo_addr = iter_array.payload_addr().add(ITER_TODO_IDX as usize);
            iter_array.set(ITER_BLOB_IDX, find_leaf((*concat).text1, todo_addr));
            text_iter_next(iter)
        } else {
            // Otherwise remove the entry from the chain
            iter_array.set(ITER_BLOB_IDX, next);
            iter_array.set(ITER_POS_IDX, SkewedPtr(0));
            iter_array.set(ITER_TODO_IDX, iter_array.get(TODO_LINK_IDX));
            text_iter_next(iter)
        }
    } else {
        // We are not at the end, read the next character from the blob
        let blob_payload = blob.payload_addr();
        let mut step: u32 = 0;
        let char = decode_code_point(blob_payload.add(pos as usize), &mut step as *mut u32);
        iter_array.set(ITER_POS_IDX, SkewedPtr((pos + step << 2) as usize));
        char
    }
}
*/
