//! Text iterators need to point to a specific position in the tree
//!
//! This is currently a simple triple (array):
//!
//! 1. A pointer to a leaf (must be a BLOB)
//! 2. Position in that blob (shifted by two for GC's sake)
//! 3. 0, or a pointer to a linked list of non-empty text values to do next
//!
//! The linked list is a tuple (array) with
//! 1. A pointer to the text
//! 2. 0, or a pointer to the next list entry

use crate::memory::{alloc_array, Memory};
use crate::rts_trap_with;
use crate::text::decode_code_point;
use crate::types::{Value, TAG_BLOB, TAG_CONCAT};

use motoko_rts_macros::ic_mem_fn;

const TODO_TEXT_IDX: u32 = 0;
const TODO_LINK_IDX: u32 = 1;

/// Find the left-most leaf of a text, putting all the others onto a list. Used to enforce the
/// invariant about TEXT_ITER_BLOB to be a blob.
unsafe fn find_leaf<M: Memory>(mem: &mut M, mut text: Value, todo: *mut Value) -> Value {
    while text.tag() == TAG_CONCAT {
        let concat = text.as_concat();

        // Add right node to TODOs
        let new_todo = alloc_array(mem, 2);
        let new_todo_array = new_todo.as_array();
        new_todo_array.set(TODO_TEXT_IDX, (*concat).text2);
        new_todo_array.set(TODO_LINK_IDX, *todo);
        *todo = new_todo;

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
#[ic_mem_fn]
pub unsafe fn text_iter<M: Memory>(mem: &mut M, text: Value) -> Value {
    let iter = alloc_array(mem, 3);
    let array = iter.as_array();

    // Initialize the TODO field first, to be able to use it use the location to `find_leaf`
    let todo_addr = array.payload_addr().add(ITER_TODO_IDX as usize) as *mut _;
    *todo_addr = Value::from_scalar(0);

    // Initialize position field
    array.set(ITER_POS_IDX, Value::from_scalar(0));

    // Initialize blob field
    array.set(ITER_BLOB_IDX, find_leaf(mem, text, todo_addr as *mut _));

    iter
}

/// Returns whether the iterator is finished
#[no_mangle]
pub unsafe extern "C" fn text_iter_done(iter: Value) -> u32 {
    let array = iter.as_array();
    let pos = array.get(ITER_POS_IDX).get_scalar() >> 1;
    let blob = array.get(ITER_BLOB_IDX).as_blob();
    let todo = array.get(ITER_TODO_IDX);

    if pos >= blob.len().0 && todo.get_scalar() == 0 {
        1
    } else {
        0
    }
}

/// Returns next character in the iterator, advances the iterator
#[ic_mem_fn]
pub unsafe fn text_iter_next<M: Memory>(mem: &mut M, iter: Value) -> u32 {
    let iter_array = iter.as_array();

    let blob = iter_array.get(ITER_BLOB_IDX).as_blob();
    let pos = iter_array.get(ITER_POS_IDX).get_scalar() >> 1;

    // If we are at the end of the current blob, find the next blob
    if pos >= blob.len().0 {
        let todo = iter_array.get(ITER_TODO_IDX);

        if todo.get_scalar() == 0 {
            // Caller should check with text_iter_done
            rts_trap_with("text_iter_next: Iter already done");
        }

        let todo_array = todo.as_array();

        let text = todo_array.get(TODO_TEXT_IDX);

        if text.tag() == TAG_CONCAT {
            // If next one is a concat node re-use both the iterator and the todo objects (avoids
            // allocation)
            let concat = text.as_concat();
            todo_array.set(TODO_TEXT_IDX, (*concat).text2);
            iter_array.set(ITER_POS_IDX, Value::from_scalar(0));
            let todo_addr = iter_array.payload_addr().add(ITER_TODO_IDX as usize);
            iter_array.set(ITER_BLOB_IDX, find_leaf(mem, (*concat).text1, todo_addr));
            text_iter_next(mem, iter)
        } else {
            // Otherwise remove the entry from the chain
            debug_assert_eq!(text.tag(), TAG_BLOB);
            iter_array.set(ITER_BLOB_IDX, text);
            iter_array.set(ITER_POS_IDX, Value::from_scalar(0));
            iter_array.set(ITER_TODO_IDX, todo_array.get(TODO_LINK_IDX));
            text_iter_next(mem, iter)
        }
    } else {
        // We are not at the end, read the next character from the blob
        let blob_payload = blob.payload_addr();
        let mut step: u32 = 0;
        let char = decode_code_point(blob_payload.add(pos as usize), &mut step as *mut u32);
        iter_array.set(ITER_POS_IDX, Value::from_scalar((pos + step) << 1));
        char
    }
}
