//! A stack for marking heap objects (for GC). There should be no allocation after the stack
//! otherwise things will break as we push. This invariant is checked in debug builds.

use crate::memory::{alloc_blob, Memory};
use crate::types::{Blob, Tag, Words};

use core::ptr::null_mut;

/// Initial stack size
pub const INIT_STACK_SIZE: Words<u32> = Words(64);

/// Pointer to the `blob` object for the mark stack. Used to get the capacity of the stack.
static mut STACK_BLOB_PTR: *mut Blob = null_mut();

/// Bottom of the mark stack
pub static mut STACK_BASE: *mut usize = null_mut();

/// Top of the mark stack
pub static mut STACK_TOP: *mut usize = null_mut();

/// Next free slot in the mark stack
pub static mut STACK_PTR: *mut usize = null_mut();

pub unsafe fn alloc_mark_stack<M: Memory>(mem: &mut M) {
    debug_assert!(STACK_BLOB_PTR.is_null());

    // Allocating an actual object here to not break dump_heap
    STACK_BLOB_PTR = alloc_blob(mem, INIT_STACK_SIZE.to_bytes()).get_ptr() as *mut Blob;
    STACK_BASE = STACK_BLOB_PTR.payload_addr() as *mut usize;
    STACK_PTR = STACK_BASE;
    STACK_TOP = STACK_BASE.add(INIT_STACK_SIZE.as_usize());
}

pub unsafe fn free_mark_stack() {
    STACK_BLOB_PTR = null_mut();
    STACK_BASE = null_mut();
    STACK_PTR = null_mut();
    STACK_TOP = null_mut();
}

/// Doubles the stack size
pub unsafe fn grow_stack<M: Memory>(mem: &mut M) {
    let stack_cap: Words<u32> = STACK_BLOB_PTR.len().to_words();
    let p = mem.alloc_words(stack_cap).get_ptr() as *mut usize;

    // Make sure nothing was allocated after the stack
    debug_assert_eq!(STACK_TOP, p);

    let new_cap: Words<u32> = stack_cap * 2;
    (*STACK_BLOB_PTR).len = new_cap.to_bytes();
    STACK_TOP = STACK_BASE.add(new_cap.as_usize());
}

pub unsafe fn push_mark_stack<M: Memory>(mem: &mut M, obj: usize, obj_tag: Tag) {
    debug_assert!(is_ptr(obj));
    // We add 2 words in a push, and `STACK_PTR` and `STACK_TOP` are both multiples of 2, so we can
    // do simple equality check here
    if STACK_PTR == STACK_TOP {
        grow_stack(mem);
    }

    *STACK_PTR = obj;
    *(STACK_PTR.add(1)) = obj_tag as usize;
    STACK_PTR = STACK_PTR.add(2);
}

fn is_ptr(p: usize) -> bool {
    p & 1 == 0
}

pub unsafe fn push_range_mark_stack<M: Memory>(mem: &mut M, ptr: *const u32, start: usize) {
    debug_assert!(is_ptr(ptr as usize));
    if STACK_PTR == STACK_TOP {
        grow_stack(mem);
    }

    *STACK_PTR = ptr as usize + 1;
    *(STACK_PTR.add(1)) = start;
    STACK_PTR = STACK_PTR.add(2);
}

pub unsafe fn pop_mark_stack(heap_base: usize) -> Option<(usize, Tag)> {
    loop {
        if STACK_PTR == STACK_BASE {
            return None;
        }
        STACK_PTR = STACK_PTR.sub(2);
        let p = *STACK_PTR;
        let mut tag = *STACK_PTR.add(1);
        if is_ptr(p) {
            return Some((p, tag as u32));
        } else {
            use crate::types::{Obj, Value, TAG_NULL, TAG_OBJECT};
            let p = p - 1;
            // we pop from a range
            let len = *(p as *const u32);
            let before_field = (p as *mut Value).add(tag);
            let obj = (*before_field.add(1)).get_raw();
            let remembered_mark = obj & 3 == 1;
            tag += 1; // increment start_index
            if len > tag as u32 {
                *STACK_PTR.add(1) = tag;
                STACK_PTR = STACK_PTR.add(2)
            }
            let field_addr = before_field.add(1);
            if remembered_mark {
                // undo remember
                let obj = obj + 2;
                *field_addr = Value::from_raw(obj);
                // perform the threading anyway
                if (*field_addr).get_ptr() <= obj as usize {
                    crate::gc::mark_compact::thread(field_addr);
                }
                continue;
            }
            // check for dynamic heap
            if crate::visitor::pointer_to_dynamic_heap(field_addr, heap_base) {
                let obj = (*field_addr).as_obj();
                // `obj.tag` will be overwritten
                let mut obj_tag = (*obj).tag;
                while obj_tag & 1 == 0 {
                    // intervening threading may have happened, so chase the real tag
                    obj_tag = (*(obj_tag as *const Obj)).tag
                }
                // the end of the chain is the original header for the object
                debug_assert!(obj_tag >= TAG_OBJECT && obj_tag <= TAG_NULL);

                // perform the threading
                if (*field_addr).get_ptr() <= obj as usize {
                    crate::gc::mark_compact::thread(field_addr);
                }
                return Some((obj as usize, obj_tag));
            }
        }
    }
}
