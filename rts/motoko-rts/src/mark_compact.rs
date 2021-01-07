//! Implements "threaded compaction" as described in The Garbage Collection Handbook section 3.3.

use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, iter_bits, set_bit};
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::mem::memcpy_words;
use crate::rts_trap_with;
use crate::types::*;

#[no_mangle]
pub(crate) unsafe extern "C" fn mark_compact(
    heap_base: u32,
    heap_end: u32,
    static_roots: SkewedPtr,
    closure_table_loc: *mut SkewedPtr,
) {
    let heap_size = Bytes(heap_end - heap_base);

    alloc_bitmap(heap_size);
    alloc_mark_stack();

    mark_static_roots(static_roots, heap_base);

    // TODO: We could skip the is_tagged_scalar, heap_base etc. checks
    push_mark_stack(*closure_table_loc, heap_base);

    mark_stack(heap_base);

    thread_roots(static_roots, heap_base);
    thread(closure_table_loc, heap_base);
    update_fwd_refs(heap_base);
    update_bwd_refs(heap_base);

    free_bitmap();
    free_mark_stack();
}

unsafe fn mark_static_roots(static_roots: SkewedPtr, heap_base: u32) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).unskew() as *mut Obj;
        mark_fields(obj, heap_base);
    }
}

unsafe fn push_mark_stack(obj: SkewedPtr, heap_base: u32) {
    if obj.is_tagged_scalar() {
        // Not a boxed object, skip
        return;
    }

    let obj = obj.unskew() as u32;

    if obj < heap_base {
        // Static objects are not collected and not marked
        return;
    }

    let obj_idx = (obj - heap_base) / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    mark_stack::push_mark_stack(obj as usize);
}

unsafe fn mark_stack(heap_base: u32) {
    while let Some(obj) = pop_mark_stack() {
        mark_fields(obj as *mut Obj, heap_base);
    }
}

unsafe fn mark_fields(obj: *mut Obj, heap_base: u32) {
    match obj.tag() {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            for i in 0..obj.size() {
                push_mark_stack(obj.get(i), heap_base);
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            for i in 0..array.len() {
                push_mark_stack(array.get(i), heap_base);
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            push_mark_stack((*mutbox).field, heap_base);
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            for i in 0..closure.size() {
                push_mark_stack(closure.get(i), heap_base);
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            push_mark_stack((*some).field, heap_base);
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            push_mark_stack((*variant).field, heap_base);
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let blob_ptr = *bigint.blob_field();
            assert_eq!((blob_ptr.unskew() as *mut Obj).tag(), TAG_BLOB);
            push_mark_stack(blob_ptr, heap_base);
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            push_mark_stack(concat.text1(), heap_base);
            push_mark_stack(concat.text2(), heap_base);
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            push_mark_stack((*obj_ind).field, heap_base);
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't include pointers, skip
        }

        TAG_NULL => {
            rts_trap_with("encountered NULL object tag in dynamic object in mark_fields");
        }

        TAG_FWD_PTR | _ => {
            // Any other tag is a bug
            // println!(
            //     500,
            //     "invalid object tag {} at {:#x}",
            //     obj.tag(),
            //     obj as usize
            // );
            rts_trap_with("invalid object tag in mark_fields");
        }
    }
}

unsafe fn thread_roots(static_roots: SkewedPtr, heap_base: u32) {
    // Static roots
    let root_array = static_roots.as_array();
    for i in 0..root_array.len() {
        thread_obj_fields(root_array.get(i).unskew() as *mut Obj, heap_base);
    }
    // No need to thread closure table here as it's on heap and we already marked it
}

/// Scan the heap, update forward references. At the end of this pass all fields will be threaded
/// and forward references will be updated, pointing to the object's new location.
unsafe fn update_fwd_refs(heap_base: u32) {
    let mut free = heap_base;

    for bit in iter_bits() {
        let p = (heap_base + (bit * WORD_SIZE)) as *mut Obj;

        // Update forward references to the object to the object's new location and restore
        // object header
        unthread(p, free);

        // Thread fields
        thread_obj_fields(p, heap_base);

        free += object_size(p as usize).to_bytes().0;
    }
}

/// Expects all fields to be threaded. Updates backward references and moves objects to their new
/// locations.
unsafe fn update_bwd_refs(heap_base: u32) {
    let mut free = heap_base;

    for bit in iter_bits() {
        let p = (heap_base + (bit * WORD_SIZE)) as *mut Obj;

        // Update backward references to the object's new location and restore object header
        unthread(p, free);

        // All references to the object now point to the new location, move the object
        let p_size_words = object_size(p as usize);
        memcpy_words(free as usize, p as usize, p_size_words);

        free += p_size_words.to_bytes().0;
    }

    crate::gc::HP = free;
}

unsafe fn thread_obj_fields(obj: *mut Obj, heap_base: u32) {
    match obj.tag() {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.payload_addr();
            for i in 0..obj.size() {
                thread(obj_payload.add(i as usize), heap_base);
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            let array_payload = array.payload_addr();
            for i in 0..array.len() {
                thread(array_payload.add(i as usize), heap_base);
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            thread(field_addr, heap_base);
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.payload_addr();
            for i in 0..closure.size() {
                thread(closure_payload.add(i as usize), heap_base);
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = &mut (*some).field;
            thread(field_addr, heap_base);
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = &mut (*variant).field;
            thread(field_addr, heap_base);
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let field_addr = bigint.blob_field();
            thread(field_addr, heap_base);
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = &mut (*concat).text1;
            thread(field1_addr, heap_base);
            let field2_addr = &mut (*concat).text2;
            thread(field2_addr, heap_base);
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            let field_addr = &mut (*obj_ind).field;
            thread(field_addr, heap_base);
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't have pointers, skip
        }

        TAG_NULL => {
            rts_trap_with("encountered NULL object tag in thread_obj_fields");
        }

        TAG_FWD_PTR | _ => {
            rts_trap_with("invalid object tag in thread_obj_fields");
        }
    }
}

unsafe fn thread(field: *mut SkewedPtr, heap_base: u32) {
    if (*field).is_tagged_scalar() {
        // Field has a scalar value, skip
        return;
    }

    if (*field).unskew() < heap_base as usize {
        // Field doesn't point to heap. The pointee won't be moved so no need to thread the field
        return;
    }

    // Store pointed object's header in the field, field address in the pointed object's header
    let pointed = (*field).unskew() as *mut Obj;
    let pointed_header = pointed.tag();
    *field = SkewedPtr(pointed_header as usize);
    (*pointed).tag = field as u32;
}

/// Unthread all references, replacing with `new_loc`
unsafe fn unthread(obj: *mut Obj, new_loc: u32) {
    // NOTE: For this to work heap addresses need to be greater than the largest value for object
    // headers. Currently this holds. TODO: Document this better.
    let mut header = (*obj).tag;
    while header > TAG_NULL {
        // TODO: is `header > TAG_NULL` the best way to distinguish a tag from a pointer?
        let tmp = (*(header as *mut Obj)).tag;
        (*(header as *mut SkewedPtr)) = skew(new_loc as usize);
        header = tmp;
    }
    // At the end of the chain is the original header for the object
    debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);
    (*obj).tag = header;
}
