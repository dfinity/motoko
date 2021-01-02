//! Implements "threaded compaction" as described in The Garbage Collection Handbook section 3.3.

// TODO: Try scanning the bitmap instead of the heap?
// TODO: Record the highest set bit, stop iteration the object for that bit

use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, set_bit};
use crate::closure_table::closure_table_loc;
use crate::gc::{get_heap_base, get_static_roots};
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::{rts_trap_with, types::*};

#[no_mangle]
pub(crate) unsafe extern "C" fn mark_compact(heap_base: u32, heap_end: u32) {
    let heap_size = Bytes(heap_end - heap_base);

    alloc_bitmap(heap_size);
    alloc_mark_stack();

    mark_static_roots();

    // TODO: We could skip the is_tagged_scalar, heap_base etc. checks
    push_mark_stack(*closure_table_loc());

    mark_stack();

    thread_roots();
    update_fwd_refs(heap_base, heap_end);
    update_bwd_refs(heap_base, heap_end);

    // free_bitmap();
    // free_mark_stack();
}

pub(crate) unsafe fn finish_mark_compact() {
    free_bitmap();
    free_mark_stack();
}

unsafe fn mark_static_roots() {
    let roots = get_static_roots().as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..roots.len() {
        let obj = roots.get(i).unskew();
        mark_fields(obj);
    }
}

unsafe fn push_mark_stack(obj: SkewedPtr) {
    if obj.is_tagged_scalar() {
        // Not a boxed object, skip
        return;
    }

    let obj = obj.unskew() as u32;

    if obj < get_heap_base() {
        // Static objects are not collected and not marked
        return;
    }

    let obj_idx = (obj - get_heap_base()) / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    mark_stack::push_mark_stack(obj as usize);
}

unsafe fn mark_stack() {
    while let Some(obj) = pop_mark_stack() {
        mark_fields(obj);
    }
}

unsafe fn mark_fields(obj: usize) {
    let obj = obj as *mut Obj;

    match obj.tag() {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            for i in 0..obj.size() {
                push_mark_stack(obj.get(i));
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            for i in 0..array.len() {
                push_mark_stack(array.get(i));
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            push_mark_stack((*mutbox).field);
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            for i in 0..closure.size() {
                push_mark_stack(closure.get(i));
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            push_mark_stack((*some).field);
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            push_mark_stack((*variant).field);
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let data_ptr = *bigint.data_ptr();
            let blob =
                ((data_ptr as usize) - (size_of::<Blob>().to_bytes().0 as usize)) as *mut Blob;
            assert_eq!((*blob).header.tag, TAG_BLOB);
            push_mark_stack(skew(blob as usize));
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            push_mark_stack(concat.text1());
            push_mark_stack(concat.text2());
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            push_mark_stack((*obj_ind).field);
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

unsafe fn thread_roots() {
    // Static roots
    let roots = get_static_roots().as_array();
    for i in 0..roots.len() {
        thread_obj_fields(roots.get(i).unskew() as *mut Obj);
    }
    // No need to thread closure table here as it's on heap and we already marked it
}

/// Scan the heap, update forward references. At the end of this pass all fields will be threaded
/// and forward references will be unthreaded, pointing to the object's new location.
unsafe fn update_fwd_refs(heap_base: u32, heap_end: u32) {
    let mut p = heap_base;
    let mut free = p;
    while p < heap_end {
        let p_bit_idx = (p - heap_base) / WORD_SIZE;
        let p_size_bytes = object_size(p as usize).to_bytes();

        if get_bit(p_bit_idx) {
            // Thread fields
            thread_obj_fields(p as *mut Obj);

            // Update forward references to the object to the object's new location and restore
            // object header
            unthread(p as *mut Obj, free);

            free += p_size_bytes.0;
        }

        p += p_size_bytes.0;
    }
}

/// Assumes all fields all threaded. Updates backward references and move objects to their new
/// locations.
unsafe fn update_bwd_refs(heap_base: u32, heap_end: u32) {
    let mut p = heap_base;
    let mut free = p;
    while p < heap_end {
        let p_bit_idx = (p - heap_base) / WORD_SIZE;
        let p_size_words = object_size(p as usize);
        let p_size_bytes = p_size_words.to_bytes();

        if get_bit(p_bit_idx) {
            // Update backward references to the object's new location and restore object header
            unthread(p as *mut Obj, free);

            // Move the object to its new location
            move_(p, free, p_size_words);

            free += p_size_bytes.0;
        }

        p += p_size_bytes.0;
    }
}

unsafe fn thread_obj_fields(obj: *mut Obj) {
    match obj.tag() {
        TAG_OBJECT => {
            todo!()
        }

        TAG_ARRAY => {
            todo!()
        }

        TAG_MUTBOX => {
            todo!()
        }

        TAG_CLOSURE => {
            todo!()
        }

        TAG_SOME => {
            todo!()
        }

        TAG_VARIANT => {
            todo!()
        }

        TAG_BIGINT => {
            todo!()
        }

        TAG_CONCAT => {
            todo!()
        }

        TAG_OBJ_IND => {
            todo!()
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

unsafe fn thread(field: *mut SkewedPtr) {
    if (*field).is_tagged_scalar() {
        // Field has a scalar value, skip
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
        let tmp = (*(header as *mut Obj)).tag;
        (*(header as *mut SkewedPtr)) = skew(new_loc as usize);
        header = tmp;
    }
    // At the end of the chain is the original header for the object
    (*obj).tag = header;
}

unsafe fn move_(obj: u32, new_loc: u32, size: Words<u32>) {
    let obj = obj as *const u32;
    let new_loc = new_loc as *mut u32;

    for i in 0..size.0 as usize {
        *new_loc.add(i) = *obj.add(i);
    }
}
