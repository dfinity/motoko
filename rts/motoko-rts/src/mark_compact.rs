use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, set_bit};
use crate::gc::{get_heap_base, get_static_roots, HP};
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::{rts_trap_with, types::*};

#[no_mangle]
unsafe extern "C" fn mark_compact() {
    alloc_bitmap();
    alloc_mark_stack();

    mark_static_roots();
    mark_stack();

    free_bitmap();
    free_mark_stack();
}

unsafe fn mark_static_roots() {
    let roots = get_static_roots().as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..roots.len() {
        mark_fields(roots.get(i).unskew());
    }
}

unsafe fn push_mark_stack(obj: usize) {
    if obj < get_heap_base() as usize {
        // Static objects are not collected and not marked
        return;
    }

    if get_bit(obj as u32) {
        // Already marked
        return;
    }

    set_bit(obj as u32);
    mark_stack::push_mark_stack(obj);
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
                push_mark_stack(obj.get(i).unskew());
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            for i in 0..array.len() {
                push_mark_stack(array.get(i).unskew());
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            push_mark_stack((*mutbox).field.unskew());
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            for i in 0..closure.size() {
                push_mark_stack(closure.get(i).unskew());
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            push_mark_stack((*some).field.unskew());
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            push_mark_stack((*variant).field.unskew());
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            push_mark_stack(concat.text1().unskew());
            push_mark_stack(concat.text2().unskew());
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            push_mark_stack((*obj_ind).field.unskew());
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't include pointers, skip
        }

        TAG_NULL => {
            rts_trap_with("encountered NULL object tag in dynamic object in mark_fields");
        }

        TAG_FWD_PTR | _ => {
            // Any other tag is a bug
            rts_trap_with("invalid object tag in mark_fields");
        }
    }
}
