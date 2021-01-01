use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, set_bit};
use crate::closure_table::closure_table_loc;
use crate::gc::{get_heap_base, get_static_roots};
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::{rts_trap_with, types::*};

#[no_mangle]
pub(crate) unsafe extern "C" fn mark_compact() {
    alloc_bitmap();
    alloc_mark_stack();

    mark_static_roots();

    // TODO: We could skip the is_tagged_scalar, heap_base etc. checks
    push_mark_stack(*closure_table_loc());

    mark_stack();

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
            rts_trap_with("invalid object tag in mark_fields");
        }
    }
}
