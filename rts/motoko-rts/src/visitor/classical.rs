use crate::rts_trap_with;
use crate::types::*;

/// A visitor that passes field addresses of fields with pointers to dynamic heap to the given
/// callback
///
/// Arguments:
///
/// * `ctx`: any context passed to the `visit_*` callbacks
/// * `obj`: the heap object to be visited (note: its heap tag may be invalid)
/// * `tag`: the heap object's logical tag (or start of array object's suffix slice)
/// * `heap_base`: start address of the dynamic heap.
/// * `visit_ptr_field`: callback for individual fields
/// * `visit_field_range`: callback for determining the suffix slice
///   Arguments:
///   * `&mut C`: passed context
///   * `usize`: start index of array suffix slice being visited
///   * `*mut Array`: home object of the slice (its heap tag may be invalid)
///   Returns:
///   * `usize`: start of the suffix slice of fields not to be passed to `visit_ptr_field`;
///            it is the callback's responsibility to deal with the spanned slice
pub unsafe fn visit_pointer_fields<C, F, G>(
    ctx: &mut C,
    obj: *mut Obj,
    tag: Tag,
    heap_base: usize,
    visit_ptr_field: F,
    visit_field_range: G,
) where
    F: Fn(&mut C, *mut Value),
    G: Fn(&mut C, usize, *mut Array) -> usize,
{
    match tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.payload_addr();
            for i in 0..obj.size() {
                let field_addr = obj_payload.add(i);
                if pointer_to_dynamic_heap(field_addr, heap_base) {
                    visit_ptr_field(ctx, obj_payload.add(i));
                }
            }
        }

        TAG_ARRAY_I | TAG_ARRAY_M | TAG_ARRAY_T | TAG_ARRAY_S | TAG_ARRAY_SLICE_MIN.. => {
            let (_, slice_start) = slice_start(tag);
            let array = obj as *mut Array;
            debug_assert!(slice_start <= array.len());
            let array_payload = array.payload_addr();
            let stop = visit_field_range(ctx, slice_start, array);
            debug_assert!(stop <= array.len());
            for i in slice_start..stop {
                let field_addr = array_payload.add(i);
                if pointer_to_dynamic_heap(field_addr, heap_base) {
                    visit_ptr_field(ctx, field_addr);
                }
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, heap_base) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.payload_addr();
            for i in 0..closure.size() {
                let field_addr = closure_payload.add(i);
                if pointer_to_dynamic_heap(field_addr, heap_base) {
                    visit_ptr_field(ctx, field_addr);
                }
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = &mut (*some).field;
            if pointer_to_dynamic_heap(field_addr, heap_base) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = &mut (*variant).field;
            if pointer_to_dynamic_heap(field_addr, heap_base) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_REGION => {
            let region = obj as *mut Region;
            let field_addr = &mut (*region).vec_pages;
            if pointer_to_dynamic_heap(field_addr, heap_base) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = &mut (*concat).text1;
            if pointer_to_dynamic_heap(field1_addr, heap_base) {
                visit_ptr_field(ctx, field1_addr);
            }
            let field2_addr = &mut (*concat).text2;
            if pointer_to_dynamic_heap(field2_addr, heap_base) {
                visit_ptr_field(ctx, field2_addr);
            }
        }

        TAG_BITS64_U | TAG_BITS64_S | TAG_BITS64_F | TAG_BITS32_U | TAG_BITS32_S | TAG_BITS32_F
        | TAG_BLOB_B | TAG_BLOB_T | TAG_BLOB_P | TAG_BLOB_A | TAG_BIGINT => {
            // These don't have pointers, skip
        }

        TAG_NULL => {
            rts_trap_with("encountered NULL object tag in visit_pointer_fields");
        }

        TAG_FWD_PTR | TAG_ONE_WORD_FILLER | TAG_FREE_SPACE | _ => {
            rts_trap_with("invalid object tag in visit_pointer_fields");
        }
    }
}

pub unsafe fn pointer_to_dynamic_heap(field_addr: *mut Value, heap_base: usize) -> bool {
    // NB. pattern matching on `field_addr.get()` generates inefficient code
    let field_value = (*field_addr).get_raw();
    is_ptr(field_value) && unskew(field_value as usize) >= heap_base
}
