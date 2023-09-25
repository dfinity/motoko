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
    visit_ptr_field: F,
    visit_field_range: G,
) where
    F: Fn(&mut C, *mut Value),
    G: Fn(&mut C, usize, *mut Array) -> usize,
{
    match tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            debug_assert!(is_pointer_field(obj.hash_blob_addr()));
            visit_ptr_field(ctx, obj.hash_blob_addr());
            let obj_payload = obj.payload_addr();
            for i in 0..obj.size() {
                let field_addr = obj_payload.add(i);
                if is_pointer_field(field_addr) {
                    visit_ptr_field(ctx, obj_payload.add(i));
                }
            }
        }

        TAG_ARRAY | TAG_ARRAY_SLICE_MIN.. => {
            let slice_start = if tag >= TAG_ARRAY_SLICE_MIN { tag } else { 0 };
            let array = obj as *mut Array;
            let array_payload = array.payload_addr();
            let stop = visit_field_range(ctx, slice_start, array);
            debug_assert!(stop <= array.len());
            for i in slice_start..stop {
                let field_addr = array_payload.add(i);
                if is_pointer_field(field_addr) {
                    visit_ptr_field(ctx, field_addr);
                }
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if is_pointer_field(field_addr) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.payload_addr();
            for i in 0..closure.size() {
                let field_addr = closure_payload.add(i);
                if is_pointer_field(field_addr) {
                    visit_ptr_field(ctx, field_addr);
                }
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = &mut (*some).field;
            if is_pointer_field(field_addr) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = &mut (*variant).field;
            if is_pointer_field(field_addr) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_REGION => {
            let region = obj as *mut Region;
            let field_addr = &mut (*region).vec_pages;
            if is_pointer_field(field_addr) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = &mut (*concat).text1;
            if is_pointer_field(field1_addr) {
                visit_ptr_field(ctx, field1_addr);
            }
            let field2_addr = &mut (*concat).text2;
            if is_pointer_field(field2_addr) {
                visit_ptr_field(ctx, field2_addr);
            }
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            let field_addr = &mut (*obj_ind).field;
            if is_pointer_field(field_addr) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_BITS64 | TAG_BLOB | TAG_BIGINT | TAG_NULL => {
            // These don't have pointers, skip
        }

        TAG_FWD_PTR | TAG_ONE_WORD_FILLER | TAG_FREE_SPACE | _ => {
            rts_trap_with("invalid object tag in visit_pointer_fields");
        }
    }
}

// Temporary function can be later removed.
pub unsafe fn is_pointer_field(field_addr: *mut Value) -> bool {
    let field_value = *field_addr;
    check_field_value(field_value);
    field_value.is_ptr()
}

// Temporary check, can be later removed.
#[cfg(feature = "ic")]
fn check_field_value(value: Value) {
    debug_assert!(value.is_scalar() || value.get_ptr() >= crate::persistence::HEAP_START);
}

#[cfg(not(feature = "ic"))]
fn check_field_value(_value: Value) {}
