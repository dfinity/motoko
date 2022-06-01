use crate::rts_trap_with;
use crate::types::*;

/// A visitor that passes field addresses of fields with pointers to dynamic heap to the given
/// callback
pub unsafe fn visit_pointer_fields<C, F, G>(
    ctx: &mut C,
    obj: *mut Obj,
    tag: Tag,
    heap_base: usize,
    visit_ptr_field: F,
    visit_field_range: G,
) where
    F: Fn(&mut C, *mut Value),
    G: Fn(&mut C, u32, *const u32) -> u32,
{
    match tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.payload_addr();
            for i in 0..obj.size() {
                let field_addr = obj_payload.add(i as usize);
                if pointer_to_dynamic_heap(field_addr, heap_base) {
                    visit_ptr_field(ctx, obj_payload.add(i as usize));
                }
            }
        }

        TAG_ARRAY | TAG_ARRAY_SLICE_LIMIT.. => {
	    let slice_start = if tag >= TAG_ARRAY_SLICE_LIMIT { tag } else { 0 };
            let array = obj as *mut Array;
            let array_payload = array.payload_addr();
            let stop = visit_field_range(ctx, slice_start, &(*array).len);
            debug_assert!(stop <= array.len());
            for i in slice_start..stop {
                let field_addr = array_payload.add(i as usize);
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
                let field_addr = closure_payload.add(i as usize);
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

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            let field_addr = &mut (*obj_ind).field;
            if pointer_to_dynamic_heap(field_addr, heap_base) {
                visit_ptr_field(ctx, field_addr);
            }
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB | TAG_BIGINT | TAG_ONE_WORD_FILLER | TAG_FREE_SPACE => {
            // These don't have pointers, skip
        }

        TAG_NULL => {
            rts_trap_with("encountered NULL object tag in visit_pointer_fields");
        }

        TAG_FWD_PTR | _ => {
            rts_trap_with("invalid object tag in visit_pointer_fields");
        }
    }
}

pub unsafe fn pointer_to_dynamic_heap(field_addr: *mut Value, heap_base: usize) -> bool {
    // NB. pattern matching on `field_addr.get()` generates inefficient code
    let field_value = (*field_addr).get_raw();
    is_ptr(field_value) && unskew(field_value as usize) >= heap_base
}
