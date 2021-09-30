use crate::rts_trap_with;
use crate::types::*;

/// A visitor that passes field addresses of fields with pointers to dynamic heap to the given
/// callback
pub unsafe fn visit_pointer_fields<F>(obj: *mut Obj, tag: Tag, mut visit_ptr_field: F)
where
    F: FnMut(*mut Value),
{
    match tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.payload_addr();
            for i in 0..obj.size() {
                let field_addr = obj_payload.add(i as usize);
                if (*field_addr).is_ptr_to_dynamic_heap() {
                    visit_ptr_field(obj_payload.add(i as usize));
                }
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            let array_payload = array.payload_addr();
            for i in 0..array.len() {
                let field_addr = array_payload.add(i as usize);
                if (*field_addr).is_ptr_to_dynamic_heap() {
                    visit_ptr_field(field_addr);
                }
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if (*field_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field_addr);
            }
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.payload_addr();
            for i in 0..closure.size() {
                let field_addr = closure_payload.add(i as usize);
                if (*field_addr).is_ptr_to_dynamic_heap() {
                    visit_ptr_field(field_addr);
                }
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = &mut (*some).field;
            if (*field_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field_addr);
            }
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = &mut (*variant).field;
            if (*field_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field_addr);
            }
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = &mut (*concat).text1;
            if (*field1_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field1_addr);
            }
            let field2_addr = &mut (*concat).text2;
            if (*field2_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field2_addr);
            }
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            let field_addr = &mut (*obj_ind).field;
            if (*field_addr).is_ptr_to_dynamic_heap() {
                visit_ptr_field(field_addr);
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
