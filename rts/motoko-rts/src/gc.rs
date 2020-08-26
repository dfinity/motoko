use crate::alloc;
use crate::common::rts_trap_with;
use crate::types::*;

extern "C" {
    /// Get end_of_heap. Implemented by the compiler.
    pub(crate) fn get_hp() -> usize;

    /// Set end_of_heap. Implemented by the compiler.
    pub(crate) fn set_hp(hp: usize);

    /// Get __heap_base
    pub(crate) fn get_heap_base() -> usize;

    /// Skewed pointer to a skewed pointer to an array. See closure-table.c for details.
    pub(crate) fn closure_table_loc() -> SkewedPtr;

    pub(crate) fn get_static_roots() -> SkewedPtr;

    pub(crate) fn as_memcpy(to: usize, from: usize, n: Bytes<u32>);
}

/// Maximum live data retained in a GC.
//
// NOTE (osa): In the original code (compile.ml) this variable was 64-bit, but I'm not sure why
// that is necessary. Pointers in wasm32 are 32-bits so if the entire address space is live you
// you max u32::MAX here, no need for 64-bits.
//
static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations done by `alloc::alloc_words` (called by the generated code).
pub static mut ALLOCATED: Bytes<u64> = Bytes(0);

unsafe fn note_live_size(live: Bytes<u32>) {
    MAX_LIVE = Bytes(::core::cmp::max(MAX_LIVE.0, live.0));
}

#[no_mangle]
pub unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

unsafe fn note_reclaimed(reclaimed: Bytes<u32>) {
    RECLAIMED.0 += reclaimed.0 as u64;
}

#[no_mangle]
pub unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[no_mangle]
pub unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    ALLOCATED
}

/// Returns object size in words
pub(crate) unsafe fn object_size(obj: usize) -> Words<u32> {
    let obj = obj as *const Obj;
    match (*obj).tag {
        TAG_OBJECT => {
            let object = obj as *const Object;
            let size = (*object).size;
            Words(size + 3) // TODO: document what "3" includes
        }

        TAG_OBJ_IND => Words(2),

        TAG_ARRAY => {
            let array = obj as *const Array;
            let size = (*array).len;
            Words(size + 2) // TODO: document what "2" includes
        }

        TAG_BITS64 => Words(3),

        TAG_MUTBOX => Words(2),

        TAG_CLOSURE => {
            let closure = obj as *const Closure;
            let size = (*closure).size;
            Words(size + 3) // TODO: document what "3" includes
        }

        TAG_SOME => Words(2),

        TAG_VARIANT => Words(3),

        TAG_BLOB => {
            let blob = obj as *const Blob;
            Words(bytes_to_words((*blob).len).0 + 2) // TODO: document this
        }

        TAG_INDIRECTION => {
            rts_trap_with("object_size of indirection\0".as_ptr());
        }

        TAG_BITS32 => Words(2),

        TAG_BIGINT => Words(5),

        TAG_CONCAT => Words(4),

        other => {
            let msg = format!(200, "invalid object tag {} in object size\0", other);
            rts_trap_with(msg.as_ptr());
        }
    }
}

pub(crate) fn is_tagged_scalar(p: SkewedPtr) -> bool {
    p.0 & 0b1 == 0
}

unsafe fn memcpy_words(to: usize, from: usize, n: Words<u32>) {
    as_memcpy(to, from, words_to_bytes(n))
}

unsafe fn memcpy_bytes(to: usize, from: usize, n: Bytes<u32>) {
    as_memcpy(to, from, n)
}

unsafe fn memset(s: usize, c: Words<u32>, b: u32) {
    let s_ptr = s as *mut u32;
    for i in 0..c.0 {
        *s_ptr.offset(i as isize) = b;
    }
}

/// Evacuate (copy) an object in from-space to to-space, return new end of to-space. Returns the
/// original to-space if the object is already evacuated.
///
/// Arguments:
///
/// - begin_from_space: Where the dynamic heap starts. Used for two things:
///
///   - An object is static if its address is below this value. These objects don't point to
///     dynamic heap so we skip those.
///
///   - After all objects are evacuated we move to-space to from-space, to be able to do that the
///     pointers need to point to their locations in from-space, which is calculated with
///     `end_to_space - begin_to_space + begin_from_space`.
///
/// - begin_to_space: Where to-space starts. See above for how this is used.
///
/// - end_to_space: Where the object in `ptr_loc` will be copied.
///
/// - ptr_loc: Location of the object to evacuate, e.g. an object field address.
///
unsafe fn evac(
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: usize,
    ptr_loc: usize,
) -> usize {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut SkewedPtr;

    if is_tagged_scalar(*ptr_loc) {
        return end_to_space;
    }

    // Ignore static objects, they can't point to dynamic heap
    if (*ptr_loc).unskew() < begin_from_space {
        return end_to_space;
    }

    let obj = (*ptr_loc).unskew() as *mut Obj;

    // Update the field if the object is already evacauted
    if (*obj).tag == TAG_INDIRECTION {
        let fwd = (*(obj as *const Indirection)).fwd;
        *ptr_loc = fwd;
        return end_to_space;
    }

    let obj_size = object_size(obj as usize);
    let obj_size_bytes = words_to_bytes(obj_size);

    // Grow memory if needed
    alloc::grow_memory(end_to_space + obj_size_bytes.0 as usize);

    // Copy object to to-space
    memcpy_words(end_to_space, obj as usize, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (end_to_space - begin_to_space) + begin_from_space;

    // Set forwarding pointer
    let fwd = obj as *mut Indirection;
    (*fwd).header.tag = TAG_INDIRECTION;
    (*fwd).fwd = skew(obj_loc);

    // Update evacuated field
    *ptr_loc = skew(obj_loc);

    // Return new end of to-space
    end_to_space + obj_size_bytes.0 as usize
}

/// Evacuate a blob payload pointed by a bigint. bigints are special in that a bigint's first field
/// is an internal pointer: it points to payload of a blob object, instead of to the header.
///
/// - `ptr_loc`: Address of a `data_ptr` field of a BigInt (see types.rs). Points to payload of a
///   blob. See types.rs for blob layout.
unsafe fn evac_bigint_blob(
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: usize,
    ptr_loc: *mut usize, // address of field with a pointer to a blob payload
) -> usize {
    let blob_payload_addr = *ptr_loc;

    // Get blob object from the payload
    let mut blob_obj_addr = skew(blob_payload_addr - 2 * (WORD_SIZE as usize));
    // Create a temporary field to the blob object, to be passed to `evac`.
    let blob_obj_addr_field = &mut blob_obj_addr;
    let blob_obj_addr_field_ptr = blob_obj_addr_field as *mut _;

    let ret = evac(
        begin_from_space,
        begin_to_space,
        end_to_space,
        blob_obj_addr_field_ptr as usize,
    );

    // blob_obj_addr_field now has the new location of the blob, get the payload address
    let blob_new_addr = (*blob_obj_addr_field).unskew();
    let blob_new_payload_addr = blob_new_addr + 2 * (WORD_SIZE as usize);

    // Update evacuated field
    *ptr_loc = blob_new_payload_addr; // not skewed!

    ret
}

unsafe fn scav(
    begin_from_space: usize,
    begin_to_space: usize,
    mut end_to_space: usize,
    obj: usize,
) -> usize {
    let obj = obj as *const Obj;

    match (*obj).tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.offset(1) as *mut SkewedPtr;
            for i in 0..(*obj).size as isize {
                end_to_space = evac(
                    begin_from_space,
                    begin_to_space,
                    end_to_space,
                    obj_payload.offset(i) as usize,
                );
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            let array_payload = array.offset(1) as *mut SkewedPtr;
            for i in 0..(*array).len as isize {
                end_to_space = evac(
                    begin_from_space,
                    begin_to_space,
                    end_to_space,
                    array_payload.offset(i) as usize,
                );
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = ((&mut (*mutbox).field) as *mut _) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.offset(1) as *mut SkewedPtr;
            for i in 0..(*closure).size as isize {
                end_to_space = evac(
                    begin_from_space,
                    begin_to_space,
                    end_to_space,
                    closure_payload.offset(i) as usize,
                );
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = ((&mut (*some).field) as *mut _) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = ((&mut (*variant).field) as *mut _) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let data_ptr_addr = (&mut (*bigint).data_ptr) as *mut _;

            end_to_space = evac_bigint_blob(
                begin_from_space,
                begin_to_space,
                end_to_space,
                data_ptr_addr,
            );
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = ((&mut (*concat).text1) as *mut _) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field1_addr);
            let field2_addr = ((&mut (*concat).text2) as *mut _) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field2_addr);
        }

        TAG_OBJ_IND => {
            let field_addr = obj.offset(1) as usize;
            end_to_space = evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't include pointers, skip
        }

        TAG_INDIRECTION => {
            // These are ignored in the original code for some reason
            // TODO (osa): I think this branch should panic
        }

        _ => {
            // Any other tag is a bug
            rts_trap_with("invalid object tag in scav\0".as_ptr());
        }
    }

    end_to_space
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots(
    begin_from_space: usize,
    begin_to_space: usize,
    mut end_to_space: usize,
    roots: *const Array,
) -> usize {
    // Roots are in a static array which we don't evacuate. Only evacuate elements.
    for i in 0..(*roots).len {
        let obj = SkewedPtr(array_get(roots, i) as usize);
        end_to_space = scav(begin_from_space, begin_to_space, end_to_space, obj.unskew());
    }
    end_to_space
}

/// The entry point. Called by the generated code.
#[no_mangle]
pub unsafe extern "C" fn collect() {
    // Beginning of tospace = end of fromspace
    let begin_from_space = get_heap_base();
    let end_from_space = get_hp();
    let begin_to_space = end_from_space;
    let mut end_to_space = begin_to_space;

    let static_roots = get_static_roots().unskew() as *const Array;

    // Evacuate roots
    end_to_space = evac_static_roots(begin_from_space, begin_to_space, end_to_space, static_roots);

    end_to_space = evac(
        begin_from_space,
        begin_to_space,
        end_to_space,
        closure_table_loc().unskew(),
    );

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < end_to_space {
        end_to_space = scav(begin_from_space, begin_to_space, end_to_space, p);
        p += words_to_bytes(object_size(p)).0 as usize;
    }

    // Note the stats
    let new_live_size = end_to_space - begin_to_space;
    note_live_size(Bytes(new_live_size as u32));

    let reclaimed = (end_from_space - begin_from_space) - (end_to_space - begin_to_space);
    note_reclaimed(Bytes(reclaimed as u32));

    // Copy to-space to the beginning of from-space
    memcpy_bytes(
        begin_from_space,
        begin_to_space,
        Bytes((end_to_space - begin_to_space) as u32),
    );

    // Reset the heap pointer
    let new_hp = begin_from_space + (end_to_space - begin_to_space);
    set_hp(new_hp);

/*
    // Reset scratch space (for debugging purposes)
    memset(
        new_hp,
        bytes_to_words(Bytes((end_to_space - new_hp) as u32)),
        0,
    );
*/
//    crate::debug::dump_heap(); // TODO (osa): The test 'life' fails if I remove this line?!?!?!
}
