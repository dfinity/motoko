use crate::alloc;
use crate::rts_trap_with;
use crate::types::*;

extern "C" {
    /// Get end_of_heap. Provided by the code generator (src/codegen/compile.ml)
    pub(crate) fn get_hp() -> usize;

    /// Set end_of_heap. Provided by the code generator (src/codegen/compile.ml)
    pub(crate) fn set_hp(hp: usize);

    /// Get __heap_base
    pub(crate) fn get_heap_base() -> usize;

    /// Skewed pointer to a skewed pointer to an array. See closure-table.c for details.
    pub(crate) fn closure_table_loc() -> SkewedPtr;

    /// Get pointer to the static memory with an array to the static roots. Provided by the
    /// generated code.
    pub(crate) fn get_static_roots() -> SkewedPtr;

    /// Provided by the C RTS, in `rts.c`.
    pub(crate) fn as_memcpy(to: usize, from: usize, n: Bytes<u32>);
}

/// Maximum live data retained in a GC.
//
static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations
pub(crate) static mut ALLOCATED: Bytes<u64> = Bytes(0);

unsafe fn note_live_size(live: Bytes<u32>) {
    MAX_LIVE = Bytes(::core::cmp::max(MAX_LIVE.0, live.0));
}

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

unsafe fn note_reclaimed(reclaimed: Bytes<u32>) {
    RECLAIMED.0 += reclaimed.0 as u64;
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[no_mangle]
unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
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

        _ => {
            rts_trap_with("Invalid object tag in object size\0".as_ptr());
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

/// Evacuate (copy) an object in from-space to to-space, update end_to_space. If the object was
/// already evacuated end_to_space is not changed.
///
/// Arguments:
///
/// - begin_from_space: Where the dynamic heap starts. Used for two things:
///
///   - An object is static if its address is below this value. These objects don't point to
///     dynamic heap so we skip those.
///
///   - After all objects are evacuated we move to-space to from-space, to be able to do that the
///     pointers need to point to their (eventual) locations in from-space, which is calculated with
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
    end_to_space: &mut usize,
    ptr_loc: usize,
) {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut SkewedPtr;

    if is_tagged_scalar(*ptr_loc) {
        return;
    }

    // Ignore static objects, they can't point to dynamic heap
    if (*ptr_loc).unskew() < begin_from_space {
        return;
    }

    let obj = (*ptr_loc).unskew() as *mut Obj;

    // Update the field if the object is already evacauted
    if (*obj).tag == TAG_INDIRECTION {
        let fwd = (*(obj as *const Indirection)).fwd;
        *ptr_loc = fwd;
        return;
    }

    let obj_size = object_size(obj as usize);
    let obj_size_bytes = words_to_bytes(obj_size);

    // Grow memory if needed
    alloc::grow_memory(*end_to_space + obj_size_bytes.0 as usize);

    // Copy object to to-space
    memcpy_words(*end_to_space, obj as usize, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (*end_to_space - begin_to_space) + begin_from_space;

    // Set forwarding pointer
    let fwd = obj as *mut Indirection;
    (*fwd).header.tag = TAG_INDIRECTION;
    (*fwd).fwd = skew(obj_loc);

    // Update evacuated field
    *ptr_loc = skew(obj_loc);

    // Update end of to-space
    *end_to_space += obj_size_bytes.0 as usize
}

/// Evacuate a blob payload pointed by a bigint. bigints are special in that a bigint's first field
/// is an internal pointer: it points to the _payload_ of a blob object, instead of skewedly pointing to the object start
///
/// - `ptr_loc`: Address of a `data_ptr` field of a BigInt (see types.rs). Points to payload of a
///   blob. See types.rs for blob layout.
unsafe fn evac_bigint_blob(
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    ptr_loc: *mut usize, // address of field with a pointer to a blob payload
) {
    let blob_payload_addr = *ptr_loc;

    // Get blob object from the payload
    let mut blob_obj_addr = skew(blob_payload_addr - 2 * (WORD_SIZE as usize));
    // Create a temporary field to the blob object, to be passed to `evac`.
    let blob_obj_addr_field = &mut blob_obj_addr;
    let blob_obj_addr_field_ptr = blob_obj_addr_field as *mut _;

    evac(
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
}

unsafe fn scav(
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    obj: usize,
) {
    let obj = obj as *const Obj;

    match (*obj).tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.offset(1) as *mut SkewedPtr;
            for i in 0..(*obj).size as isize {
                evac(
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
                evac(
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
            evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.offset(1) as *mut SkewedPtr;
            for i in 0..(*closure).size as isize {
                evac(
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
            evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = ((&mut (*variant).field) as *mut _) as usize;
            evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let data_ptr_addr = (&mut (*bigint).data_ptr) as *mut _;

            evac_bigint_blob(
                begin_from_space,
                begin_to_space,
                end_to_space,
                data_ptr_addr,
            );
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = ((&mut (*concat).text1) as *mut _) as usize;
            evac(begin_from_space, begin_to_space, end_to_space, field1_addr);
            let field2_addr = ((&mut (*concat).text2) as *mut _) as usize;
            evac(begin_from_space, begin_to_space, end_to_space, field2_addr);
        }

        TAG_OBJ_IND => {
            let obj_ind = obj as *mut ObjInd;
            let field_addr = ((&mut (*obj_ind).field) as *mut _) as usize;
            evac(begin_from_space, begin_to_space, end_to_space, field_addr);
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't include pointers, skip
        }

        TAG_INDIRECTION | _ => {
            // Any other tag is a bug
            rts_trap_with("invalid object tag in scav\0".as_ptr());
        }
    }
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots(
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    roots: *const Array,
) {
    // Roots are in a static array which we don't evacuate. Only evacuate elements.
    for i in 0..(*roots).len {
        let obj = SkewedPtr(array_get(roots, i) as usize);
        scav(begin_from_space, begin_to_space, end_to_space, obj.unskew());
    }
}

/// The entry point. Called by the generated code.
#[no_mangle]
pub unsafe extern "C" fn collect() {
    let begin_from_space = get_heap_base();
    let end_from_space = get_hp();
    let begin_to_space = end_from_space;
    let mut end_to_space = begin_to_space;

    let static_roots = get_static_roots().unskew() as *const Array;

    // Evacuate roots
    evac_static_roots(
        begin_from_space,
        begin_to_space,
        &mut end_to_space,
        static_roots,
    );

    evac(
        begin_from_space,
        begin_to_space,
        &mut end_to_space,
        closure_table_loc().unskew(),
    );

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < end_to_space {
        // NB: end_to_space keeps changing within this loop
        scav(begin_from_space, begin_to_space, &mut end_to_space, p);
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

    // Reset scratch space (for debugging purposes)
    memset(
        new_hp,
        bytes_to_words(Bytes((end_to_space - new_hp) as u32)),
        0,
    );
}
