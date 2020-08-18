// TODO: Review SkewedPtrs below, most of them don't need to be SkewedPtrs
//
// TODO: usize == u32 on Wasm, but rustc doesn't assume we're compiling exclusively to Wasm so we
// have lots of coercions. It'll be simpler if we only used u32.
//
// TODO: inconsistent use of size vs. len

use core::arch::wasm32;
use core::fmt::Write;

use crate::array::array_idx_unchecked;
use crate::common::{debug_print, rts_trap_with, FmtWrite};
use crate::types::*;

const WORD_SIZE: u32 = 4;

extern "C" {
    /// Get end_of_heap
    fn get_hp() -> usize;

    /// Set end_of_heap
    fn set_hp(hp: SkewedPtr);

    /// Get __heap_base
    fn get_heap_base() -> usize;

    /// See closure-table.c
    fn closure_table_loc() -> SkewedPtr;

    fn get_static_roots() -> SkewedPtr;
}

/// Maximum live data retained in a GC, in bytes.
//
// NOTE (osa): In the original code (compile.ml) this variable was 64-bit, but I'm not sure why
// that is necessary. Pointers in wasm32 are 32-bits so if the entire address space is live you
// you max u32::MAX here, no need for 64-bits.
//
static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// In bytes
// TODO: I don't understand what this is for
static mut RECLAIMED: Bytes<u64> = Bytes(0);

#[no_mangle]
pub unsafe extern "C" fn note_live_size(live: Bytes<u32>) {
    MAX_LIVE = Bytes(::core::cmp::max(MAX_LIVE.0, live.0));
}

#[no_mangle]
pub unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

#[no_mangle]
pub unsafe extern "C" fn note_reclaimed(reclaimed: Bytes<u32>) {
    RECLAIMED.0 += reclaimed.0 as u64;
}

#[no_mangle]
pub unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

/// Page allocation. Ensures that the memory up to the given unskewed pointer is allocated.
#[no_mangle]
pub unsafe extern "C" fn grow_memory(ptr: usize) {
    let total_pages_needed = ((ptr / 65536) + 1) as i32;
    let current_pages = wasm32::memory_size(0) as i32;
    let new_pages_needed = total_pages_needed - current_pages;
    if new_pages_needed > 0 {
        if wasm32::memory_grow(0, new_pages_needed as usize) == core::usize::MAX {
            rts_trap_with("Cannot grow memory\0".as_ptr());
        }
    }
}

/// Returns object size in words
#[no_mangle]
pub unsafe extern "C" fn object_size(obj: usize) -> Words<u32> {
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
            Words((((*blob).len + 3) / 4) + 2) // TODO: document this
        }

        TAG_INDIRECTION => {
            rts_trap_with("object_size of indirection\0".as_ptr());
        }

        TAG_BITS32 => Words(2),

        TAG_BIGINT => Words(5),

        TAG_CONCAT => Words(4),

        _ => {
            rts_trap_with("invalid object tag in object_size\0".as_ptr());
        }
    }
}

#[no_mangle]
pub unsafe extern "C" fn is_tagged_scalar(p: SkewedPtr) -> bool {
    p.0 & 0b1 == 0
}

fn words_to_bytes(words: Words<u32>) -> Bytes<u32> {
    Bytes(words.0 * WORD_SIZE)
}

// Rounds up
fn bytes_to_words(bytes: Bytes<u32>) -> Words<u32> {
    // Rust issue for adding ceiling_div: https://github.com/rust-lang/rfcs/issues/2844
    Words((bytes.0 + WORD_SIZE - 1) / WORD_SIZE)
}

#[no_mangle]
pub unsafe extern "C" fn memcpy_words_skewed(to: SkewedPtr, from: SkewedPtr, n: Words<u32>) {
    let to_ptr = to.unskew() as *mut u32;
    let from_ptr = from.unskew() as *mut u32;
    for i in 0..n.0 as isize {
        *to_ptr.offset(i) = *(from_ptr.offset(i));
    }
}

/// Evacuate (copy) an object in from-space to to-space, return new end of to-space. Returns the
/// original to-space if the object is already evacuated.
///
/// Arguments:
///
/// - begin_from_space: Where the dynamic heap starts. Used for two things:
///
///   - An object is static if its address is below this value. These object don't need collection
///      so we skip those.
///
///   - After all objects are evacuated we move to-space to from-space, to be able to do that the
///     pointers need to point to their locations in from-space, which is calculated with
///     `end_to_space - begin_to_space + begin_from_space`.
///
/// - begin_to_space: Where to-space starts. See above for how this is used.
///
/// - ptr: Location of the object to evacuate, e.g. an object field address.
///

// NB. This is 'evacuate_common' in compile.ml

unsafe fn evac(
    begin_from_space: usize, // not skewed!
    begin_to_space: usize,   // not skewed!
    end_to_space: usize,     // not skewed!
    ptr_loc: SkewedPtr,      // skewed pointer to the field with object to evacuate to to-space
) -> SkewedPtr {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc.unskew() as *mut SkewedPtr;

    // Ignore static objects, they can't point to dynamic heap
    if (ptr_loc as usize) < begin_from_space {
        return skew(end_to_space);
    }

    let obj_skewed = *ptr_loc;
    let obj = (*ptr_loc).unskew() as *mut Obj;

    // Update the field if the object is already evacauted
    if (*obj).tag == TAG_INDIRECTION {
        let fwd = (*(obj as *const Indirection)).fwd;
        *ptr_loc = fwd;
    }

    let obj_size = object_size(obj_skewed.unskew());
    let obj_size_bytes = words_to_bytes(obj_size);

    // Grow memory if needed
    grow_memory(end_to_space + obj_size_bytes.0 as usize);

    // Copy object to to-space
    memcpy_words_skewed(skew(end_to_space), obj_skewed, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (end_to_space - begin_to_space) + begin_from_space;

    // Set forwarding pointer
    let fwd = obj as *mut Indirection;
    (*fwd).header.tag = TAG_INDIRECTION;
    (*fwd).fwd = skew(obj_loc);

    // Update evacuated field
    *ptr_loc = skew(obj_loc);

    // Return new end of to-space
    skew(end_to_space + obj_size_bytes.0 as usize)
}

#[no_mangle]
pub unsafe extern "C" fn evacuate(
    begin_from_space: usize, // not skewed!
    begin_to_space: usize,   // not skewed!
    end_to_space: usize,     // not skewed!
    ptr_loc: SkewedPtr,      // skewed pointer to field with the object to evacuate to to-space
) -> SkewedPtr {
    if is_tagged_scalar(*(ptr_loc.unskew() as *const SkewedPtr)) {
        return skew(end_to_space);
    }

    evac(begin_from_space, begin_to_space, end_to_space, ptr_loc)
}

/// Evacuate a blob payload pointed by a bigint. bigints are special in that a bigint's first field
/// is an internal pointer: it points to payload of a blob object, instead of to the header.
///
/// - `ptr_loc`: Address of a `data_ptr` field of a BigInt (see types.rs). Points to payload of a
///   blob. See types.rs for blob layout.
#[no_mangle]
pub unsafe extern "C" fn evacuate_bigint_blob(
    begin_from_space: SkewedPtr,
    begin_to_space: SkewedPtr,
    end_to_space: SkewedPtr,
    ptr_loc: *mut usize, // address of field with a pointer to a blob payload, not skewed
) -> SkewedPtr {
    let blob_payload_addr = *ptr_loc;

    // Get blob object from the payload
    let mut blob_obj_addr = skew(blob_payload_addr - 2 * (WORD_SIZE as usize));
    // Create a temporary field to the blob object, to be passed to `evac`.
    let blob_obj_addr_field = &mut blob_obj_addr;
    let blob_obj_addr_field_ptr = blob_obj_addr_field as *mut _;

    let ret = evacuate(
        begin_from_space.unskew(),
        begin_to_space.unskew(),
        end_to_space.unskew(),
        skew(blob_obj_addr_field_ptr as usize), // FIXME: no need to be skewed
    );

    // blob_obj_addr_field now has the new location of the blob, get the payload address
    let blob_new_addr = (*blob_obj_addr_field).unskew();
    let blob_new_payload_addr = blob_new_addr + 2 * (WORD_SIZE as usize);

    // Update evacuated field
    *ptr_loc = blob_new_payload_addr; // not skewed!

    ret
}

unsafe fn scav(
    begin_from_space: SkewedPtr,
    begin_to_space: SkewedPtr,
    mut end_to_space: SkewedPtr,
    obj: SkewedPtr,
) -> SkewedPtr {
    let obj = obj.unskew() as *const Obj;

    match (*obj).tag {
        TAG_OBJECT => {
            let obj = obj as *mut Object;
            let obj_payload = obj.offset(1) as *mut SkewedPtr;
            for i in 0..(*obj).size as isize {
                end_to_space = evacuate(
                    begin_from_space.unskew(),
                    begin_to_space.unskew(),
                    end_to_space.unskew(),
                    skew(obj_payload.offset(i) as usize), // FIXME: no need to be skewed
                );
            }
        }

        TAG_ARRAY => {
            let array = obj as *mut Array;
            let array_payload = array.offset(1) as *mut SkewedPtr;
            for i in 0..(*array).len as isize {
                end_to_space = evacuate(
                    begin_from_space.unskew(),
                    begin_to_space.unskew(),
                    end_to_space.unskew(),
                    skew(array_payload.offset(i) as usize), // FIXME
                );
            }
        }

        TAG_MUTBOX => {
            let mutbox = obj as *mut MutBox;
            let field_addr = ((&mut (*mutbox).field) as *mut _) as usize;
            end_to_space = evacuate(
                begin_from_space.unskew(),
                begin_to_space.unskew(),
                end_to_space.unskew(),
                skew(field_addr),
            );
        }

        TAG_CLOSURE => {
            let closure = obj as *mut Closure;
            let closure_payload = closure.offset(1) as *mut SkewedPtr;
            for i in 0..(*closure).size as isize {
                end_to_space = evacuate(
                    begin_from_space.unskew(),
                    begin_to_space.unskew(),
                    end_to_space.unskew(),
                    skew(closure_payload.offset(i) as usize), // FIXME
                );
            }
        }

        TAG_SOME => {
            let some = obj as *mut Some;
            let field_addr = ((&mut (*some).field) as *mut _) as usize;
            end_to_space = evacuate(
                begin_from_space.unskew(),
                begin_to_space.unskew(),
                end_to_space.unskew(),
                skew(field_addr),
            );
        }

        TAG_VARIANT => {
            let variant = obj as *mut Variant;
            let field_addr = ((&mut (*variant).field) as *mut _) as usize;
            end_to_space = evacuate(
                begin_from_space.unskew(),
                begin_to_space.unskew(),
                end_to_space.unskew(),
                skew(field_addr),
            );
        }

        TAG_BIGINT => {
            let bigint = obj as *mut BigInt;
            let data_ptr_addr = (&mut (*bigint).data_ptr) as *mut _;

            end_to_space = evacuate_bigint_blob(
                begin_from_space,
                begin_to_space,
                end_to_space,
                data_ptr_addr,
            );
        }

        TAG_CONCAT => {
            let concat = obj as *mut Concat;
            let field1_addr = ((&mut (*concat).text1) as *mut _) as usize;
            end_to_space = evacuate(
                begin_from_space.unskew(),
                begin_to_space.unskew(),
                end_to_space.unskew(),
                skew(field1_addr),
            );
            let field2_addr = ((&mut (*concat).text2) as *mut _) as usize;
            end_to_space = evacuate(
                begin_from_space.unskew(),
                begin_to_space.unskew(),
                end_to_space.unskew(),
                skew(field2_addr),
            );
        }

        TAG_BITS64 | TAG_BITS32 | TAG_BLOB => {
            // These don't include pointers, skip
        }

        TAG_OBJ_IND | TAG_INDIRECTION => {
            // These are ignored in the original code for some reason TODO
        }

        _ => {
            // Any other tag is a bug
            rts_trap_with("invalid object tag in scav\0".as_ptr());
        }
    }

    end_to_space
}

unsafe fn evac_static_roots(
    begin_from_space: SkewedPtr,
    begin_to_space: SkewedPtr,
    mut end_to_space: SkewedPtr,
    roots: *const Array,
) -> SkewedPtr {
    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);

    write!(&mut fmt, "Evacuating {} roots...\n", (*roots).len).unwrap();
    fmt.print();

    // Roots are in a static array which we don't evacuate. Only evacuate elements.
    for i in 0..(*roots).len {
        let obj = array_idx_unchecked(roots, i);

        fmt.reset();
        write!(&mut fmt, "Evacuating root {}: {:#x}", i, obj.unskew()).unwrap();
        fmt.print();

        end_to_space = scav(begin_from_space, begin_to_space, end_to_space, obj);
    }
    end_to_space
}

#[no_mangle]
pub unsafe extern "C" fn rust_collect_garbage() {
    debug_print("### GC begins");

    let static_roots = get_static_roots().unskew() as *const Array;

    // Beginning of tospace = end of fromspace
    let begin_from_space = get_heap_base();
    let end_from_space = get_hp();
    let begin_to_space = end_from_space;
    let mut end_to_space = begin_to_space;

    debug_print("### Evacuating roots");

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);

    write!(
        &mut fmt,
        "### begin_from_space={:#x}\n\
         ### end_from_space={:#x}\n\
         ### begin_to_space={:#x}\n\
         ### end_to_space={:#x}\n\
         ### static_roots={:#x}\n",
        begin_from_space, end_from_space, begin_to_space, end_to_space, static_roots as usize,
    )
    .expect("Can't write");

    fmt.print();

    // Evacuate roots
    end_to_space = evac_static_roots(
        skew(begin_from_space),
        skew(begin_to_space),
        skew(end_to_space),
        static_roots,
    )
    .unskew();

    debug_print("### Evacuated static roots");

    end_to_space = evacuate(
        begin_from_space,
        begin_to_space,
        end_to_space,
        closure_table_loc(),
    )
    .unskew();

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < end_to_space {
        end_to_space = scav(
            skew(begin_from_space),
            skew(begin_to_space),
            skew(end_to_space),
            skew(p),
        )
        .unskew();
        p += words_to_bytes(object_size(p)).0 as usize;
    }

    // Note the stats
    let new_live_size = end_to_space - begin_to_space;
    note_live_size(Bytes(new_live_size as u32));

    let reclaimed = (end_from_space - begin_from_space) - (end_to_space - begin_to_space);
    note_reclaimed(Bytes(reclaimed as u32));

    // Copy to-space to the beginning of from-space
    memcpy_words_skewed(
        skew(begin_from_space),
        skew(begin_to_space),
        bytes_to_words(Bytes((end_to_space - begin_to_space) as u32)),
    );

    // Reset the heap pointer
    set_hp(skew(begin_from_space + (end_to_space - begin_to_space)));

    debug_print("### GC finished");
}
