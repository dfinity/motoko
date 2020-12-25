//! Memory management for libtommath

/*
A libtommath arbitrary precision integer is a struct (`mp_int`) that contains a pointer to a data
array.

 - The libtommath library never allocates the struct, so we are in full control. We can embed the
   struct in Motoko heap object with a dedicated tag for it.

 - The data array is allocated with mp_calloc and mp_realloc. We provide these calls, allocate
   Motoko arrays (using the TAG_BLOB tag for byte arrays, not TAG_ARRAY for arrays of pointers) and
   store the pointer to the _payload_ in the `mp_digit* dp` field of the struct.

   The GC has special knowledge about the dp field of the struct and understands that this pointer
   points inside the TAG_BLOB heap object. We can still move them around in the GC without issues.

   The length of the byte array is always equal to the allocation asked for by libtommath (no
   shrinking via mp_realloc supported). This means we can assert that it matches the old_size
   passed to mp_realloc as an additional check. We can also support shrinking via mp_realloc, but
   then we have to drop that check.

Note on libtommath error handling
---------------------------------

Most libtommath operations return an int to signal error codes. These are (see tommath.h):

   #define MP_OKAY       0   / * ok result * /
   #define MP_MEM        -2  / * out of mem * /
   #define MP_VAL        -3  / * invalid input * /
   #define MP_RANGE      MP_VAL
   #define MP_ITER       -4  / * Max. iterations reached * /

We will never hit MP_MEM, because our allocation functions trap if they cannot allocate. But the
others can happen (e.g. division by 0). In that case, we call a trap function provided by the RTS.
*/

use crate::alloc::alloc_blob;
use crate::mem::memcpy_bytes;
use crate::types::{Blob, Bytes, TAG_BLOB};

unsafe fn mp_alloc(n: Bytes<u32>) -> *mut u8 {
    let blob = alloc_blob(n);
    blob.as_blob().payload_addr()
}

#[no_mangle]
unsafe extern "C" fn mp_calloc(n_elems: usize, elem_size: Bytes<usize>) -> *mut libc::c_void {
    let size = Bytes((n_elems * elem_size.0) as u32); // Overflow check?
    let payload = mp_alloc(size) as *mut u32;

    // NB. alloc_bytes rounds up to words so we do the same here to set the whole buffer
    for i in 0..size.to_words().0 {
        *payload.add(i as usize) = 0;
    }

    payload as *mut _
}

#[no_mangle]
unsafe extern "C" fn mp_realloc(
    ptr: *mut libc::c_void,
    old_size: Bytes<u32>,
    new_size: Bytes<u32>,
) -> *mut libc::c_void {
    let blob = (ptr as *mut u32).sub(2) as *mut Blob;

    assert_eq!((*blob).header.tag, TAG_BLOB);
    assert_eq!(blob.len(), old_size);

    if new_size > blob.len() {
        let new_ptr = mp_alloc(new_size);
        memcpy_bytes(new_ptr as usize, ptr as usize, old_size);
        new_ptr as *mut _
    } else if new_size == blob.len() {
        ptr
    } else {
        // libtommath only shrinks via mp_shrink and we do not use that function, so this should not
        // happen.
        panic!("mp_realloc: trying to shrink");
    }
}

#[no_mangle]
unsafe extern "C" fn mp_free(_ptr: *mut libc::c_void, _size: u32) {}
