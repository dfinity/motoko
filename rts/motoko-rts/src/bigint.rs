//! Implements big int support:
//!
//! - libtommath memory management
//! - libtommath wrappers
//! - (s)leb128 encoding/decoding for bigints

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
*/

use crate::alloc::{alloc_blob, alloc_words};
use crate::buf::{read_byte, Buf};
use crate::mem::memcpy_bytes;
use crate::types::{size_of, BigInt, Blob, Bytes, SkewedPtr, TAG_BIGINT, TAG_BLOB};

use crate::{rts_trap, tommath_bindings::*};

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

/*
Note on libtommath error handling
---------------------------------

Most libtommath operations return an enum to signal error codes. These are (see tommath.h):

   MP_OKAY  = 0,   /* no error */
   MP_ERR   = -1,  /* unknown error */
   MP_MEM   = -2,  /* out of mem */
   MP_VAL   = -3,  /* invalid input */
   MP_ITER  = -4,  /* maximum iterations reached */
   MP_BUF   = -5   /* buffer overflow, supplied buffer too small */

We will never hit MP_MEM, because our allocation functions trap if they cannot allocate. But the
others can happen (e.g. division by 0). In that case, we call a trap function provided by the
compiler.
*/

// TODO (osa): Why generate this in the compiler?
extern "C" {
    fn bigint_trap() -> !;
}

unsafe fn check(err: mp_err) {
    if err != 0 {
        bigint_trap();
    }
}

unsafe fn mp_get_u32(p: *const mp_int) -> u32 {
    mp_get_i32(p) as u32
}

unsafe fn mp_get_u64(p: *const mp_int) -> u64 {
    mp_get_i64(p) as u64
}

unsafe fn mp_isneg(p: *const mp_int) -> bool {
    (*p).sign == 1
}

unsafe fn mp_iszero(p: *const mp_int) -> bool {
    (*p).used == 0
}

#[no_mangle]
unsafe extern "C" fn bigint_alloc() -> SkewedPtr {
    let r = alloc_words(size_of::<BigInt>());
    let r_ptr = r.unskew() as *mut BigInt;
    (*r_ptr).header.tag = TAG_BIGINT;
    check(mp_init(&mut (*r_ptr).mp_int));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_of_word32(w: u32) -> SkewedPtr {
    let r = bigint_alloc();
    mp_set_u32(r.as_bigint().mp_int_ptr(), w);
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_of_word32_signed(i: i32) -> SkewedPtr {
    let r = bigint_alloc();
    mp_set_i32(r.as_bigint().mp_int_ptr(), i);
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word32_wrap(p: SkewedPtr) -> u32 {
    mp_get_u32(p.as_bigint().mp_int_ptr())
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word32_trap(p: SkewedPtr) -> u32 {
    let mp_int = p.as_bigint().mp_int_ptr();

    if mp_isneg(mp_int) || mp_count_bits(mp_int) > 32 {
        bigint_trap();
    }

    mp_get_u32(mp_int)
}

// a : BigInt, msg : Blob
#[no_mangle]
unsafe extern "C" fn bigint_to_word32_trap_with(p: SkewedPtr, msg: SkewedPtr) -> u32 {
    let mp_int = p.as_bigint().mp_int_ptr();

    if mp_isneg(mp_int) || mp_count_bits(mp_int) > 32 {
        rts_trap(msg.as_blob().payload_addr(), msg.as_blob().len());
    }

    mp_get_u32(mp_int)
}

// TODO (osa): I don't understand the code below, it uses get_mag (magnitude?) but the name
// suggests it's to convert bigint to u32?
#[no_mangle]
unsafe extern "C" fn bigint_to_word32_signed_trap(p: SkewedPtr) -> i32 {
    let mp_int = p.as_bigint().mp_int_ptr();

    if mp_count_bits(mp_int) > 32 {
        bigint_trap();
    }

    let x = mp_get_mag_u32(mp_int) as i32;
    if mp_isneg(mp_int) {
        let x = -x;
        if x >= 0 {
            // TODO (osa): Why not ==?
            bigint_trap();
        }
        x
    } else {
        if x < 0 {
            bigint_trap();
        }
        x
    }
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word64_wrap(p: SkewedPtr) -> u64 {
    mp_get_u64(p.as_bigint().mp_int_ptr())
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word64_trap(p: SkewedPtr) -> u64 {
    let mp_int = p.as_bigint().mp_int_ptr();

    if mp_isneg(mp_int) || mp_count_bits(mp_int) > 64 {
        bigint_trap();
    }

    mp_get_u64(mp_int)
}

// TODO (osa): Same as bigint_to_word32_signed_trap
#[no_mangle]
unsafe extern "C" fn bigint_to_word64_signed_trap(p: SkewedPtr) -> i64 {
    let mp_int = p.as_bigint().mp_int_ptr();

    if mp_count_bits(mp_int) > 64 {
        bigint_trap();
    }

    let x = mp_get_mag_u64(mp_int) as i64;
    if mp_isneg(mp_int) {
        let x = -x;
        if x >= 0 {
            // TODO (osa): Why not ==?
            bigint_trap();
        }
        x
    } else {
        if x < 0 {
            bigint_trap();
        }
        x
    }
}

#[no_mangle]
unsafe extern "C" fn bigint_of_word64(w: u64) -> SkewedPtr {
    let p = bigint_alloc();
    mp_set_u64(p.as_bigint().mp_int_ptr(), w);
    p
}

// TODO (osa): Rename to int64 for consistency?
#[no_mangle]
unsafe extern "C" fn bigint_of_word64_signed(i: i64) -> SkewedPtr {
    let p = bigint_alloc();
    mp_set_i64(p.as_bigint().mp_int_ptr(), i);
    p
}

#[no_mangle]
unsafe extern "C" fn bigint_eq(a: SkewedPtr, b: SkewedPtr) -> bool {
    mp_cmp(a.as_bigint().mp_int_ptr(), b.as_bigint().mp_int_ptr()) == 0
}

#[no_mangle]
unsafe extern "C" fn bigint_lt(a: SkewedPtr, b: SkewedPtr) -> bool {
    mp_cmp(a.as_bigint().mp_int_ptr(), b.as_bigint().mp_int_ptr()) < 0
}

#[no_mangle]
unsafe extern "C" fn bigint_gt(a: SkewedPtr, b: SkewedPtr) -> bool {
    mp_cmp(a.as_bigint().mp_int_ptr(), b.as_bigint().mp_int_ptr()) > 0
}

#[no_mangle]
unsafe extern "C" fn bigint_le(a: SkewedPtr, b: SkewedPtr) -> bool {
    mp_cmp(a.as_bigint().mp_int_ptr(), b.as_bigint().mp_int_ptr()) <= 0
}

#[no_mangle]
unsafe extern "C" fn bigint_ge(a: SkewedPtr, b: SkewedPtr) -> bool {
    mp_cmp(a.as_bigint().mp_int_ptr(), b.as_bigint().mp_int_ptr()) >= 0
}

#[no_mangle]
unsafe extern "C" fn bigint_add(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_add(
        a.as_bigint().mp_int_ptr(),
        b.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_sub(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_sub(
        a.as_bigint().mp_int_ptr(),
        b.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_mul(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_mul(
        a.as_bigint().mp_int_ptr(),
        b.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_pow(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let exp = bigint_to_word32_trap(b);
    let r = bigint_alloc();
    check(mp_expt_u32(
        a.as_bigint().mp_int_ptr(),
        exp,
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_div(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    let mut rem: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    check(mp_init(&mut rem as *mut _));
    check(mp_div(
        a.as_bigint().mp_int_ptr(),
        b.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
        &mut rem, // TODO: not possible to pass null here?
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_rem(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    let mut quot: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    check(mp_init(&mut quot as *mut _));
    check(mp_div(
        a.as_bigint().mp_int_ptr(),
        b.as_bigint().mp_int_ptr(),
        &mut quot, // TODO: not possible to pass null here?
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_neg(a: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_neg(
        a.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_abs(a: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_abs(
        a.as_bigint().mp_int_ptr(),
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_isneg(a: SkewedPtr) -> bool {
    mp_isneg(a.as_bigint().mp_int_ptr())
}

#[no_mangle]
unsafe extern "C" fn bigint_lsh(a: SkewedPtr, b: i32) -> SkewedPtr {
    let r = bigint_alloc();
    check(mp_mul_2d(
        a.as_bigint().mp_int_ptr(),
        b,
        r.as_bigint().mp_int_ptr(),
    ));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_count_bits(a: SkewedPtr) -> i32 {
    mp_count_bits(a.as_bigint().mp_int_ptr())
}

#[no_mangle]
unsafe extern "C" fn bigint_leb128_size(a: SkewedPtr) -> u32 {
    if mp_iszero(a.as_bigint().mp_int_ptr()) {
        1
    } else {
        (bigint_count_bits(a) as u32 + 6) / 7 // divide by 7, round up
    }
}

// TODO (osa): I don't understand what `add_bit` means
unsafe fn bigint_leb128_encode_go(tmp: *mut mp_int, mut buf: *mut u8, add_bit: bool) {
    if mp_isneg(tmp) {
        bigint_trap();
    }

    loop {
        let byte = mp_get_u32(tmp) as u8;
        check(mp_div_2d(tmp, 7, tmp, core::ptr::null_mut()));
        if !mp_iszero(tmp) || (add_bit && byte & (1 << 6) != 0) {
            *buf = byte | (1 << 7);
            buf = buf.add(1);
        } else {
            *buf = byte;
            break;
        }
    }
}

#[no_mangle]
unsafe extern "C" fn bigint_leb128_encode(n: SkewedPtr, buf: *mut u8) {
    let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    check(mp_init_copy(&mut tmp, n.as_bigint().mp_int_ptr()));
    bigint_leb128_encode_go(&mut tmp, buf, false)
}

#[no_mangle]
unsafe extern "C" fn bigint_2complement_bits(n: SkewedPtr) -> u32 {
    let mp_int = n.as_bigint().mp_int_ptr();
    if mp_isneg(mp_int) {
        let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
        check(mp_init_copy(&mut tmp, mp_int));
        check(mp_incr(&mut tmp));
        1 + mp_count_bits(&tmp) as u32
    } else {
        1 + mp_count_bits(mp_int) as u32
    }
}

#[no_mangle]
unsafe extern "C" fn bigint_sleb128_size(n: SkewedPtr) -> u32 {
    (bigint_2complement_bits(n) + 6) / 7 // divide by 7, ruond up
}

#[no_mangle]
unsafe extern "C" fn bigint_sleb128_encode(n: SkewedPtr, buf: *mut u8) {
    let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    check(mp_init_copy(&mut tmp, n.as_bigint().mp_int_ptr()));

    if mp_isneg(&tmp) {
        // Turn negatiave numbers into the two's complement of the right size
        let bytes = bigint_sleb128_size(n);
        let mut big: mp_int = core::mem::zeroed();
        check(mp_init(&mut big));
        check(mp_2expt(&mut big, 7 * bytes as i32));
        check(mp_add(&mut tmp, &big, &mut tmp));
        bigint_leb128_encode_go(&mut tmp, buf, false)
    } else {
        bigint_leb128_encode_go(&mut tmp, buf, true)
    }
}

#[no_mangle]
unsafe extern "C" fn bigint_leb128_decode(buf: *mut Buf) -> SkewedPtr {
    let r = bigint_alloc();

    let r_mp_int = r.as_bigint().mp_int_ptr();
    mp_zero(r_mp_int);

    let mut tmp: mp_int = core::mem::zeroed();
    check(mp_init(&mut tmp));

    let mut shift = 0;
    loop {
        let byte = read_byte(buf);
        mp_set_u32(&mut tmp, (byte & 0b0111_1111) as u32);
        check(mp_mul_2d(&mut tmp, shift, &mut tmp));
        check(mp_add(r_mp_int, &tmp, r_mp_int));
        shift += 7;

        if byte & 0b1000_0000 == 0 {
            break;
        }
    }

    r
}

#[no_mangle]
unsafe extern "C" fn bigint_sleb128_decode(buf: *mut Buf) -> SkewedPtr {
    let r = bigint_alloc();

    let r_mp_int = r.as_bigint().mp_int_ptr();
    mp_zero(r_mp_int);

    let mut tmp: mp_int = core::mem::zeroed();
    check(mp_init(&mut tmp));

    let mut shift = 0;
    let mut last_sign_bit_set;
    loop {
        let byte = read_byte(buf);
        mp_set_u32(&mut tmp, (byte & 0b0111_1111) as u32);
        check(mp_mul_2d(&mut tmp, shift, &mut tmp));
        check(mp_add(r_mp_int, &tmp, r_mp_int));
        last_sign_bit_set = byte & 0b0100_0000 != 0;
        shift += 7;

        if byte & 0b1000_0000 == 0 {
            break;
        }
    }

    if last_sign_bit_set {
        // Negative number, un-2-complement it
        let mut big: mp_int = core::mem::zeroed();
        check(mp_init(&mut big));
        check(mp_2expt(&mut big, shift));
        check(mp_sub(r_mp_int, &big, r_mp_int));
    }

    r
}
