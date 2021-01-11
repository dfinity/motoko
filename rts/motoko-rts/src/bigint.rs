//! Implements big int support:
//!
//! - libtommath memory management
//! - libtommath wrappers
//! - (s)leb128 encoding/decoding for bigints

/*
A libtommath arbitrary precision integer is a struct (`mp_int`) that contains a pointer to a data
array.

 - The libtommath library never allocates the struct, so we are in full control. We embed the
   struct in `BigInt` heap objects.

 - The data array is allocated with `mp_calloc` and `mp_realloc`. We provide these calls, allocate
   Motoko blobs and store the pointer in the `BigInt`s `mp_int_dp` field.

   When calling a libtommath function, we allocate a `mp_int` on stack with correct `dp` pointer
   (that points to the `Blob`'s payload) and pass the pointer to the struct to the library
   function. On return we update the `BigInt` fields with the fields of stack-allocated `mp_int`.
   This is done in `BigInt::with_mp_int_ptr`.
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
    let blob = (ptr as *mut Blob).sub(1);

    debug_assert_eq!((*blob).header.tag, TAG_BLOB);
    debug_assert_eq!(blob.len(), old_size);

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

unsafe fn check(err: mp_err) -> Result<(), ()> {
    if err == 0 {
        Ok(())
    } else {
        Err(())
    }
}

unsafe fn trap_on_err<A>(ret: Result<A, ()>) -> A {
    match ret {
        Ok(value) => value,
        Err(()) => bigint_trap(),
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

unsafe fn bigint_alloc() -> SkewedPtr {
    let r = alloc_words(size_of::<BigInt>());

    let r_ptr = r.unskew() as *mut BigInt;
    (*r_ptr).header.tag = TAG_BIGINT;
    (*r_ptr).mp_int_used = 0;
    (*r_ptr).mp_int_alloc = 0;
    (*r_ptr).mp_int_sign = 0;
    (*r_ptr).mp_int_dp = SkewedPtr(0);

    match check((r_ptr as *mut BigInt).with_mut_mp_int_ptr(|p| mp_init(p))) {
        Ok(()) => r,
        Err(()) => bigint_trap(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn bigint_of_word32(w: u32) -> SkewedPtr {
    let r = bigint_alloc();
    r.as_bigint().with_mut_mp_int_ptr(|p| mp_set_u32(p, w));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_of_word32_signed(i: i32) -> SkewedPtr {
    let r = bigint_alloc();
    r.as_bigint().with_mut_mp_int_ptr(|p| mp_set_i32(p, i));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word32_wrap(p: SkewedPtr) -> u32 {
    p.as_bigint().with_mp_int_ptr(|p| mp_get_u32(p))
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word32_trap(p: SkewedPtr) -> u32 {
    trap_on_err(p.as_bigint().with_mp_int_ptr(|p| {
        if mp_isneg(p) || mp_count_bits(p) > 32 {
            Err(())
        } else {
            Ok(mp_get_u32(p))
        }
    }))
}

// a : BigInt, msg : Blob
#[no_mangle]
unsafe extern "C" fn bigint_to_word32_trap_with(p: SkewedPtr, msg: SkewedPtr) -> u32 {
    let ret = p.as_bigint().with_mp_int_ptr(|p| {
        if mp_isneg(p) || mp_count_bits(p) > 32 {
            Err(())
        } else {
            Ok(mp_get_u32(p))
        }
    });

    match ret {
        Ok(value) => value,
        Err(()) => rts_trap(msg.as_blob().payload_addr(), msg.as_blob().len()),
    }
}

// TODO (osa): I don't understand the code below, it uses get_mag (magnitude?) but the name
// suggests it's to convert bigint to u32?
#[no_mangle]
unsafe extern "C" fn bigint_to_word32_signed_trap(p: SkewedPtr) -> i32 {
    trap_on_err(p.as_bigint().with_mp_int_ptr(|p| {
        if mp_count_bits(p) > 32 {
            return Err(());
        }

        let x = mp_get_mag_u32(p) as i32;
        if mp_isneg(p) {
            let x = -x;
            if x >= 0 {
                // TODO (osa): Why not ==?
                Err(())
            } else {
                Ok(x)
            }
        } else {
            if x < 0 {
                Err(())
            } else {
                Ok(x)
            }
        }
    }))
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word64_wrap(p: SkewedPtr) -> u64 {
    p.as_bigint().with_mp_int_ptr(|p| mp_get_u64(p))
}

#[no_mangle]
unsafe extern "C" fn bigint_to_word64_trap(p: SkewedPtr) -> u64 {
    trap_on_err(p.as_bigint().with_mp_int_ptr(|p| {
        if mp_isneg(p) || mp_count_bits(p) > 64 {
            Err(())
        } else {
            Ok(mp_get_u64(p))
        }
    }))
}

// TODO (osa): Same as bigint_to_word32_signed_trap
#[no_mangle]
unsafe extern "C" fn bigint_to_word64_signed_trap(p: SkewedPtr) -> i64 {
    trap_on_err(p.as_bigint().with_mp_int_ptr(|p| {
        if mp_count_bits(p) > 64 {
            return Err(());
        }

        let x = mp_get_mag_u64(p) as i64;
        if mp_isneg(p) {
            let x = -x;
            if x >= 0 {
                // TODO (osa): Why not ==?
                Err(())
            } else {
                Ok(x)
            }
        } else {
            if x < 0 {
                Err(())
            } else {
                Ok(x)
            }
        }
    }))
}

#[no_mangle]
unsafe extern "C" fn bigint_of_word64(w: u64) -> SkewedPtr {
    let p = bigint_alloc();
    p.as_bigint().with_mut_mp_int_ptr(|p| mp_set_u64(p, w));
    p
}

// TODO (osa): Rename to int64 for consistency?
#[no_mangle]
unsafe extern "C" fn bigint_of_word64_signed(i: i64) -> SkewedPtr {
    let p = bigint_alloc();
    p.as_bigint().with_mut_mp_int_ptr(|p| mp_set_i64(p, i));
    p
}

#[no_mangle]
pub unsafe extern "C" fn bigint_eq(a: SkewedPtr, b: SkewedPtr) -> bool {
    a.as_bigint()
        .with_mp_int_ptr(|a| b.as_bigint().with_mp_int_ptr(|b| mp_cmp(a, b) == 0))
}

#[no_mangle]
unsafe extern "C" fn bigint_lt(a: SkewedPtr, b: SkewedPtr) -> bool {
    a.as_bigint()
        .with_mp_int_ptr(|a| b.as_bigint().with_mp_int_ptr(|b| mp_cmp(a, b) < 0))
}

#[no_mangle]
unsafe extern "C" fn bigint_gt(a: SkewedPtr, b: SkewedPtr) -> bool {
    a.as_bigint()
        .with_mp_int_ptr(|a| b.as_bigint().with_mp_int_ptr(|b| mp_cmp(a, b) > 0))
}

#[no_mangle]
unsafe extern "C" fn bigint_le(a: SkewedPtr, b: SkewedPtr) -> bool {
    a.as_bigint()
        .with_mp_int_ptr(|a| b.as_bigint().with_mp_int_ptr(|b| mp_cmp(a, b) <= 0))
}

#[no_mangle]
unsafe extern "C" fn bigint_ge(a: SkewedPtr, b: SkewedPtr) -> bool {
    a.as_bigint()
        .with_mp_int_ptr(|a| b.as_bigint().with_mp_int_ptr(|b| mp_cmp(a, b) >= 0))
}

#[no_mangle]
pub unsafe extern "C" fn bigint_add(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        b.as_bigint()
            .with_mp_int_ptr(|b| r.as_bigint().with_mut_mp_int_ptr(|r| mp_add(a, b, r)))
    })));
    r
}

#[no_mangle]
pub unsafe extern "C" fn bigint_sub(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        b.as_bigint()
            .with_mp_int_ptr(|b| r.as_bigint().with_mut_mp_int_ptr(|r| mp_sub(a, b, r)))
    })));
    r
}

#[no_mangle]
pub unsafe extern "C" fn bigint_mul(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        b.as_bigint()
            .with_mp_int_ptr(|b| r.as_bigint().with_mut_mp_int_ptr(|r| mp_mul(a, b, r)))
    })));
    r
}

#[no_mangle]
pub unsafe extern "C" fn bigint_pow(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let exp = bigint_to_word32_trap(b);
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        r.as_bigint()
            .with_mut_mp_int_ptr(|r| mp_expt_u32(a, exp, r))
    })));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_div(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        b.as_bigint().with_mp_int_ptr(|b| {
            r.as_bigint()
                .with_mut_mp_int_ptr(|r| mp_div(a, b, r, core::ptr::null_mut()))
        })
    })));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_rem(a: SkewedPtr, b: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        b.as_bigint().with_mp_int_ptr(|b| {
            r.as_bigint()
                .with_mut_mp_int_ptr(|r| mp_div(a, b, core::ptr::null_mut(), r))
        })
    })));
    r
}

#[no_mangle]
pub unsafe extern "C" fn bigint_neg(a: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        r.as_bigint().with_mut_mp_int_ptr(|r| mp_neg(a, r))
    })));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_abs(a: SkewedPtr) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        r.as_bigint().with_mut_mp_int_ptr(|r| mp_abs(a, r))
    })));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_isneg(a: SkewedPtr) -> bool {
    a.as_bigint().with_mp_int_ptr(|a| mp_isneg(a))
}

#[no_mangle]
unsafe extern "C" fn bigint_lsh(a: SkewedPtr, b: i32) -> SkewedPtr {
    let r = bigint_alloc();
    trap_on_err(check(a.as_bigint().with_mp_int_ptr(|a| {
        r.as_bigint().with_mut_mp_int_ptr(|r| mp_mul_2d(a, b, r))
    })));
    r
}

#[no_mangle]
unsafe extern "C" fn bigint_count_bits(a: SkewedPtr) -> i32 {
    a.as_bigint().with_mp_int_ptr(|a| mp_count_bits(a))
}

#[no_mangle]
pub unsafe extern "C" fn bigint_leb128_size(a: SkewedPtr) -> u32 {
    a.as_bigint().with_mp_int_ptr(|a| {
        if mp_iszero(a) {
            1
        } else {
            (mp_count_bits(a) as u32 + 6) / 7 // divide by 7, round up
        }
    })
}

// TODO (osa): I don't understand what `add_bit` means
unsafe fn bigint_leb128_encode_go(tmp: *mut mp_int, mut buf: *mut u8, add_bit: bool) {
    if mp_isneg(tmp) {
        bigint_trap();
    }

    loop {
        let byte = mp_get_u32(tmp) as u8;
        trap_on_err(check(mp_div_2d(tmp, 7, tmp, core::ptr::null_mut())));
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
pub unsafe extern "C" fn bigint_leb128_encode(n: SkewedPtr, buf: *mut u8) {
    let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    trap_on_err(check(
        n.as_bigint().with_mp_int_ptr(|n| mp_init_copy(&mut tmp, n)),
    ));
    bigint_leb128_encode_go(&mut tmp, buf, false)
}

#[no_mangle]
unsafe extern "C" fn bigint_2complement_bits(n: SkewedPtr) -> u32 {
    trap_on_err(n.as_bigint().with_mp_int_ptr(|n| {
        if mp_isneg(n) {
            let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
            check(mp_init_copy(&mut tmp, n))?;
            check(mp_incr(&mut tmp))?;
            Ok(1 + mp_count_bits(&tmp) as u32)
        } else {
            Ok(1 + mp_count_bits(n) as u32)
        }
    }))
}

#[no_mangle]
pub unsafe extern "C" fn bigint_sleb128_size(n: SkewedPtr) -> u32 {
    (bigint_2complement_bits(n) + 6) / 7 // divide by 7, ruond up
}

#[no_mangle]
pub unsafe extern "C" fn bigint_sleb128_encode(n: SkewedPtr, buf: *mut u8) {
    let mut tmp: mp_int = core::mem::zeroed(); // or core::mem::uninitialized?
    trap_on_err(check(
        n.as_bigint().with_mp_int_ptr(|n| mp_init_copy(&mut tmp, n)),
    ));

    if mp_isneg(&tmp) {
        // Turn negatiave numbers into the two's complement of the right size
        let bytes = bigint_sleb128_size(n);
        let mut big: mp_int = core::mem::zeroed();
        trap_on_err(check(mp_init(&mut big)));
        trap_on_err(check(mp_2expt(&mut big, 7 * bytes as i32)));
        trap_on_err(check(mp_add(&mut tmp, &big, &mut tmp)));
        bigint_leb128_encode_go(&mut tmp, buf, false)
    } else {
        bigint_leb128_encode_go(&mut tmp, buf, true)
    }
}

#[no_mangle]
pub unsafe extern "C" fn bigint_leb128_decode(buf: *mut Buf) -> SkewedPtr {
    let r = bigint_alloc();

    trap_on_err(r.as_bigint().with_mut_mp_int_ptr(|r| {
        mp_zero(r);

        let mut tmp: mp_int = core::mem::zeroed();
        check(mp_init(&mut tmp))?;

        let mut shift = 0;
        loop {
            let byte = read_byte(buf);
            mp_set_u32(&mut tmp, (byte & 0b0111_1111) as u32);
            check(mp_mul_2d(&mut tmp, shift, &mut tmp))?;
            check(mp_add(r, &tmp, r))?;
            shift += 7;

            if byte & 0b1000_0000 == 0 {
                break;
            }
        }

        Ok(())
    }));

    r
}

#[no_mangle]
pub unsafe extern "C" fn bigint_sleb128_decode(buf: *mut Buf) -> SkewedPtr {
    let r = bigint_alloc();

    trap_on_err(r.as_bigint().with_mut_mp_int_ptr(|r| {
        mp_zero(r);

        let mut tmp: mp_int = core::mem::zeroed();
        check(mp_init(&mut tmp))?;

        let mut shift = 0;
        let mut last_sign_bit_set;
        loop {
            let byte = read_byte(buf);
            mp_set_u32(&mut tmp, (byte & 0b0111_1111) as u32);
            check(mp_mul_2d(&mut tmp, shift, &mut tmp))?;
            check(mp_add(r, &tmp, r))?;
            last_sign_bit_set = byte & 0b0100_0000 != 0;
            shift += 7;

            if byte & 0b1000_0000 == 0 {
                break;
            }
        }

        if last_sign_bit_set {
            // Negative number, un-2-complement it
            let mut big: mp_int = core::mem::zeroed();
            check(mp_init(&mut big))?;
            check(mp_2expt(&mut big, shift))?;
            check(mp_sub(r, &big, r))?;
        }

        Ok(())
    }));

    r
}
