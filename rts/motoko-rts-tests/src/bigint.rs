use crate::page_alloc::TestPageAlloc;

use motoko_rts::bigint::{self, *};
use motoko_rts::buf::Buf;
use motoko_rts::space::Space;
use motoko_rts::types::{Bytes, Value};

// mp functions below are implemented separately for tests as we can't modify mp_int source code to
// pass a page allocator/space for allocation and generate a monomorphic version for IC.

// Global variable for the allocation space as mp allocation routines are static functions and
// can't capture values
//
// TODO: Update Rust and use MaybeUninit here
static mut ALLOCATION_SPACE: Option<Space<TestPageAlloc>> = None;

#[no_mangle]
unsafe extern "C" fn mp_calloc(n_elems: usize, elem_size: Bytes<usize>) -> *mut libc::c_void {
    bigint::mp_calloc(ALLOCATION_SPACE.as_mut().unwrap(), n_elems, elem_size)
}

#[no_mangle]
unsafe extern "C" fn mp_realloc(
    ptr: *mut libc::c_void,
    old_size: Bytes<u32>,
    new_size: Bytes<u32>,
) -> *mut libc::c_void {
    bigint::mp_realloc(ALLOCATION_SPACE.as_mut().unwrap(), ptr, old_size, new_size)
}

pub unsafe fn test() {
    println!("Testing BigInt ...");

    let page_alloc = TestPageAlloc::new(1024);
    ALLOCATION_SPACE = Some(Space::new(page_alloc));

    assert!(bigint_eq(
        bigint_pow(bigint_of_word32(70), bigint_of_word32(32)),
        bigint_mul(
            bigint_pow(bigint_of_word32(70), bigint_of_word32(31)),
            bigint_of_word32(70)
        )
    ));

    //
    // (s)leb128 encoding
    //

    let one = bigint_of_word32(1);
    let two = bigint_of_word32(2);
    for i in 0..100 {
        let two_pow_i = bigint_pow(two, bigint_of_word32(i));
        let minus_one = bigint_sub(two_pow_i, one);
        let plus_one = bigint_add(two_pow_i, one);

        test_bigint_leb128(minus_one);
        test_bigint_leb128(two_pow_i);
        test_bigint_leb128(plus_one);

        test_bigint_sleb128(minus_one);
        test_bigint_sleb128(two_pow_i);
        test_bigint_sleb128(plus_one);
        test_bigint_sleb128(bigint_neg(minus_one));
        test_bigint_sleb128(bigint_neg(two_pow_i));
        test_bigint_sleb128(bigint_neg(plus_one));
    }

    let mut space = ALLOCATION_SPACE.take().unwrap();
    space.free();
}

// Check leb128 encode/decode roundtrip
unsafe fn test_bigint_leb128(n: Value) {
    let mut buf = [0u8; 100];
    let s = bigint_leb128_size(n);
    let mut buf_ = Buf {
        ptr: buf.as_mut_ptr(),
        end: buf.as_mut_ptr().add(100),
    };
    bigint_leb128_encode(n, buf.as_mut_ptr());
    let n2 = bigint_leb128_decode(&mut buf_ as *mut _);
    assert!(bigint_eq(n, n2));
    assert_eq!(buf_.ptr.offset_from(buf.as_ptr()), s as isize);
}

// Check sleb128 encode/decode roundtrip
unsafe fn test_bigint_sleb128(n: Value) {
    let mut buf = [0u8; 100];
    let s = bigint_sleb128_size(n);
    bigint_sleb128_encode(n, buf.as_mut_ptr());
    let mut buf_ = Buf {
        ptr: buf.as_mut_ptr(),
        end: buf.as_mut_ptr().add(100),
    };
    let n2 = bigint_sleb128_decode(&mut buf_ as *mut _);
    assert!(bigint_eq(n, n2));
    assert_eq!(buf_.ptr.offset_from(buf.as_ptr()), s as isize);
}
