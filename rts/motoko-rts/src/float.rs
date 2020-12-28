use crate::text::text_of_ptr_size;
use crate::types::{Bytes, SkewedPtr};

#[no_mangle]
unsafe extern "C" fn float_fmt(a: f64, prec: u32, mode: u32) -> SkewedPtr {
    // prec and mode are tagged (TODO (osa): what tag???)
    let mode = mode >> 24;
    let prec = core::cmp::min(prec >> 24, 100) as usize;

    // 110 bytes needed for max precision (TODO (osa): why? how?)
    let buf = [0u8; 120];

    // TODO (osa): Where are these modes defined?
    // NB. Using snprintf because I think only 0 and 3 are supposed by Rust's built-in formatter
    let fmt = match mode {
        0 => "%.*f\0",
        1 => "%.*e\0",
        2 => "%.*g\0",
        3 => "%.*a\0",
        _ => panic!("float_fmt: unrecognized mode"),
    };

    let n_written = libc::snprintf(
        buf.as_ptr() as *mut _,
        120,
        fmt.as_ptr() as *const _,
        prec,
        a as libc::c_double,
    );

    assert!(n_written > 0);

    text_of_ptr_size(buf.as_ptr(), Bytes(n_written as u32))
}

#[no_mangle]
unsafe extern "C" fn float_pow(a: f64, b: f64) -> f64 {
    core::intrinsics::powf64(a, b)
}

#[no_mangle]
unsafe extern "C" fn float_sin(a: f64) -> f64 {
    core::intrinsics::sinf64(a)
}

#[no_mangle]
unsafe extern "C" fn float_cos(a: f64) -> f64 {
    core::intrinsics::cosf64(a)
}

#[no_mangle]
unsafe extern "C" fn float_exp(a: f64) -> f64 {
    core::intrinsics::expf64(a)
}

#[no_mangle]
unsafe extern "C" fn float_log(a: f64) -> f64 {
    core::intrinsics::logf64(a)
}

#[no_mangle]
unsafe extern "C" fn float_rem(a: f64, b: f64) -> f64 {
    a % b
}

#[no_mangle]
unsafe extern "C" fn float_tan(a: f64) -> f64 {
    extern "C" {
        fn tan(a: f64) -> f64;
    }
    tan(a)
}

#[no_mangle]
unsafe extern "C" fn float_arcsin(a: f64) -> f64 {
    extern "C" {
        fn asin(a: f64) -> f64;
    }
    asin(a)
}

#[no_mangle]
unsafe extern "C" fn float_arccos(a: f64) -> f64 {
    extern "C" {
        fn acos(a: f64) -> f64;
    }
    acos(a)
}

#[no_mangle]
unsafe extern "C" fn float_arctan(a: f64) -> f64 {
    extern "C" {
        fn atan(a: f64) -> f64;
    }
    atan(a)
}

#[no_mangle]
unsafe extern "C" fn float_arctan2(a: f64, b: f64) -> f64 {
    extern "C" {
        fn atan2(a: f64, b: f64) -> f64;
    }
    atan2(a, b)
}
