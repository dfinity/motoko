#![allow(non_camel_case_types, dead_code, unused_variables)]

pub(crate) type size_t = usize;
pub(crate) type c_char = i8;
pub(crate) type c_int = i32;

pub type mp_digit = u32;
pub type mp_sign = c_int;
pub type mp_ord = c_int;
pub type mp_err = c_int;

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct mp_int {
    pub used: c_int,
    pub alloc: c_int,
    pub sign: mp_sign,
    pub dp: *mut mp_digit,
}

#[no_mangle]
pub extern "C" fn mp_init(a: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_get_double(a: *const mp_int) -> f64 {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_set_double(a: *mut mp_int, b: f64) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_get_i32(a: *const mp_int) -> i32 {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_set_i32(a: *mut mp_int, b: i32) {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_set_u32(a: *mut mp_int, b: u32) {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_get_i64(a: *const mp_int) -> i64 {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_set_i64(a: *mut mp_int, b: i64) {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_set_u64(a: *mut mp_int, b: u64) {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_init_copy(a: *mut mp_int, b: *const mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_div_2d(
        a: *const mp_int,
        b: c_int,
        c: *mut mp_int,
        d: *mut mp_int,
    ) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_mul_2d(
        a: *const mp_int,
        b: c_int,
        c: *mut mp_int,
    ) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_2expt(a: *mut mp_int, b: c_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_neg(a: *const mp_int, b: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_abs(a: *const mp_int, b: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_cmp(a: *const mp_int, b: *const mp_int) -> mp_ord {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_add(a: *const mp_int, b: *const mp_int, c: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_sub(a: *const mp_int, b: *const mp_int, c: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_mul(a: *const mp_int, b: *const mp_int, c: *mut mp_int) -> mp_err {
    unimplemented!()    
}

#[no_mangle]
pub extern "C" fn mp_div(a: *const mp_int, b: *const mp_int, c: *mut mp_int, d: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_incr(a: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_expt_u32(a: *const mp_int, b: u32, c: *mut mp_int) -> mp_err {
    unimplemented!()
}

#[no_mangle]
pub extern "C" fn mp_count_bits(a: *const mp_int) -> c_int {
    unimplemented!()
}
