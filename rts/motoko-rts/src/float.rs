/*
#[no_mangle]
unsafe extern "C" fn float_fmt(a: f64, prec: u32, mode: u32) {
    // prec and mode are passed tagged (TODO (osa): What tag??????)
    let mode = mode >> 24;
    let prec = core::cmp::min(prec >> 24, 100);


}
*/
