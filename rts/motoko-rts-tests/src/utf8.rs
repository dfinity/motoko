// NOTE (osa): These tests are ported from the C version of the RTS, we could probably remove this
// now as we use Rust's UTF-8 validation now.

use motoko_rts::utf8::utf8_valid;

static TEST_STRS_VALID: [&[u8]; 2] = [
    b"abcd",
    // issue 1208
    b" \xe2\x96\x88 ",
];

static TEST_STRS_INVALID: [&[u8]; 32] = [
    // from https://www.cl.cam.ac.uk/~mgk25/ucs/examples/UTF-8-test.txt
    //
    // 3.5  Impossible bytes
    b"\xfe",
    b"\xff",
    // 4.1  Examples of an overlong ASCII character
    b"\xc0\xaf",
    b"\xe0\x80\xaf",
    b"\xf0\x80\x80\xaf",
    b"\xf8\x80\x80\x80\xaf",
    b"\xfc\x80\x80\x80\x80\xaf",
    // 4.2  Maximum overlong sequences
    b"\xc1\xbf",
    b"\xe0\x9f\xbf",
    b"\xf0\x8f\xbf\xbf",
    b"\xf8\x87\xbf\xbf\xbf",
    b"\xfc\x83\xbf\xbf\xbf\xbf",
    // 4.3  Overlong representation of the NUL character
    b"\xc0\x80",
    b"\xe0\x80\x80",
    b"\xf0\x80\x80\x80",
    b"\xf8\x80\x80\x80\x80",
    b"\xfc\x80\x80\x80\x80\x80",
    // 5.1 Single UTF-16 surrogates
    b"\xed\xa0\x80",
    b"\xed\xad\xbf",
    b"\xed\xae\x80",
    b"\xed\xaf\xbf",
    b"\xed\xb0\x80",
    b"\xed\xbe\x80",
    b"\xed\xbf\xbf",
    // 5.2 Paired UTF-16 surrogates
    b"\xed\xa0\x80\xed\xb0\x80",
    b"\xed\xa0\x80\xed\xbf\xbf",
    b"\xed\xad\xbf\xed\xb0\x80",
    b"\xed\xad\xbf\xed\xbf\xbf",
    b"\xed\xae\x80\xed\xb0\x80",
    b"\xed\xae\x80\xed\xbf\xbf",
    b"\xed\xaf\xbf\xed\xb0\x80",
    b"\xed\xaf\xbf\xed\xbf\xbf",
];

pub unsafe fn test() {
    println!("Testing UTF8 validation ...");

    for test_str in TEST_STRS_VALID.iter() {
        assert!(utf8_valid(
            test_str.as_ptr() as *const _,
            test_str.len() as u32
        ));
    }

    for test_str in TEST_STRS_INVALID.iter() {
        assert!(!utf8_valid(
            test_str.as_ptr() as *const _,
            test_str.len() as u32
        ));
    }
}
