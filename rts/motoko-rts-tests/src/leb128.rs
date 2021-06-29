use motoko_rts::buf::Buf;
use motoko_rts::leb128::{
    leb128_decode, leb128_decode_checked, leb128_encode, sleb128_decode, sleb128_decode_checked,
    sleb128_encode,
};

use quickcheck::{quickcheck, TestResult};

pub unsafe fn test() {
    println!("Testing (s)leb128 encode-decode roundtrip ...");

    assert!(!roundtrip_signed(i32::MIN).is_failure());
    assert!(!roundtrip_signed(i32::MAX).is_failure());

    assert!(!roundtrip_unsigned(u32::MIN).is_failure());
    assert!(!roundtrip_unsigned(u32::MAX).is_failure());

    quickcheck(roundtrip_signed as fn(i32) -> TestResult);
    quickcheck(roundtrip_unsigned as fn(u32) -> TestResult);

    // Check overflows
    check_signed_decode_overflow(&[
        0b1111_1111,
        0b1111_1111,
        0b1111_1111,
        0b1111_1111,
        0b0111_0111,
    ]); // i32::MIN - 1

    check_signed_decode_overflow(&[
        0b1000_0000,
        0b1000_0000,
        0b1000_0000,
        0b1000_0000,
        0b0000_1000,
    ]); // i32::MAX + 1

    check_unsigned_decode_overflow(&[
        0b1000_0000,
        0b1000_0000,
        0b1000_0000,
        0b1000_0000,
        0b0001_0000,
    ]); // u32::MAX + 1
}

fn roundtrip_signed(val: i32) -> TestResult {
    unsafe {
        let mut buf = [0u8; 100];
        sleb128_encode(val, buf.as_mut_ptr());

        let mut buf_ = Buf {
            ptr: buf.as_mut_ptr(),
            end: buf.as_mut_ptr().add(100),
        };

        TestResult::from_bool(sleb128_decode(&mut buf_) == val)
    }
}

fn roundtrip_unsigned(val: u32) -> TestResult {
    unsafe {
        let mut buf = [0u8; 100];
        leb128_encode(val, buf.as_mut_ptr());

        let mut buf_ = Buf {
            ptr: buf.as_mut_ptr(),
            end: buf.as_mut_ptr().add(100),
        };

        TestResult::from_bool(leb128_decode(&mut buf_) == val)
    }
}

unsafe fn check_signed_decode_overflow(buf: &[u8]) {
    let mut buf_ = Buf {
        ptr: buf.as_ptr() as *mut _,
        end: buf.as_ptr().add(buf.len()) as *mut _,
    };

    assert_eq!(sleb128_decode_checked(&mut buf_), None);
}

unsafe fn check_unsigned_decode_overflow(buf: &[u8]) {
    let mut buf_ = Buf {
        ptr: buf.as_ptr() as *mut _,
        end: buf.as_ptr().add(buf.len()) as *mut _,
    };

    assert_eq!(leb128_decode_checked(&mut buf_), None);
}
