use motoko_rts::buf::Buf;
use motoko_rts::leb128::{leb128_decode, leb128_encode, sleb128_decode, sleb128_encode};

use quickcheck::{quickcheck, TestResult};

pub unsafe fn test() {
    println!("Testing (s)leb128 encode-decode roundtrip ...");
    quickcheck(roundtrip_signed as fn(i32) -> TestResult);
    quickcheck(roundtrip_unsigned as fn(u32) -> TestResult);
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
