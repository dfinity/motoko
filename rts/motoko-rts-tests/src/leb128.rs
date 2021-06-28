use motoko_rts::buf::Buf;
use motoko_rts::leb128::{
    leb128_decode_checked, leb128_encode, sleb128_decode_checked, sleb128_encode,
};

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing (s)leb128 encode-decode roundtrip ...");

    let mut proptest_runner = TestRunner::new(Config {
        cases: 10_000,
        failure_persistence: None, // TODO: I don't know what this is about, but it generates a
        // warning in runtime
        ..Default::default()
    });

    // TODO: Some of the tests below are disabled as sleb128_decode is buggy: #2625

    roundtrip_signed(1).unwrap();
    roundtrip_signed(0).unwrap();
    // roundtrip_signed(-1).unwrap();
    // roundtrip_signed(i32::MIN).unwrap(); // -2147483648
    // roundtrip_signed(i32::MAX).unwrap(); // 2147483647

    // proptest_runner
    //     .run(&proptest::num::i32::ANY, roundtrip_signed)
    //     .unwrap();

    roundtrip_unsigned(1).unwrap();
    roundtrip_unsigned(0).unwrap();
    roundtrip_unsigned(u32::MIN).unwrap();
    roundtrip_unsigned(u32::MAX).unwrap();

    proptest_runner
        .run(&proptest::num::u32::ANY, roundtrip_unsigned)
        .unwrap();
}

fn roundtrip_signed(val: i32) -> TestCaseResult {
    unsafe {
        let mut buf = [0u8; 100];
        sleb128_encode(val, buf.as_mut_ptr());

        let mut buf_ = Buf {
            ptr: buf.as_mut_ptr(),
            end: buf.as_mut_ptr().add(100),
        };

        match sleb128_decode_checked(&mut buf_) {
            None => Err(TestCaseError::Fail(
                format!("sleb128 decoding of {} overflowed", val).into(),
            )),
            Some(val_) => {
                if val_ == val {
                    Ok(())
                } else {
                    Err(TestCaseError::Fail(
                        format!("Encode-decode roundtrip gives different value for {}", val).into(),
                    ))
                }
            }
        }
    }
}

fn roundtrip_unsigned(val: u32) -> TestCaseResult {
    unsafe {
        let mut buf = [0u8; 100];
        leb128_encode(val, buf.as_mut_ptr());

        let mut buf_ = Buf {
            ptr: buf.as_mut_ptr(),
            end: buf.as_mut_ptr().add(100),
        };

        match leb128_decode_checked(&mut buf_) {
            None => Err(TestCaseError::Fail(
                format!("leb128 decoding of {} overflowed", val).into(),
            )),
            Some(val_) => {
                if val_ == val {
                    Ok(())
                } else {
                    Err(TestCaseError::Fail(
                        format!("Encode-decode roundtrip gives different value for {}", val).into(),
                    ))
                }
            }
        }
    }
}
