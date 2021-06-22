use motoko_rts::buf::Buf;
use motoko_rts::leb128::{leb128_decode, leb128_encode, sleb128_decode, sleb128_encode};

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing (s)leb128 encode-decode roundtrip ...");

    let mut proptest_runner = TestRunner::new(Config {
        cases: 1_000,
        failure_persistence: None, // TODO: I don't know what this is about, but it generates a
        // warning in runtime
        ..Default::default()
    });

    proptest_runner
        .run(&proptest::num::i32::ANY, roundtrip_signed)
        .unwrap();
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

        if sleb128_decode(&mut buf_) == val {
            Ok(())
        } else {
            Err(TestCaseError::Fail(
                "Encode-decode roundtrip gives different value".into(),
            ))
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

        if leb128_decode(&mut buf_) == val {
            Ok(())
        } else {
            Err(TestCaseError::Fail(
                "Encode-decode roundtrip gives different value".into(),
            ))
        }
    }
}
