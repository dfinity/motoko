use motoko_rts::bitmap::{get_and_set_bit, get_bit, set_bit, BITMAP_PTR};

use quickcheck::{quickcheck, TestResult};

use std::collections::HashSet;

pub unsafe fn test() {
    println!("Testing bitmap ...");
    println!("  Testing set_bit/get_bit (QuickCheck)");
    quickcheck(test_set_get as fn(Vec<u16>) -> TestResult);
    println!("  Testing get_or_set_bit (QuickCheck)");
    quickcheck(test_get_and_set_bit as fn(HashSet<u16>) -> TestResult);
}

fn test_set_get(mut bits: Vec<u16>) -> TestResult {
    let mut vec: Vec<u8> = vec![0u8; 65535];

    unsafe {
        BITMAP_PTR = vec.as_mut_ptr();

        for bit in &bits {
            set_bit(*bit as u32);
            if !get_bit(*bit as u32) {
                return TestResult::error("set-get error");
            }
        }

        bits.sort();
        let mut last_bit: Option<u16> = None;
        for bit in bits {
            // Bits from the last set bit up to current bit should be 0
            if let Some(last_bit) = last_bit {
                for i in last_bit + 1..bit {
                    if get_bit(i as u32) {
                        return TestResult::error("get_bit of unset bit is true");
                    }
                }
            }

            // Current bit should be set
            if !get_bit(bit as u32) {
                return TestResult::error("get_bit of set bit is false");
            }

            last_bit = Some(bit);
        }
    }

    TestResult::passed()
}

fn test_get_and_set_bit(bits: HashSet<u16>) -> TestResult {
    let mut vec: Vec<u8> = vec![0u8; 65535];

    unsafe {
        BITMAP_PTR = vec.as_mut_ptr();

        for bit in bits.iter() {
            if get_and_set_bit(*bit as u32) {
                return TestResult::error("get_and_set_bit of unset bit is true");
            }

            if !get_bit(*bit as u32) {
                return TestResult::error("get_bit of bit set with get_and_set_bit is false");
            }
        }
    }

    TestResult::passed()
}
