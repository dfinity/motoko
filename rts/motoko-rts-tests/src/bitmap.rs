use motoko_rts::bitmap::{alloc_bitmap, get_and_set_bit, get_bit, set_bit};
use motoko_rts::types::{Bytes, WORD_SIZE};

use quickcheck::{quickcheck, TestResult};

use std::collections::HashSet;

pub unsafe fn test() {
    println!("Testing bitmap ...");
    println!("  Testing set_bit/get_bit (QuickCheck)");
    test_set_get(vec![0, 33]).unwrap();
    quickcheck(test_set_get_qc as fn(Vec<u16>) -> TestResult);
    println!("  Testing get_or_set_bit (QuickCheck)");
    // quickcheck(test_get_and_set_bit as fn(HashSet<u16>) -> TestResult);
}

fn test_set_get_qc(bits: Vec<u16>) -> TestResult {
    match test_set_get(bits) {
        Ok(()) => TestResult::passed(),
        Err(err) => TestResult::error(&err),
    }
}

fn test_set_get(mut bits: Vec<u16>) -> Result<(), String> {
    if bits.is_empty() {
        return Ok(());
    }

    unsafe {
        alloc_bitmap(Bytes(
            u32::from(*bits.iter().max().unwrap() + 1) * WORD_SIZE,
        ));

        for bit in &bits {
            set_bit(*bit as u32);
            if !get_bit(*bit as u32) {
                return Err("set-get error".to_string());
            }
        }

        bits.sort();

        let mut last_bit: Option<u16> = None;
        for bit in bits {
            // Bits from the last set bit up to current bit should be 0
            if let Some(last_bit) = last_bit {
                for i in last_bit + 1..bit {
                    if get_bit(i as u32) {
                        return Err(format!("get_bit({}) of unset bit is true", i));
                    }
                }
            }

            // Current bit should be set
            if !get_bit(bit as u32) {
                return Err("get_bit of set bit is false".to_string());
            }

            last_bit = Some(bit);
        }
    }

    Ok(())
}

fn test_get_and_set_bit(bits: HashSet<u16>) -> TestResult {
    if bits.is_empty() {
        return TestResult::passed();
    }

    unsafe {
        alloc_bitmap(Bytes(
            u32::from(*bits.iter().max().unwrap() + 1) * WORD_SIZE,
        ));

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
