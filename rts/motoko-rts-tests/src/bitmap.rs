use motoko_rts::bitmap::{alloc_bitmap, free_bitmap, get_bit, set_bit};

use quickcheck::{quickcheck, TestResult};

pub unsafe fn test() {
    println!("Testing bitmap ... (QuickCheck)");
    quickcheck(test_ as fn(Vec<u16>) -> TestResult);
}

fn test_(mut bits: Vec<u16>) -> TestResult {
    unsafe {
        free_bitmap();
        alloc_bitmap();

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
