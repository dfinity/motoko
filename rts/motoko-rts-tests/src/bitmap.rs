use motoko_rts::bitmap::{alloc_bitmap, get_and_set_bit, get_bit, iter_bits, set_bit};
use motoko_rts::types::{Bytes, Words, WORD_SIZE};

use quickcheck::{quickcheck, TestResult};

use std::collections::HashSet;

pub unsafe fn test() {
    println!("Testing bitmap ...");
    println!("  Testing set_bit/get_bit");
    test_set_get(vec![0, 33]).unwrap();
    quickcheck(test_set_get_qc as fn(Vec<u16>) -> TestResult);
    println!("  Testing get_or_set_bit");
    quickcheck(test_get_and_set_bit as fn(HashSet<u16>) -> TestResult);
    println!("  Testing bit iteration");
    quickcheck(test_bit_iter as fn(HashSet<u16>) -> TestResult);
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
            set_bit(u32::from(*bit));
            if !get_bit(u32::from(*bit)) {
                return Err("set-get error".to_string());
            }
        }

        bits.sort();

        let mut last_bit: Option<u16> = None;
        for bit in bits {
            // Bits from the last set bit up to current bit should be 0
            if let Some(last_bit) = last_bit {
                for i in last_bit + 1..bit {
                    if get_bit(u32::from(i)) {
                        return Err(format!("get_bit({}) of unset bit is true", i));
                    }
                }
            }

            // Current bit should be set
            if !get_bit(u32::from(bit)) {
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
            if get_and_set_bit(u32::from(*bit)) {
                return TestResult::error("get_and_set_bit of unset bit is true");
            }

            if !get_bit(u32::from(*bit)) {
                return TestResult::error("get_bit of bit set with get_and_set_bit is false");
            }
        }
    }

    TestResult::passed()
}

fn test_bit_iter(bits: HashSet<u16>) -> TestResult {
    // If the max bit is N, the heap size is at least N+1 words
    let heap_size = Words(u32::from(
        bits.iter().max().map(|max_bit| max_bit + 1).unwrap_or(0),
    ))
    .to_bytes();

    unsafe {
        alloc_bitmap(heap_size);

        for bit in bits.iter() {
            set_bit(u32::from(*bit));
        }

        let mut bits_sorted = bits.into_iter().collect::<Vec<_>>();
        bits_sorted.sort();

        let mut bit_vec_iter = bits_sorted.into_iter();
        let mut bit_map_iter = iter_bits();

        while let Some(vec_bit) = bit_vec_iter.next() {
            match bit_map_iter.next() {
                None => {
                    return TestResult::error(
                        "bitmap iterator didn't yield but there are more bits",
                    );
                }
                Some(map_bit) => {
                    if map_bit != u32::from(vec_bit) {
                        return TestResult::error(&format!(
                            "bitmap iterator yields {}, but actual bit is {}",
                            map_bit, vec_bit
                        ));
                    }
                }
            }
        }

        if let Some(map_bit) = bit_map_iter.next() {
            return TestResult::error(&format!(
                "bitmap iterator yields {}, but there are no more bits left",
                map_bit
            ));
        }
    }

    TestResult::passed()
}
