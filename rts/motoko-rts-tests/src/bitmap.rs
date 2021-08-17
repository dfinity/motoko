use crate::page_alloc::TestPageAlloc;

use motoko_rts::bitmap::{Bitmap, BITMAP_ITER_END};
use motoko_rts::constants::WORD_SIZE;
use motoko_rts::types::{Bytes, Words};

use std::collections::HashSet;

use proptest::strategy::Strategy;
use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing bitmap ...");
    println!("  Testing set_bit/get_bit");

    test_set_get(vec![0, 33]).unwrap();

    let mut proptest_runner = TestRunner::new(Config {
        cases: 100,
        failure_persistence: None,
        ..Default::default()
    });

    proptest_runner
        .run(&bit_index_vec_strategy(), test_set_get_proptest)
        .unwrap();

    println!("  Testing bit iteration");
    proptest_runner
        .run(&bit_index_set_strategy(), test_bit_iter)
        .unwrap();
}

/// Generates vectors of bit indices
fn bit_index_vec_strategy() -> impl Strategy<Value = Vec<u16>> {
    proptest::collection::vec(0u16..u16::MAX, 0..1_000)
}

/// Same as `bit_index_vec_strategy`, but generates sets
fn bit_index_set_strategy() -> impl Strategy<Value = HashSet<u16>> {
    proptest::collection::hash_set(0u16..u16::MAX, 0..1_000)
}

fn test_set_get_proptest(bits: Vec<u16>) -> TestCaseResult {
    test_set_get(bits).map_err(|err| TestCaseError::Fail(err.into()))
}

fn test_set_get(mut bits: Vec<u16>) -> Result<(), String> {
    if bits.is_empty() {
        return Ok(());
    }

    unsafe {
        let mut bitmap = Bitmap::new((u32::from(*bits.iter().max().unwrap()) + 1));

        for bit in &bits {
            (&mut bitmap as *mut Bitmap).set(u32::from(*bit));
            if !(&mut bitmap as *mut Bitmap).get(u32::from(*bit)) {
                return Err("set-get error".to_string());
            }
        }

        bits.sort();

        let mut last_bit: Option<u16> = None;
        for bit in bits {
            // Bits from the last set bit up to current bit should be 0
            if let Some(last_bit) = last_bit {
                for i in last_bit + 1..bit {
                    if (&mut bitmap as *mut Bitmap).get(u32::from(i)) {
                        return Err(format!("get_bit({}) of unset bit is true", i));
                    }
                }
            }

            // Current bit should be set
            if !(&mut bitmap as *mut Bitmap).get(u32::from(bit)) {
                return Err("get_bit of set bit is false".to_string());
            }

            last_bit = Some(bit);
        }
    }

    Ok(())
}

fn test_bit_iter(bits: HashSet<u16>) -> TestCaseResult {
    // If the max bit is N, the heap size is at least N+1 words
    let heap_size = Words(u32::from(
        bits.iter().max().map(|max_bit| max_bit + 1).unwrap_or(0),
    ));

    unsafe {
        let mut bitmap = Bitmap::new(heap_size.0);

        for bit in bits.iter() {
            (&mut bitmap as *mut Bitmap).set(u32::from(*bit));
        }

        let mut bits_sorted = bits.into_iter().collect::<Vec<_>>();
        bits_sorted.sort();

        let mut bit_vec_iter = bits_sorted.into_iter();
        let mut bit_map_iter = (&mut bitmap as *mut Bitmap).iter();

        while let Some(vec_bit) = bit_vec_iter.next() {
            match bit_map_iter.next() {
                BITMAP_ITER_END => {
                    return Err(TestCaseError::Fail(
                        "bitmap iterator didn't yield but there are more bits".into(),
                    ));
                }
                map_bit => {
                    if map_bit != u32::from(vec_bit) {
                        return Err(TestCaseError::Fail(
                            format!(
                                "bitmap iterator yields {}, but actual bit is {}",
                                map_bit, vec_bit
                            )
                            .into(),
                        ));
                    }
                }
            }
        }

        let map_bit = bit_map_iter.next();
        if map_bit != BITMAP_ITER_END {
            return Err(TestCaseError::Fail(
                format!(
                    "bitmap iterator yields {}, but there are no more bits left",
                    map_bit
                )
                .into(),
            ));
        }
    }

    Ok(())
}
