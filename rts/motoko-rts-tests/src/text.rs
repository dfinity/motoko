//! Text and text iterator tests

use crate::memory::TestMemory;

use motoko_rts::memory::Memory;
use motoko_rts::text::{
    blob_of_text, decode_code_point, text_compare, text_concat, text_len, text_of_str,
    text_singleton, text_size,
};
use motoko_rts::text_iter::{text_iter, text_iter_done, text_iter_next};
use motoko_rts::types::{Bytes, SkewedPtr, Words, TAG_BLOB};

use std::convert::TryFrom;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

static STR: &str = "abcdefgh";

struct TextIter<'a, M: Memory> {
    obj: SkewedPtr,
    mem: &'a mut M,
}

impl<'a, M: Memory> TextIter<'a, M> {
    fn from_text(mem: &'a mut M, text: SkewedPtr) -> Self {
        TextIter {
            obj: unsafe { text_iter(mem, text) },
            mem,
        }
    }
}

impl<'a, M: Memory> Iterator for TextIter<'a, M> {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        unsafe {
            if text_iter_done(self.obj) == 1 {
                None
            } else {
                let next = text_iter_next(self.mem, self.obj);
                Some(char::try_from(next).unwrap())
            }
        }
    }
}

pub unsafe fn test() {
    println!("Testing text and text iterators ...");

    let mut mem = TestMemory::new(Words(1024 * 1024));

    println!("  Testing decode_code_point and text_singleton for ASCII");
    for i in 0..=255u32 {
        let char = char::try_from(i).unwrap();
        let mut str = String::new();
        str.push(char);
        let mut out: u32 = 0;
        let char_decoded = decode_code_point(str.as_ptr(), &mut out as *mut _);
        assert_eq!(out, str.len() as u32);
        assert_eq!(char::try_from(char_decoded).unwrap(), char);

        let text = text_singleton(&mut mem, char as u32);
        assert_eq!(TextIter::from_text(&mut mem, text).collect::<String>(), str);
    }

    println!("  Testing text blob iteration");
    for i in 0..8 {
        let str = &STR[0..i + 1];
        let text = text_of_str(&mut mem, str);
        assert_eq!(text.tag(), TAG_BLOB);
        let iter = TextIter::from_text(&mut mem, text);
        assert_eq!(iter.collect::<String>(), str);
    }

    println!("  Testing concatenation");
    concat1(&mut mem);

    drop(mem);

    let mut proptest_runner = TestRunner::new(Config {
        cases: 1_000,
        failure_persistence: None,
        ..Default::default()
    });

    proptest_runner
        .run(
            &proptest::collection::vec(proptest::string::string_regex(".{0, 20}").unwrap(), 1..20),
            |strs| {
                let mut mem = TestMemory::new(Words(1024 * 1024));
                concat_prop(&mut mem, strs)
            },
        )
        .unwrap();
}

unsafe fn concat1<M: Memory>(mem: &mut M) {
    // A simple test extracted from a QuickCheck generated test case
    let strs = ["a", "öabcdef", "y"];

    let mut obj = text_of_str(mem, "");
    for str in &strs {
        let str_obj = text_of_str(mem, str);
        obj = text_concat(mem, obj, str_obj);
    }

    let expected = strs.concat();

    // Check number of characters
    assert_eq!(text_len(obj), expected.chars().count() as u32);

    // Check text size in bytes
    assert_eq!(text_size(obj), Bytes(expected.len() as u32));

    // Generate blob
    let text_blob = blob_of_text(mem, obj);

    // Check number of characters in blob
    assert_eq!(text_len(text_blob), expected.chars().count() as u32);

    // Check blob size in bytes
    assert_eq!(text_size(text_blob), Bytes(expected.len() as u32));

    // Check blob iteration
    let blob = blob_of_text(mem, obj);
    assert_eq!(TextIter::from_text(mem, blob).collect::<String>(), expected);

    // Check blob-concat comparison
    assert_eq!(text_compare(text_blob, obj), 0);

    // Check concat iteration
    assert_eq!(TextIter::from_text(mem, obj).collect::<String>(), expected);
}

fn concat_prop<M: Memory>(mem: &mut M, strs: Vec<String>) -> TestCaseResult {
    unsafe {
        let mut obj = text_of_str(mem, "");
        for str in &strs {
            let str_obj = text_of_str(mem, str);
            obj = text_concat(mem, obj, str_obj);
        }

        let expected = strs.concat();

        // Check number of characters
        if text_len(obj) != expected.chars().count() as u32 {
            return Err(TestCaseError::Fail("text_len".into()));
        }

        // Check text size in bytes
        if text_size(obj) != Bytes(expected.len() as u32) {
            return Err(TestCaseError::Fail("text_size".into()));
        }

        // Generate blob
        let text_blob = blob_of_text(mem, obj);

        // Check number of characters in blob
        if text_len(text_blob) != expected.chars().count() as u32 {
            return Err(TestCaseError::Fail("blob text_len".into()));
        }

        // Check blob size in bytes
        if text_size(text_blob) != Bytes(expected.len() as u32) {
            return Err(TestCaseError::Fail("blob text_size".into()));
        }

        // Check blob iteration
        let blob = blob_of_text(mem, obj);
        if TextIter::from_text(mem, blob).collect::<String>() != expected {
            return Err(TestCaseError::Fail("blob_of_text iteration".into()));
        }

        // Check blob-concat comparison
        if text_compare(text_blob, obj) != 0 {
            return Err(TestCaseError::Fail("text_compare of blob and text".into()));
        }

        // Check concat iteration
        if TextIter::from_text(mem, obj).collect::<String>() != expected {
            return Err(TestCaseError::Fail("iteration".into()));
        }

        Ok(())
    }
}
