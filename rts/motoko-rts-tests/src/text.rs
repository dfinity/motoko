//! Text and text iterator tests

use motoko_rts::text::{
    blob_of_text, decode_code_point, text_compare, text_concat, text_len, text_of_str,
    text_singleton, text_size,
};
use motoko_rts::text_iter::{text_iter, text_iter_done, text_iter_next};
use motoko_rts::types::{Bytes, SkewedPtr, TAG_BLOB};

use std::convert::TryFrom;

use quickcheck::{quickcheck, TestResult};

static STR: &str = "abcdefgh";

struct TextIter {
    obj: SkewedPtr,
}

impl TextIter {
    fn from_text(text: SkewedPtr) -> Self {
        TextIter {
            obj: unsafe { text_iter(text) },
        }
    }
}

impl Iterator for TextIter {
    type Item = char;

    fn next(&mut self) -> Option<char> {
        unsafe {
            if text_iter_done(self.obj) == 1 {
                None
            } else {
                Some(char::try_from(text_iter_next(self.obj)).unwrap())
            }
        }
    }
}

pub unsafe fn test() {
    println!("Testing text and text iterators ...");

    println!("  Testing decode_code_point and text_singleton for ASCII");
    for i in 0..=255u32 {
        let char = char::try_from(i).unwrap();
        let mut str = String::new();
        str.push(char);
        let mut out: u32 = 0;
        let char_decoded = decode_code_point(str.as_ptr(), &mut out as *mut _);
        assert_eq!(out, str.len() as u32);
        assert_eq!(char::try_from(char_decoded).unwrap(), char);

        let text = text_singleton(char as u32);
        assert_eq!(TextIter::from_text(text).collect::<String>(), str);
    }

    println!("  Testing text blob iteration");
    for i in 0..8 {
        let str = &STR[0..i + 1];
        let text = text_of_str(str);
        assert_eq!(text.tag(), TAG_BLOB);
        let iter = TextIter::from_text(text);
        assert_eq!(iter.collect::<String>(), str);
    }

    println!("  Testing concatenation");
    concat1();
    quickcheck(concat_prop as fn(Vec<String>) -> TestResult);
}

unsafe fn concat1() {
    // A simple test extracted from a QuickCheck generated test case
    let strs = ["a", "öabcdef", "y"];

    let mut obj = text_of_str("");
    for str in &strs {
        let str_obj = text_of_str(str);
        obj = text_concat(obj, str_obj);
    }

    let expected = strs.concat();

    // Check number of characters
    assert_eq!(text_len(obj), expected.chars().count() as u32);

    // Check text size in bytes
    assert_eq!(text_size(obj), Bytes(expected.len() as u32));

    // Generate blob
    let text_blob = blob_of_text(obj);

    // Check number of characters in blob
    assert_eq!(text_len(text_blob), expected.chars().count() as u32);

    // Check blob size in bytes
    assert_eq!(text_size(text_blob), Bytes(expected.len() as u32));

    // Check blob iteration
    assert_eq!(
        TextIter::from_text(blob_of_text(obj)).collect::<String>(),
        expected
    );

    // Check blob-concat comparison
    assert_eq!(text_compare(text_blob, obj), 0);

    // Check concat iteration
    assert_eq!(TextIter::from_text(obj).collect::<String>(), expected);
}

fn concat_prop(strs: Vec<String>) -> TestResult {
    unsafe {
        let mut obj = text_of_str("");
        for str in &strs {
            let str_obj = text_of_str(str);
            obj = text_concat(obj, str_obj);
        }

        let expected = strs.concat();

        // Check number of characters
        if text_len(obj) != expected.chars().count() as u32 {
            return TestResult::error("text_len");
        }

        // Check text size in bytes
        if text_size(obj) != Bytes(expected.len() as u32) {
            return TestResult::error("text_size");
        }

        // Generate blob
        let text_blob = blob_of_text(obj);

        // Check number of characters in blob
        if text_len(text_blob) != expected.chars().count() as u32 {
            return TestResult::error("blob text_len");
        }

        // Check blob size in bytes
        if text_size(text_blob) != Bytes(expected.len() as u32) {
            return TestResult::error("blob text_size");
        }

        // Check blob iteration
        if TextIter::from_text(blob_of_text(obj)).collect::<String>() != expected {
            return TestResult::error("blob_of_text iteration");
        }

        // Check blob-concat comparison
        if text_compare(text_blob, obj) != 0 {
            return TestResult::error("text_compare of blob and text");
        }

        // Check concat iteration
        if TextIter::from_text(obj).collect::<String>() != expected {
            return TestResult::error("iteration");
        }

        TestResult::passed()
    }
}
