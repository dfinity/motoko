//! Stream tests

use crate::memory::TestMemory;

use motoko_rts::memory::Memory;
use motoko_rts::stream::alloc_stream;
use motoko_rts::text::{
    blob_of_text, decode_code_point, text_compare, text_concat, text_len, text_of_str,
    text_singleton, text_size,
};
use motoko_rts::text_iter::{text_iter, text_iter_done, text_iter_next};
use motoko_rts::types::{Bytes, Stream, Value, Words};

use std::convert::TryFrom;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing streaming ...");

    let mut mem = TestMemory::new(Words(1024 * 1024));

    println!("  Testing stream creation");
    let stream = alloc_stream(&mut mem, Bytes(60));

    println!("  Testing stream filling (single bytes)");
    for b in 32..92u8 {
        stream.as_stream().stash8(b);
    }
    assert_eq!(stream.as_blob().get(24), 32);
    assert_eq!(stream.as_blob().get(83), 91);

    println!("  Testing stream filling (blocks)");
    println!("  Testing stream decay");
}
