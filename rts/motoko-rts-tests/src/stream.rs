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
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(60)) as usize);

    println!("  Testing stream filling (single bytes)");
    for b in 32..92u8 {
        stream.as_stream().cache_byte(b);
    }
    assert_eq!(stream.as_blob().get(24), 32);
    assert_eq!(stream.as_blob().get(83), 91);

    println!("  Testing stream decay");
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(60));
    assert_eq!(stream.as_blob().len(), Bytes(16));

    println!("  Testing stream filling (blocks)");
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(6000)) as usize);
    let chunk: [u8; 10] = [10, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    for _ in 0..600 {
        stream
            .as_stream()
            .cache_bytes(&chunk[0], Bytes(chunk.len() as u32));
    }
    assert_eq!(stream.as_blob().get(24), 10);
    assert_eq!(stream.as_blob().get(25), 1);
    assert_eq!(stream.as_blob().get(33), 9);
    assert_eq!(stream.as_blob().get(34), 10);
    assert_eq!(stream.as_blob().get(6023), 9);
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(6000));

    // TODO: cache_bytes more than STREAM_CHUNK_SIZE

    println!("  Testing stream flushing");
    let mut written = Bytes(0);
    fn just_count(stream: *mut Stream, _ptr: *const u8, n: Bytes<u32>) { written += n }
    let stream = alloc_stream(&mut mem, Bytes(6000));
    (*stream).outputter = just_count;
    let place = stream.reserve(Bytes(20));
    *place = 'a' as u8;
    *place.add(1) = 'b' as u8;
}
