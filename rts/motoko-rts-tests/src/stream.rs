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
    for b in 36..96u8 {
        stream.as_stream().cache_byte(b);
    }
    assert_eq!(stream.as_blob().get(36), 36);
    assert_eq!(stream.as_blob().get(91), 91);

    println!("  Testing stream decay");
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(60));
    assert_eq!(stream.as_blob().len(), Bytes(24));

    println!("  Testing stream filling (blocks)");
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(6000)) as usize);
    let chunk: [u8; 10] = [10, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    for _ in 0..600 {
        stream
            .as_stream()
            .cache_bytes(&chunk[0], Bytes(chunk.len() as u32));
    }
    assert_eq!(stream.as_blob().get(36), 10);
    assert_eq!(stream.as_blob().get(37), 1);
    assert_eq!(stream.as_blob().get(45), 9);
    assert_eq!(stream.as_blob().get(46), 10);
    assert_eq!(stream.as_blob().get(6035), 9);
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(6000));

    // TODO: cache_bytes more than STREAM_CHUNK_SIZE

    println!("  Testing stream flushing");
    static mut written: Bytes<u32> = Bytes(0);
    fn just_count(stream: *mut Stream, ptr: *const u8, n: Bytes<u32>) {
        unsafe {
            assert_eq!(*ptr, 'a' as u8);
            written += n
        }
    }
    let stream = alloc_stream(&mut mem, Bytes(6000));
    (*stream).outputter = just_count;
    let place = stream.reserve(Bytes(20));
    *place = 'a' as u8;
    *place.add(1) = 'b' as u8;
    assert_eq!(written, Bytes(0)); // nothing written yet
    stream.shutdown();
    assert_eq!(written, Bytes(20)); // now!
    let place = stream.reserve(Bytes(200));
    *place = 'a' as u8;
    assert_eq!(written, Bytes(20)); // nothing written yet
    let place = stream.reserve(Bytes(6000 - 200));
    assert_eq!(written, Bytes(20)); // nothing written yet
    stream.cache_byte(97);
    assert_eq!(written, Bytes(6020)); // all at once
    stream.shutdown();
    assert_eq!(written, Bytes(6021)); // u8 too
}
