//! Stream tests

use crate::memory::TestMemory;

use motoko_rts::memory::Memory;
use motoko_rts::stream::alloc_stream;
use motoko_rts::text::{
    blob_of_text, decode_code_point, text_compare, text_concat, text_len, text_of_str,
    text_singleton, text_size,
};
use motoko_rts::text_iter::{text_iter, text_iter_done, text_iter_next};
use motoko_rts::types::{size_of, Blob, Bytes, Stream, Value, Words};

use std::convert::TryFrom;

use proptest::test_runner::{Config, TestCaseError, TestCaseResult, TestRunner};

pub unsafe fn test() {
    println!("Testing streaming ...");

    let mut mem = TestMemory::new(Words(1024 * 1024));

    const STREAM_SMALL_SIZE: u32 = 60;

    println!("  Testing stream creation");
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(STREAM_SMALL_SIZE)) as usize);

    let initial_stream_filled = (size_of::<Stream>() - size_of::<Blob>())
        .to_bytes()
        .as_u32();

    println!("  Testing stream filling (single bytes)");
    for b in initial_stream_filled..(initial_stream_filled + STREAM_SMALL_SIZE) {
        stream.as_stream().cache_byte(b as u8);
    }
    assert_eq!(
        stream.as_blob().get(initial_stream_filled) as u32,
        initial_stream_filled
    );
    let last = initial_stream_filled + STREAM_SMALL_SIZE - 1;
    assert_eq!(stream.as_blob().get(last) as u32, last);

    println!("  Testing stream decay");
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(STREAM_SMALL_SIZE));
    assert_eq!(stream.as_blob().len(), Bytes(24));

    println!("  Testing stream filling (blocks)");
    const STREAM_LARGE_SIZE: u32 = 6000;
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(STREAM_LARGE_SIZE)) as usize);
    let chunk: [u8; 10] = [10, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    for _ in 0..STREAM_LARGE_SIZE / chunk.len() as u32 {
        stream
            .as_stream()
            .cache_bytes(&chunk[0], Bytes(chunk.len() as u32));
    }
    assert_eq!(stream.as_blob().get(initial_stream_filled), 10);
    assert_eq!(stream.as_blob().get(initial_stream_filled + 1), 1);
    assert_eq!(stream.as_blob().get(initial_stream_filled + 9), 9);
    assert_eq!(stream.as_blob().get(initial_stream_filled + 10), 10);
    assert_eq!(
        stream
            .as_blob()
            .get(initial_stream_filled + STREAM_LARGE_SIZE - 1),
        9
    );
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(STREAM_LARGE_SIZE));

    // TODO: cache_bytes more than STREAM_CHUNK_SIZE

    println!("  Testing stream flushing");
    static mut written: Bytes<u32> = Bytes(0);
    fn just_count(stream: *mut Stream, ptr: *const u8, n: Bytes<u32>) {
        unsafe {
            assert_eq!(*ptr, 'a' as u8);
            written += n
        }
    }
    let stream = alloc_stream(&mut mem, Bytes(STREAM_LARGE_SIZE));
    (*stream).outputter = just_count;
    const STREAM_RESERVE_SIZE1: u32 = 20;
    let place = stream.reserve(Bytes(STREAM_RESERVE_SIZE1));
    *place = 'a' as u8;
    *place.add(1) = 'b' as u8;
    assert_eq!(written, Bytes(0)); // nothing written yet
    stream.shutdown();
    assert_eq!(written, Bytes(STREAM_RESERVE_SIZE1)); // now!
    const STREAM_RESERVE_SIZE2: u32 = 200;
    let place = stream.reserve(Bytes(STREAM_RESERVE_SIZE2));
    *place = 'a' as u8;
    assert_eq!(written, Bytes(STREAM_RESERVE_SIZE1)); // nothing written yet
    let place = stream.reserve(Bytes(STREAM_LARGE_SIZE - STREAM_RESERVE_SIZE2));
    assert_eq!(written, Bytes(STREAM_RESERVE_SIZE1)); // nothing written yet
    stream.cache_byte(97);
    assert_eq!(written, Bytes(STREAM_LARGE_SIZE + STREAM_RESERVE_SIZE1)); // all at once
    stream.shutdown();
    assert_eq!(written, Bytes(STREAM_LARGE_SIZE + STREAM_RESERVE_SIZE1 + 1)); // u8 too
}
