//! Stream tests

use crate::memory::{initialize_test_memory, reset_test_memory};

use motoko_rts::stream::alloc_stream;
use motoko_rts::types::{size_of, Blob, Bytes, Stream, Value};

pub unsafe fn test() {
    println!("Testing streaming ...");

    let mut mem = initialize_test_memory();

    const STREAM_SMALL_SIZE: usize = 60;

    println!("  Testing stream creation");
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(STREAM_SMALL_SIZE)) as usize);

    let initial_stream_filled = (size_of::<Stream>() - size_of::<Blob>())
        .to_bytes()
        .as_usize();

    println!("  Testing stream filling (single bytes)");
    for b in initial_stream_filled..(initial_stream_filled + STREAM_SMALL_SIZE) {
        stream.as_stream().cache_byte(b as u8);
    }
    assert_eq!(
        stream.as_blob().get(initial_stream_filled) as usize,
        initial_stream_filled
    );
    let last = initial_stream_filled + STREAM_SMALL_SIZE - 1;
    assert_eq!(stream.as_blob().get(last) as usize, last);

    println!("  Testing stream decay");
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(STREAM_SMALL_SIZE));
    const REMAINDER: usize = 16;
    assert_eq!(stream.as_blob().len(), Bytes(REMAINDER));

    println!("  Testing stream filling (blocks)");
    const STREAM_LARGE_SIZE: usize = 6000;
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(STREAM_LARGE_SIZE)) as usize);
    let chunk: [u8; 10] = [10, 1, 2, 3, 4, 5, 6, 7, 8, 9];
    for _ in 0..STREAM_LARGE_SIZE / chunk.len() {
        stream
            .as_stream()
            .cache_bytes(&chunk[0], Bytes(chunk.len()));
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
    static mut WRITTEN: Bytes<usize> = Bytes(0);
    fn just_count(_stream: *mut Stream, ptr: *const u8, n: Bytes<usize>) {
        unsafe {
            assert_eq!(*ptr, 'a' as u8);
            WRITTEN += n
        }
    }
    let stream = alloc_stream(&mut mem, Bytes(STREAM_LARGE_SIZE));
    (*stream).outputter = just_count;
    const STREAM_RESERVE_SIZE1: usize = 20;
    let place = stream.reserve(Bytes(STREAM_RESERVE_SIZE1));
    *place = 'a' as u8;
    *place.add(1) = 'b' as u8;
    assert_eq!(WRITTEN, Bytes(0)); // nothing written yet
    stream.shutdown();
    assert_eq!(WRITTEN, Bytes(STREAM_RESERVE_SIZE1)); // now!
    const STREAM_RESERVE_SIZE2: usize = 200;
    let place = stream.reserve(Bytes(STREAM_RESERVE_SIZE2));
    *place = 'a' as u8;
    assert_eq!(WRITTEN, Bytes(STREAM_RESERVE_SIZE1)); // nothing written yet
    stream.reserve(Bytes(STREAM_LARGE_SIZE - STREAM_RESERVE_SIZE2));
    assert_eq!(WRITTEN, Bytes(STREAM_RESERVE_SIZE1)); // nothing written yet
    stream.cache_byte(97);
    assert_eq!(WRITTEN, Bytes(STREAM_LARGE_SIZE + STREAM_RESERVE_SIZE1)); // all at once
    stream.shutdown();
    assert_eq!(WRITTEN, Bytes(STREAM_LARGE_SIZE + STREAM_RESERVE_SIZE1 + 1)); // u8 too

    reset_test_memory();
}
