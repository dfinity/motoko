//! Stream tests

use crate::memory::TestMemory;

use motoko_rts::stream::alloc_stream;
use motoko_rts::types::{Bytes, Stream, Value, Words};

pub unsafe fn test() {
    println!("Testing streaming ...");

    let mut mem = TestMemory::new(Words(1024 * 1024));

    println!("  Testing stream creation");
    let stream = Value::from_ptr(alloc_stream(&mut mem, Bytes(60)) as usize);

    println!("  Testing stream filling (single bytes)");
    for b in 32..92u8 {
        stream.as_stream().cache_byte(b);
    }
    assert_eq!(stream.as_blob().get(32), 32);
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
    assert_eq!(stream.as_blob().get(32), 10);
    assert_eq!(stream.as_blob().get(33), 1);
    assert_eq!(stream.as_blob().get(41), 9);
    assert_eq!(stream.as_blob().get(42), 10);
    assert_eq!(stream.as_blob().get(6031), 9);
    let blob = stream.as_stream().split();
    assert_eq!(blob.as_blob().len(), Bytes(6000));

    // TODO: cache_bytes more than STREAM_CHUNK_SIZE

    println!("  Testing stream flushing");
    static mut WRITTEN: Bytes<u32> = Bytes(0);
    fn just_count(_stream: *mut Stream, ptr: *const u8, n: Bytes<u32>) {
        unsafe {
            assert_eq!(*ptr, 'a' as u8);
            WRITTEN += n
        }
    }
    let stream = alloc_stream(&mut mem, Bytes(6000));
    assert_eq!(stream.read_ptr64(), 0);
    assert_eq!(stream.read_start64(), 0);
    assert_eq!(stream.read_limit64(), 0);
    stream.write_outputter(just_count);
    let place = stream.reserve(Bytes(20));
    *place = 'a' as u8;
    *place.add(1) = 'b' as u8;
    assert_eq!(WRITTEN, Bytes(0)); // nothing written yet
    stream.shutdown();
    assert_eq!(WRITTEN, Bytes(20)); // now!
    let place = stream.reserve(Bytes(200));
    *place = 'a' as u8;
    assert_eq!(WRITTEN, Bytes(20)); // nothing written yet
    stream.reserve(Bytes(6000 - 200));
    assert_eq!(WRITTEN, Bytes(20)); // nothing written yet
    stream.cache_byte(97);
    assert_eq!(WRITTEN, Bytes(6020)); // all at once
    stream.shutdown();
    assert_eq!(WRITTEN, Bytes(6021)); // u8 too
}
