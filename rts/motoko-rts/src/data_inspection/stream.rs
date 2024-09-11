//! Simple stream implementation used for data inspection.
//!
//! As the stream size is not known in advance, the stream is represented a
//! linked list of fixed-size chunks that are finally concatenated to a single
//! combined blob.
//!
//! NOTES:
//! * The stream is only used temporarily during a message as it contains
//!   low-level pointers that are not considered by the GC.
//! * The internal nodes and chunk blocks can be reclaimed by the next GC run.

use core::{cmp::min, mem::size_of};

use crate::{
    barriers::allocation_barrier,
    constants::WORD_SIZE,
    mem_utils::memcpy_bytes,
    memory::{alloc_blob, Memory},
    types::{Blob, Bytes, Value, TAG_BLOB_B},
};

const STREAM_CHUNK_SIZE: usize = 1024 * WORD_SIZE;

/// Pointing to the last invalid Wasm page.
pub struct Stream {
    first: *mut StreamChunk,
    last: *mut StreamChunk,
    length: usize,
}

impl Stream {
    pub fn new() -> Stream {
        Stream {
            first: StreamChunk::null(),
            last: StreamChunk::null(),
            length: 0,
        }
    }

    pub unsafe fn write<M: Memory, T>(&mut self, mem: &mut M, value: &T) {
        let length = size_of::<T>();
        let value_address = value as *const T as usize;
        self.raw_write(mem, value_address, length);
    }

    pub unsafe fn raw_write<M: Memory>(
        &mut self,
        mem: &mut M,
        source_address: usize,
        length: usize,
    ) {
        let mut start = source_address;
        let end = source_address + length;
        while start < end {
            self.ensure_capacity(mem);
            let capacity = STREAM_CHUNK_SIZE - self.length % STREAM_CHUNK_SIZE;
            let chunk_size = min(end - start, capacity);
            (*self.last).write(start, chunk_size);
            self.length += chunk_size;
            start += chunk_size;
            debug_assert!(start <= end);
        }
    }

    unsafe fn ensure_capacity<M: Memory>(&mut self, mem: &mut M) {
        if self.length % STREAM_CHUNK_SIZE == 0 {
            self.append_chunk(mem);
        }
    }

    unsafe fn append_chunk<M: Memory>(&mut self, mem: &mut M) {
        debug_assert_eq!(self.length % STREAM_CHUNK_SIZE, 0);
        let new_chunk = StreamChunk::new(mem);
        if self.first == StreamChunk::null() {
            self.first = new_chunk;
        }
        if self.last != StreamChunk::null() {
            (*self.last).next = new_chunk;
        }
        self.last = new_chunk;
    }

    pub unsafe fn finalize<M: Memory>(&self, mem: &mut M) -> Value {
        let combined = alloc_blob(mem, TAG_BLOB_B, Bytes(self.length));
        allocation_barrier(combined); // Allow general use of the returned blob.
        let mut target = combined.as_blob_mut().payload_addr() as usize;
        let mut current = self.first;
        let mut remainder = self.length;
        while current != StreamChunk::null() {
            let chunk_size = min(remainder, STREAM_CHUNK_SIZE);
            (*current).read(target, chunk_size);
            target += chunk_size;
            remainder -= chunk_size;
            current = (*current).next;
        }
        debug_assert_eq!(remainder, 0);
        combined
    }
}

/// Stream chunks are blobs.
#[repr(C)]
struct StreamChunk {
    pub header: Blob,
    pub next: *mut StreamChunk,
    pub data: [u8; STREAM_CHUNK_SIZE],
}

impl StreamChunk {
    fn null() -> *mut StreamChunk {
        // Conservatively, point to last Wasm page that is not allocated.
        0xffff_ffff_ffff_ffff as *mut StreamChunk
    }

    unsafe fn new<M: Memory>(mem: &mut M) -> *mut StreamChunk {
        let size = crate::types::Bytes(size_of::<StreamChunk>());
        let blob = alloc_blob(mem, TAG_BLOB_B, size);
        // No post allocation barrier as this RTS-internal blob will be collected by the GC.
        let chunk = blob.as_blob_mut() as *mut StreamChunk;
        (*chunk).next = StreamChunk::null();
        // `(*chunk).data` can remain uninitialized as only written data is dumped.
        chunk
    }

    unsafe fn write(&mut self, source_address: usize, length: usize) {
        debug_assert!(length <= STREAM_CHUNK_SIZE);
        let target = &mut self.data as *mut u8;
        memcpy_bytes(target as usize, source_address, Bytes(length));
    }

    unsafe fn read(&self, target_address: usize, length: usize) {
        debug_assert!(length <= STREAM_CHUNK_SIZE);
        let source = &self.data as *const u8;
        memcpy_bytes(target_address, source as usize, Bytes(length));
    }
}
