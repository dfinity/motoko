//! The implementation of streaming serialisation
//!
//! When serialising Motoko stable variables to stable memory we used to first completely
//! fill up an in-heap buffer and then copy that wholesale into stable memory. This can be
//! disadvategous for two reasons:
//!  - double copying
//!  - heap congestion (especially for the compacting collector)
//!
//! Instead now we'll only allocate a small(ish) blob that will serve as a temporary storage
//! for bytes in transit, while bigger chunks will flush this staging area before being written
//! directly to destination.
//!
//!

// Layout of a stream node:
//
//      ┌────────────┬─────┬───────┬─────────┬─────────┬────────┬──────────┐
//      │ tag (blob) │ len │ ptr64 │ limit64 │ flusher │ filled │ cache... │
//      └────────────┴─────┴───────┴─────────┴─────────┴────────┴──────────┘
//
// We reuse the opaque nature of blobs (to Motoko) and stick Rust-related information
// into the leading bytes:
// - `tag` and `len` are blob metadata
// - `ptr64` and `limit64` are the next and past-end pointers into stable memory
// - `filled` and `cache` are the number of bytes consumed from the blob, and the
//   staging area of the stream, respectively
// - `flusher` is the function to be called when `len - filled` approaches zero.

use crate::memory::{alloc_blob, Memory};
use crate::rts_trap_with;
use crate::types::{size_of, Blob, Bytes, Stream, Value};

const MAX_STREAM_SIZE: Bytes<u32> = Bytes((1 << 30) - 1);

pub unsafe fn alloc_stream<M: Memory>(mem: &mut M, size: Bytes<u32>) -> Value {
    debug_assert!(size > size_of::<Stream>().to_bytes());
    if size > MAX_STREAM_SIZE {
        rts_trap_with("alloc_stream: Cache too large");
    }
    let blob = alloc_blob(mem, size);
    let stream = blob.as_stream();
    (*stream).ptr64 = 0;
    (*stream).limit64 = 0;
    (*stream).flusher = 0;
    (*stream).filled = (size_of::<Stream>() - size_of::<Blob>()).to_bytes();
    blob
}

impl Stream {
    pub unsafe fn payload_addr(self: *mut Self) -> *mut u8 {
        self.add(1) as *mut u8 // skip closure header
    }

    /*
    pub unsafe fn len(self: *mut Self) -> Bytes<u32> {
        (*self).len
    }

    pub unsafe fn get(self: *mut Self, idx: u32) -> u8 {
        *self.payload_addr().add(idx as usize)
    }
     */
}
