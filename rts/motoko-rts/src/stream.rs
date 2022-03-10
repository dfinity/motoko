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

use crate::types::{Blob};

#[repr(C)] // See the note at the beginning of this module
pub struct Stream {
    pub header: Blob,
    pub ptr64: u64,
    pub limit64: u64,
    pub flusher: u32,
    pub filled: u32
    // cache data follows ..
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
