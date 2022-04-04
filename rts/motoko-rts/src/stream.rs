//! The implementation of streaming serialisation
//!
//! When serialising Motoko stable variables to stable memory we used to first completely
//! fill up an in-heap buffer and then copy that wholesale into stable memory. This can be
//! disadvantageous for two reasons:
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
//      └────────────┴─────┴───┴───┴────┴────┴─────────┴────────┴──────────┘
//
// We reuse the opaque nature of blobs (to Motoko) and stick Rust-related information
// into the leading bytes:
// - `tag` and `len` are blob metadata
// - `ptr64` and `limit64` are the next and past-end pointers into stable memory
// - `filled` and `cache` are the number of bytes consumed from the blob, and the
//   staging area of the stream, respectively
// - `flusher` is the function to be called when `len - filled` approaches zero.
// - INVARIANT: keep `BlobStream.filled_field` (in compile.ml) in sync with the layout!

use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::rts_trap_with;
use crate::types::{size_of, Blob, Bytes, Stream, Value, TAG_BLOB};

use motoko_rts_macros::ic_mem_fn;

const MAX_STREAM_SIZE: Bytes<u32> = Bytes((1 << 30) - 1);
const INITIAL_STREAM_FILLED: Bytes<u32> = Bytes(24);
const STREAM_CHUNK_SIZE: Bytes<u32> = Bytes(128);

#[ic_mem_fn]
pub unsafe fn alloc_stream<M: Memory>(mem: &mut M, size: Bytes<u32>) -> *mut Stream {
    debug_assert_eq!(
        INITIAL_STREAM_FILLED,
        (size_of::<Stream>() - size_of::<Blob>()).to_bytes()
    );
    if size > MAX_STREAM_SIZE {
        rts_trap_with("alloc_stream: Cache too large");
    }
    let stream = alloc_blob(mem, size + INITIAL_STREAM_FILLED).as_stream();
    (*stream).ptr64 = 0;
    (*stream).limit64 = 0;
    (*stream).flusher = Stream::flush; // FIXME: needed? send_to_stable? BOTH?
    (*stream).filled = INITIAL_STREAM_FILLED;
    stream
}

impl Stream {
    #[inline]
    pub unsafe fn payload_addr(self: *const Self) -> *const u8 {
        self.add(1) as *const u8 // skip closure header
    }

    /// make sure that the cache is empty
    #[inline]
    fn flush(self: *mut Self) {
        unsafe {
            if (*self).filled > INITIAL_STREAM_FILLED {
                self.send_to_stable(self.payload_addr(), (*self).filled - INITIAL_STREAM_FILLED);
                (*self).filled = INITIAL_STREAM_FILLED
            }
        }
    }
    fn send_to_stable(self: *mut Self, _ptr: *const u8, _n: Bytes<u32>) {
        assert!(false)
    }

    /// Ingest a number of bytes into the stream.
    #[export_name = "stream_write"]
    pub fn stash(self: *mut Self, ptr: *const u8, n: Bytes<u32>) {
        unsafe {
            if (*self).limit64 != 0 && n > STREAM_CHUNK_SIZE
                || (*self).header.len - (*self).filled < n
            {
                self.flush();
                self.send_to_stable(ptr, n);
            } else {
                let dest = (self as *mut Blob)
                    .payload_addr()
                    .add((*self).filled.as_usize());
                (*self).filled += n;
                assert!((*self).filled <= (*self).header.len);
                memcpy_bytes(dest as usize, ptr as usize, n);
            }
        }
    }
    #[export_name = "stream_write_byte"]
    pub fn stash8(self: *mut Self, byte: u8) {
        unsafe {
            if (*self).filled == (*self).header.len {
                self.flush()
            }
            (self as *mut Blob).set((*self).filled.as_u32(), byte);
            (*self).filled += Bytes(1)
        }
    }

    #[export_name = "stream_reserve"]
    pub fn reserve(self: *mut Self, bytes: Bytes<u32>) -> *mut u8 {
        unsafe {
            if (*self).filled + bytes > (*self).header.len {
        assert!(false);
                self.flush()
            }
            let ptr = (self as *mut Blob).payload_addr().add((*self).filled.as_usize());
            (*self).filled += bytes;
            ptr
        }
    }

    /// Split the stream object into two `Blob`s, a front-runner (small) one
    /// and a latter one that comprises the current amount of the cached bytes.
    /// Lengths are adjusted correspondingly.
    #[export_name = "stream_split"]
    pub unsafe fn split(self: *mut Self) -> Value {
	//crate::debug::print_value(Value::from_ptr(self as usize));



	//assert_eq!(((*self).header.len, (*self).filled), (Bytes(78), Bytes(51)));

	//if (*self).filled.as_u32() <= 69 {return Value::from_raw((*self).filled.as_u32());}


	(*self).header.len = INITIAL_STREAM_FILLED - size_of::<Blob>().to_bytes();
        (*self).filled -= INITIAL_STREAM_FILLED;
        let blob = (self.payload_addr() as *mut Blob).sub(1);
        (*blob).header.tag = TAG_BLOB;
        debug_assert_eq!(blob.len(), (*self).filled);




	//assert_eq!((blob.len(), (*self).filled), (Bytes(54), Bytes(54)));

	//crate::debug::print_value(Value::from_ptr(self as usize));
	//crate::debug::print_value(Value::from_ptr(blob as usize));


	
	Value::from_ptr(blob as usize)
    }




    #[export_name = "diff_stream_pos"]
    pub unsafe fn diff_stream_pos(prev: i32, now: i32) -> i32 {
	let diff = prev - now;
	//assert!(diff == -18 || diff == -23);
	diff
    }


    #[export_name = "check_len"]
    pub unsafe fn check_len(now: u32) -> u32 {
	if now > 100 {
	    //crate::debug::print_value(Value::from_raw(now))
	}else{

	    //assert_eq!(now, 54);
	}
	now
    }

}
