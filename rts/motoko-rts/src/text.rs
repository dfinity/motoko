//! The implementation of the Text type in Motoko
//!
//! One main goal of this datastructure (inspired by ropes and similar) is to support constant time
//! concatenation, by having a dedicated heap object for the concatenation of two strings.
//!
//! The first goal was to wire up this C code with the RTS that encapsulates the internals of
//! strings.
//!
//! This encapsulation is not complete (and likely never will)
//!  * the compiler needs to emit static text literals,
//!  * the garbage collector needs to know some of the internals.
//!
//! In a subsequent step, the actual concatenation node has been introduced.
//!
//! From here on, there are stretch goals like:
//!  - restructure recursive code to not use unbounded C stack
//!  - maybe rebalancing

// Layout of a concat node:
//
//      ┌──────────────┬─────────┬───────┬───────┐
//      │ tag (concat) │ n_bytes │ text1 │ text2 │
//      └──────────────┴─────────┴───────┴───────┘
//
// Note that `CONCAT_LEN` and `BLOB_LEN` are identical, so no need to check the tag to know the
// size of the text.

use crate::alloc::{alloc_blob, alloc_words};
use crate::mem::memcpy_bytes;
use crate::rts_trap_with;
use crate::types::{Blob, Bytes, Concat, Obj, SkewedPtr, Words, TAG_BLOB, TAG_CONCAT};

use core::cmp::{min, Ordering};

const MAX_STR_SIZE: Bytes<u32> = Bytes((1 << 30) - 1);
// Strings smaller than this must be blobs
// Make this MAX_STR_SIZE to disable the use of ropes completely, e.g. for debugging
const MIN_CONCAT_SIZE: Bytes<u32> = Bytes(9);

// tag, n_bytes, text1, text2
const CONCAT_WORDS: Words<u32> = Words(4);

// TODO: remove no_mangle after porting the whole text.c
#[no_mangle]
unsafe extern "C" fn alloc_text_blob(size: Bytes<u32>) -> SkewedPtr {
    if size > MAX_STR_SIZE {
        rts_trap_with("alloc_text_bloc: Text too large\0".as_ptr());
    }
    alloc_blob(size)
}

#[no_mangle]
unsafe extern "C" fn text_of_ptr_size(buf: *const u8, n: Bytes<u32>) -> SkewedPtr {
    let blob = alloc_text_blob(n);
    let payload_addr = (blob.unskew() as *const Blob).payload_addr();
    memcpy_bytes(payload_addr as usize, buf as usize, n);
    blob
}

pub(crate) unsafe fn text_of_str(s: &str) -> SkewedPtr {
    // TODO (osa): How are Motoko strings encoded? Rust strings are UTF-8
    text_of_ptr_size(s.as_ptr(), Bytes(s.len() as u32))
}

#[no_mangle]
unsafe extern "C" fn text_concat(s1: SkewedPtr, s2: SkewedPtr) -> SkewedPtr {
    let blob1 = s1.unskew() as *const Blob;
    let blob2 = s2.unskew() as *const Blob;

    let blob1_len = blob1.len();
    let blob2_len = blob2.len();

    if blob1_len == Bytes(0) {
        return s2;
    }

    if blob2_len == Bytes(0) {
        return s1;
    }

    let new_len = blob1_len + blob2_len;

    // Short texts are copied into a single blob
    if new_len < MIN_CONCAT_SIZE {
        let r = alloc_text_blob(new_len);
        let r_payload: *const u8 = (r.unskew() as *const Blob).payload_addr();
        memcpy_bytes(r_payload as usize, blob1.payload_addr() as usize, blob1_len);
        memcpy_bytes(
            r_payload.add(blob1_len.0 as usize) as usize,
            blob2.payload_addr() as usize,
            blob2_len,
        );
        return r;
    }

    // Check max size
    if new_len > MAX_STR_SIZE {
        rts_trap_with("text_concat: Text too large\0".as_ptr());
    }

    // Create concat node
    let r = alloc_words(CONCAT_WORDS);
    let r_concat = r.unskew() as *mut Concat;
    (*r_concat).header.tag = TAG_CONCAT;
    (*r_concat).n_bytes = new_len;
    (*r_concat).text1 = s1;
    (*r_concat).text2 = s2;
    r
}

// Leaving breadcrumbs in the destination buffer for which concat node/blob to continue
// serializing
#[repr(C)]
struct Crumb {
    /// Pointer to the concat node/blob to serialize
    t: SkewedPtr,
    /// Where to serialize the concat node/blob
    next: *const Crumb,
}

#[no_mangle]
unsafe extern "C" fn text_to_buf(mut s: SkewedPtr, mut buf: *mut u8) {
    let mut next_crumb: *const Crumb = core::ptr::null();

    loop {
        let s_ptr = s.unskew() as *const Obj;
        if (*s_ptr).tag == TAG_BLOB {
            let blob = s_ptr as *const Blob;
            memcpy_bytes(buf as usize, blob.payload_addr() as usize, blob.len());

            if next_crumb.is_null() {
                return;
            }

            buf = next_crumb as *mut u8;
            s = (*next_crumb).t;
            next_crumb = (*next_crumb).next;
        } else {
            debug_assert_eq!((*s_ptr).tag, TAG_CONCAT);
            let concat = s_ptr as *const Concat;
            let s1 = (*concat).text1;
            let s2 = (*concat).text2;

            let s1_len = text_size(s1);
            let s2_len = text_size(s2);

            if s2_len < Bytes(core::mem::size_of::<Crumb>() as u32) {
                // If second string is smaller than size of a crumb just do it directly
                text_to_buf(s2, buf.add(s1_len.0 as usize));
                s = s1;
            } else {
                // Otherwise leave a breadcrumb to the location of the second string
                let new_crumb: *mut Crumb = buf.add(s1_len.0 as usize) as *mut Crumb;
                (*new_crumb).t = s2;
                (*new_crumb).next = next_crumb;
                next_crumb = new_crumb;
                s = s1;
            }
        }
    }
}

// Straighten into contiguous memory, if needed (e.g. for system calls)
#[no_mangle]
unsafe extern "C" fn blob_of_text(s: SkewedPtr) -> SkewedPtr {
    let obj = s.unskew() as *const Obj;
    if (*obj).tag == TAG_BLOB {
        s
    } else {
        debug_assert_eq!((*obj).tag, TAG_CONCAT);
        let concat = obj as *const Concat;
        let r = alloc_text_blob((*concat).n_bytes);
        text_to_buf(s, (r.unskew() as *const Blob).payload_addr() as *mut u8);
        r
    }
}

#[no_mangle]
unsafe extern "C" fn text_size(s: SkewedPtr) -> Bytes<u32> {
    // We don't know whether the string is a blob or concat, but both types have the length in same
    // location so using any of the types to get the length is fine
    (s.unskew() as *const Blob).len()
}

/// Compares texts from given offset on for the given number of bytes. All assumed to be in range.
unsafe fn text_compare_range(
    s1: SkewedPtr,
    offset1: Bytes<u32>,
    s2: SkewedPtr,
    offset2: Bytes<u32>,
    n: Bytes<u32>,
) -> Ordering {
    // println!(
    //     100,
    //     "text_compare_range(offset1={}, offset2={}, n={})", offset1.0, offset2.0, n.0
    // );

    // Follow the left/right strings of concat nodes until we reach to blobs or concats that cannot
    // be split further (the range spans left and right strings)
    let (s1, offset1) = text_get_range(s1, offset1, n);
    let (s2, offset2) = text_get_range(s2, offset2, n);

    let s1_obj = s1.unskew() as *const Obj;
    let s2_obj = s2.unskew() as *const Obj;

    // Decompose concats
    if s1_obj.tag() == TAG_CONCAT {
        // println!(50, "Decomposing s1");

        let s1_concat = s1_obj as *const Concat;
        let n_compared = text_size((*s1_concat).text1) - offset1;
        let cmp = text_compare_range((*s1_concat).text1, offset1, s2, offset2, n_compared);
        match cmp {
            Ordering::Less | Ordering::Greater => cmp,
            Ordering::Equal => text_compare_range(
                (*s1_concat).text2,
                Bytes(0),
                s2,
                offset2 + n_compared,
                n - n_compared,
            ),
        }
    } else if s2_obj.tag() == TAG_CONCAT {
        // println!(50, "Decomposing s2");

        let s2_concat = s2_obj as *const Concat;
        let n_compared = text_size((*s2_concat).text1) - offset2;
        let cmp = text_compare_range(s1, offset1, (*s2_concat).text1, offset2, n_compared);
        match cmp {
            Ordering::Less | Ordering::Greater => cmp,
            Ordering::Equal => text_compare_range(
                s1,
                offset1 + n_compared,
                (*s2_concat).text2,
                Bytes(0),
                n - n_compared,
            ),
        }
    } else {
        // println!(20, "memcpy(n={})", n.0);

        debug_assert_eq!(s1_obj.tag(), TAG_BLOB);
        debug_assert_eq!(s2_obj.tag(), TAG_BLOB);

        let s1_blob = s1_obj as *const Blob;
        let s2_blob = s2_obj as *const Blob;

        let cmp = libc::memcmp(
            s1_blob.payload_addr().add(offset1.0 as usize) as *const _,
            s2_blob.payload_addr().add(offset2.0 as usize) as *const _,
            n.0 as usize,
        );

        if cmp < 0 {
            Ordering::Less
        } else if cmp == 0 {
            Ordering::Equal
        } else {
            Ordering::Greater
        }
    }
}

/// Follow left/right strings of concat nodes until we reach to a BLOB or a CONCAT that can't be
/// split further (i.e. range spans left and right nodes). Returns a BLOB or CONCAT.
unsafe fn text_get_range(
    mut s: SkewedPtr,
    mut offset: Bytes<u32>,
    n: Bytes<u32>,
) -> (SkewedPtr, Bytes<u32>) {
    // println!(100, "text_get_range(offset={}, n={})", offset.0, n.0);

    loop {
        let s_obj = s.unskew() as *const Obj;

        if s_obj.tag() == TAG_CONCAT {
            let s_concat = s_obj as *const Concat;

            let left = (*s_concat).text1;
            let left_size = text_size(left);

            // Follow left node?
            if left_size >= offset + n {
                // println!(50, "Following left");
                s = left;
                continue;
            }

            // Follow right node?
            if offset >= left_size {
                // println!(50, "Following right");
                s = (*s_concat).text2;
                offset -= left_size;
                continue;
            }
        } else {
            debug_assert_eq!(s_obj.tag(), TAG_BLOB);
        }

        break;
    }

    (s, offset)
}

#[no_mangle]
unsafe extern "C" fn text_compare(s1: SkewedPtr, s2: SkewedPtr) -> i32 {
    let n1 = text_size(s1);
    let n2 = text_size(s2);
    let n = min(n1, n2);

    match text_compare_range(s1, Bytes(0), s2, Bytes(0), n) {
        Ordering::Less => -1,
        Ordering::Greater => 1,
        Ordering::Equal => {
            if n1 > n {
                1
            } else if n2 > n {
                -1
            } else {
                0
            }
        }
    }
}
