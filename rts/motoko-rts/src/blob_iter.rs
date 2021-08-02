#[cfg(feature = "ic")]
use crate::types::{size_of, Array, Bytes, SkewedPtr, Words, TAG_ARRAY};

use motoko_rts_macros::ic_mem_fn;

#[cfg(feature = "ic")]
const ITER_BLOB_IDX: u32 = 0;

#[cfg(feature = "ic")]
const ITER_POS_IDX: u32 = 1;

/// Returns iterator for the given blob
#[ic_mem_fn(ic_only)]
unsafe fn blob_iter<M: crate::memory::Memory>(mem: &mut M, blob: SkewedPtr) -> SkewedPtr {
    let iter_ptr = mem.alloc_words(size_of::<Array>() + Words(2));

    let iter_array = iter_ptr.unskew() as *mut Array;
    (*iter_array).header.tag = TAG_ARRAY;
    (*iter_array).len = 2;

    iter_array.set(ITER_BLOB_IDX, blob);
    iter_array.set(ITER_POS_IDX, SkewedPtr(0));

    iter_ptr
}

/// Returns whether the iterator is finished
#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn blob_iter_done(iter: SkewedPtr) -> u32 {
    let iter_array = iter.as_array();

    let blob = iter_array.get(ITER_BLOB_IDX);
    let pos = Bytes((iter_array.get(ITER_POS_IDX).0 >> 2) as u32);

    (pos >= blob.as_blob().len()).into()
}

/// Reads next byte, advances the iterator
#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn blob_iter_next(iter: SkewedPtr) -> u32 {
    let iter_array = iter.as_array();

    let blob = iter_array.get(ITER_BLOB_IDX);
    let pos = (iter_array.get(ITER_POS_IDX).0 >> 2) as u32;

    crate::write_barrier::write_barrier(
        iter_array.payload_addr().add(ITER_POS_IDX as usize) as usize
    );
    iter_array.set(ITER_POS_IDX, SkewedPtr(((pos + 1) << 2) as usize));

    blob.as_blob().get(pos).into()
}
