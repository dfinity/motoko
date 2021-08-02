//! This file implements the data structure the Motoko runtime uses to keep track of outstanding
//! continuations. It needs to support the following operations
//!
//!  1. Adding a continuation (any heap pointer) and getting an index (i32)
//!  2. Looking up a continuation by index, which also frees it
//!  3. Peek into an existing continuation and hand back additional data
//!  4. GC must be able to traverse and move continuations in the table
//!
//! By a continuation we understand any collection of callables that help to advance
//! the actor's control flow.
//! The current implementation stores the continuations in heap-allocated Motoko array.
//!
//! To efficiently look up the next free index, we use an implicit free list: `FREE_SLOT` is the
//! index into the array payload of the next free item. Each free item contains the index of the
//! next free item, shifted 2 bits to the left (to make the index a scalar and traverse them in
//! GC).
//!
//! The last item will have value `TABLE.len() << 2`, so after adding a continuation to the last free
//! slot `FREE_SLOT` will be `table_size`, which is when we see that the array is full.
//!
//! When the table is full, we double the size, copy the existing table, and add the second half to
//! the free list. Since all indices are relative to the payload begin, they stay valid. We never
//! shrink the table.

use crate::memory::{alloc_array, Memory};
use crate::rts_trap_with;
use crate::types::SkewedPtr;
use crate::write_barrier::write_barrier;

use motoko_rts_macros::ic_mem_fn;

const INITIAL_SIZE: u32 = 256;

// Skewed pointer to the `Array` object. This needs to be a skewed pointer to be able to pass its
// location to the GC.
static mut TABLE: SkewedPtr = SkewedPtr(0);

// Number of currently live continuations
static mut N_CONTINUATIONS: u32 = 0;

// Next free slot
static mut FREE_SLOT: u32 = 0;

unsafe fn create_continuation_table<M: Memory>(mem: &mut M) {
    TABLE = alloc_array(mem, INITIAL_SIZE);
    FREE_SLOT = 0;
    N_CONTINUATIONS = 0;

    let table = TABLE.as_array();
    for i in 0..INITIAL_SIZE {
        table.set(i, SkewedPtr((i as usize + 1) << 2));
    }
}

unsafe fn double_continuation_table<M: Memory>(mem: &mut M) {
    let old_array = TABLE.as_array();
    let old_size = old_array.len();

    assert_eq!(FREE_SLOT, old_size);

    let new_size = old_size * 2;

    TABLE = alloc_array(mem, new_size);
    let new_array = TABLE.as_array();

    for i in 0..old_size {
        new_array.set(i, old_array.get(i));
    }

    for i in old_size..new_size {
        new_array.set(i, SkewedPtr((i as usize + 1) << 2));
    }
}

#[ic_mem_fn]
pub unsafe fn remember_continuation<M: Memory>(mem: &mut M, ptr: SkewedPtr) -> u32 {
    if TABLE.0 == 0 {
        create_continuation_table(mem);
    }

    if FREE_SLOT == TABLE.as_array().len() {
        double_continuation_table(mem);
    }

    // Just as a sanity check make sure the ptr is really skewed
    if ptr.is_tagged_scalar() {
        rts_trap_with("remember_continuation: Argument is not a skewed pointer");
    }

    let idx = FREE_SLOT;

    let table = TABLE.as_array();

    FREE_SLOT = (table.get(idx).0 >> 2) as u32;

    write_barrier(table.payload_addr().add(idx as usize) as usize);
    table.set(idx, ptr);

    N_CONTINUATIONS += 1;

    idx
}

// Position of the future in explicit self-send ContinuationTable entries
// Invariant: keep this synchronised with compiler.ml (see future_array_index)
const FUTURE_ARRAY_INDEX: u32 = 2;

#[no_mangle]
pub unsafe extern "C" fn peek_future_continuation(idx: u32) -> SkewedPtr {
    if TABLE.0 == 0 {
        rts_trap_with("peek_future_continuation: Continuation table not allocated");
    }

    if idx >= TABLE.as_array().len() {
        rts_trap_with("peek_future_continuation: Continuation index out of range");
    }

    let ptr = TABLE.as_array().get(idx);

    if ptr.0 & 0b1 != 1 {
        rts_trap_with("peek_future_continuation: Continuation index not in table");
    }

    ptr.as_array().get(FUTURE_ARRAY_INDEX)
}

#[no_mangle]
pub unsafe extern "C" fn recall_continuation(idx: u32) -> SkewedPtr {
    if TABLE.0 == 0 {
        rts_trap_with("recall_continuation: Continuation table not allocated");
    }

    if idx >= TABLE.as_array().len() {
        rts_trap_with("recall_continuation: Continuation index out of range");
    }

    let table = TABLE.as_array();

    let ptr = table.get(idx);

    write_barrier(table.payload_addr().add(idx as usize) as usize);
    table.set(idx, SkewedPtr((FREE_SLOT << 2) as usize));

    FREE_SLOT = idx;

    N_CONTINUATIONS -= 1;

    if ptr.0 & 0b1 != 1 {
        rts_trap_with("recall_continuation: Continuation index not in table");
    }

    ptr
}

#[no_mangle]
pub unsafe extern "C" fn continuation_count() -> u32 {
    N_CONTINUATIONS
}

#[cfg(feature = "ic")]
pub(crate) unsafe fn continuation_table_loc() -> *mut SkewedPtr {
    &mut TABLE
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe extern "C" fn continuation_table_size() -> u32 {
    if TABLE.0 == 0 {
        0
    } else {
        TABLE.as_array().len()
    }
}
