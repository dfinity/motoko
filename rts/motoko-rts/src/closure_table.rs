//! This file implements the data structure the Motoko runtime uses to keep track of outstanding
//! closures. It needs to support the following operations
//!
//!  1. Adding a closure (any heap pointer) and getting an index (i32)
//!  2. Looking up a closure by index, which also frees it
//!  3. GC must be able to traverse and move clsoures in the table
//!  4. Obtain number of closures registered (TODO (osa): Why?)
//!  5. Obtain size of table (TODO (osa): Why?)
//!
//! Current implementation stores the closures in heap-allocated Motoko array.
//!
//! To efficiently look up the next free index, we use an implicit free list: `FREE_SLOT` is the
//! index into the array payload of the next free item. Each free item contains the index of the
//! next free item, shifted 2 bits to the left (to make the index a scalar and traverse them in
//! GC).
//!
//! The last item will have value `TABLE.len() << 2`, so after adding a closure to the last free
//! slot `FREE_SLOT` will be `table_size`, which is when we see that the array is full.
//!
//! When the table is full, we double the size, copy the existing table, and add the second half to
//! the free list. Since all indices are relative to the payload begin, they stay valid. We never
//! shrink the table.

use crate::alloc::alloc_array;
use crate::rts_trap_with;
use crate::types::{Array, SkewedPtr};

const INITIAL_SIZE: u32 = 256;

// Skewed pointer to the `Array` object. This needs to be a skewed pointer to be able to pass its
// location to the GC.
static mut TABLE: SkewedPtr = SkewedPtr(0);

// Number of currently live closures
static mut N_CLOSURES: u32 = 0;

// Next free slot
static mut FREE_SLOT: u32 = 0;

unsafe fn crate_closure_table() {
    TABLE = alloc_array(INITIAL_SIZE);
    FREE_SLOT = 0;
    N_CLOSURES = 0;

    let table: *mut Array = TABLE.unskew() as *mut Array;
    for i in 0..INITIAL_SIZE {
        table.set(i, SkewedPtr((i as usize + 1) << 2));
    }
}

unsafe fn double_closure_table() {
    let old_array = TABLE.unskew() as *mut Array;
    let old_size = (old_array as *const Array).len();

    assert_eq!(FREE_SLOT, old_size);

    let new_size = old_size * 2;

    TABLE = alloc_array(new_size);
    let new_array = TABLE.unskew() as *mut Array;

    for i in 0..old_size {
        new_array.set(i, (old_array as *const Array).get(i));
    }

    for i in old_size..new_size {
        new_array.set(i, SkewedPtr((i as usize + 1) << 2));
    }
}

#[no_mangle]
pub unsafe extern "C" fn remember_closure(ptr: SkewedPtr) -> u32 {
    if TABLE.0 == 0 {
        crate_closure_table();
    }

    if FREE_SLOT == (TABLE.unskew() as *const Array).len() {
        double_closure_table();
    }

    // Just as a sanity check make sure the ptr is really skewed
    if ptr.0 & 0b1 != 1 {
        rts_trap_with("remember_closure: Argument is not a skewed pointer\0".as_ptr());
    }

    let idx = FREE_SLOT;

    FREE_SLOT = ((TABLE.unskew() as *const Array).get(idx).0 >> 2) as u32;
    (TABLE.unskew() as *mut Array).set(idx, ptr);
    N_CLOSURES += 1;

    idx
}

#[no_mangle]
pub unsafe extern "C" fn recall_closure(idx: u32) -> SkewedPtr {
    if TABLE.0 == 0 {
        rts_trap_with("recall_closure: Closure table not allocated\0".as_ptr());
    }

    if idx >= (TABLE.unskew() as *const Array).len() {
        rts_trap_with("recall_closure: Closure index out of range\0".as_ptr());
    }

    let ptr = (TABLE.unskew() as *const Array).get(idx);

    (TABLE.unskew() as *mut Array).set(idx, SkewedPtr((FREE_SLOT << 2) as usize));
    FREE_SLOT = idx;

    N_CLOSURES -= 1;

    if ptr.0 & 0b1 != 1 {
        rts_trap_with("recall_closure: Closure index not in table\0".as_ptr());
    }

    ptr
}

#[no_mangle]
pub unsafe extern "C" fn closure_count() -> u32 {
    N_CLOSURES
}

pub(crate) unsafe fn closure_table_loc() -> *mut SkewedPtr {
    &mut TABLE
}

#[no_mangle]
unsafe extern "C" fn closure_table_size() -> u32 {
    if TABLE.0 == 0 {
        0
    } else {
        (TABLE.unskew() as *const Array).len()
    }
}
