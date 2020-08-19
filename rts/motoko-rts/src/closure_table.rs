/*
This file implements the data structure the Motoko runtime uses to keep track of outstanding
closures. It needs to support the following operations

 1. Adding a closure (any heap pointer) and getting an index (i32)
 2. Looking up a closure by index, which also frees it
 3. GC must traverse all closures, and possibly move the table
 4. Obtain number of closures registered
 5. Obtain size of table

This stores the closures in a normal, heap-allocated, Motoko array. This means
3. is simple: just traverse this array in GC as normal.

To efficiently look up the next free things, we use an implicit free list:
`free_slot` is the index (shifted 2 bit to the left) into the array payload of
the next free item. Each free item contains the index of another free item.
The last free slot is marked as (-1 << 2).

We shift these 2 bit to the left so that GC treats them as scalars, not as
pointers. It also means they can be used as a byte offset, but that is more
cute than actually important.

When the table is full, we double the size, copy the existing table, and add the
second half to the free list. Since all indices are relative to the payload
begin, they stay valid. We never shrink the table.
*/

#![allow(dead_code)]

use crate::alloc;
use crate::common::{rts_trap_with, FmtWrite};
use crate::types::*;

use core::fmt::Write;

const FULL: u32 = (-1i32 as u32) << 2;
const INITIAL_SIZE: u32 = 256;

static mut TABLE: *mut Array = ::core::ptr::null_mut();
static mut N_CLOSURES: u32 = 0;
static mut FREE_SLOT: u32 = FULL;

const ARRAY_HEADER_SIZE: Words<u32> = Words(2);

unsafe fn alloc_array(len: u32) -> *mut Array {
    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(&mut fmt, "alloc_array: len={}\n", len,).unwrap();
    fmt.print();

    // 2 = Array header size
    let ptr = alloc::alloc_words(Words(ARRAY_HEADER_SIZE.0 + len)).unskew() as *mut Array;
    (*ptr).header.tag = TAG_ARRAY;
    (*ptr).len = len;
    ptr
}

unsafe fn create_closure_table() {
    TABLE = alloc_array(INITIAL_SIZE);
    FREE_SLOT = 0;
    for i in 0..INITIAL_SIZE - 1 {
        array_set(TABLE, i, (i + 1) << 2);
    }
    array_set(TABLE, INITIAL_SIZE - 1, FULL);

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "create_closure_table: table len={}\0",
        (*TABLE).len
    )
    .unwrap();
    fmt.print();
}

unsafe fn double_closure_table() {
    let old_size = (*TABLE).len;
    let new_size = old_size * 2;
    let old_table = TABLE;
    let new_table = alloc_array(new_size);

    for i in 0..old_size {
        array_set(new_table, i, array_get(old_table, i));
    }

    for i in old_size..new_size - 1 {
        array_set(new_table, i, (i + 1) << 2);
    }

    array_set(new_table, new_size - 1, FREE_SLOT);
    FREE_SLOT = old_size << 2;
    TABLE = new_table;

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "double_closure_table: table len={}\0",
        (*TABLE).len
    )
    .unwrap();
    fmt.print();
}

#[no_mangle]
pub unsafe extern "C" fn remember_closure(p: SkewedPtr) -> u32 {
    if TABLE.is_null() {
        create_closure_table();
    } else if FREE_SLOT == FULL {
        double_closure_table();
    }

    if !p.check_sanity() {
        let mut buf = [0 as u8; 200];
        let mut fmt = FmtWrite::new(&mut buf);
        write!(
            &mut fmt,
            "remember_closure: Storing unboxed literals not supported (p={:#x})\0",
            p.0
        )
        .unwrap();
        rts_trap_with(buf.as_ptr());
    }

    let free_idx = FREE_SLOT >> 2;
    FREE_SLOT = array_get(TABLE, free_idx);
    array_set(TABLE, free_idx, p.0 as u32);
    N_CLOSURES += 1;

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "remember_closure: table={:#x}, {:#x} --> {} (n_closures={}, table len={})",
        TABLE as usize,
        p.unskew(),
        free_idx,
        N_CLOSURES,
        (*TABLE).len,
    )
    .unwrap();
    fmt.print();

    free_idx
}

#[no_mangle]
pub unsafe extern "C" fn recall_closure(idx: u32) -> SkewedPtr {
    if TABLE.is_null() {
        rts_trap_with("recall_closure: Closure table not allocated\0".as_ptr());
    }

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "recall_closure: table={:#x}, idx={}, n_closures={}, table len={}\n",
        TABLE as usize,
        idx,
        N_CLOSURES,
        (*TABLE).len
    )
    .unwrap();
    fmt.print();

    if idx >= (*TABLE).len {
        let mut buf = [0 as u8; 200];
        let mut fmt = FmtWrite::new(&mut buf);
        write!(
            &mut fmt,
            "recall_closure: Closure index out of range (idx={}, n_closures={}, table len={})\n",
            idx,
            N_CLOSURES,
            (*TABLE).len
        )
        .unwrap();
        rts_trap_with(buf.as_ptr());
    }

    let cls = array_get(TABLE, idx);
    array_set(TABLE, idx, FREE_SLOT);
    FREE_SLOT = idx << 2;
    N_CLOSURES -= 1;

    if cls & 0b10 != 0b10 {
        let mut buf = [0 as u8; 200];
        let mut fmt = FmtWrite::new(&mut buf);
        write!(
            &mut fmt,
            "recall_closure: Element is not a skewed ptr (idx={}, elem={:#x})\0",
            idx, cls
        )
        .unwrap();
        rts_trap_with(buf.as_ptr());
    }

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "recall_closure: {} --> {:#x}, table_len={}\n",
        idx,
        cls,
        (*TABLE).len
    )
    .unwrap();
    fmt.print();

    SkewedPtr(cls as usize) // TODO: check sanity
}

#[no_mangle]
pub unsafe extern "C" fn closure_count() -> u32 {
    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(&mut fmt, "closure_count: {}\n", N_CLOSURES).unwrap();
    fmt.print();

    let mut buf = [0 as u8; 200];
    let mut fmt = FmtWrite::new(&mut buf);
    write!(
        &mut fmt,
        "closure_count: table len={}\0",
        (*TABLE).len
    )
    .unwrap();
    fmt.print();

    N_CLOSURES
}

#[no_mangle]
pub unsafe extern "C" fn closure_table_loc() -> SkewedPtr {
    skew((&TABLE as *const _) as usize)
}

#[no_mangle]
pub unsafe extern "C" fn closure_table_size() -> u32 {
    if TABLE.is_null() {
        let mut buf = [0 as u8; 200];
        let mut fmt = FmtWrite::new(&mut buf);
        write!(&mut fmt, "closure_table_size len=0\n").unwrap();
        fmt.print();
        0
    } else {
        let mut buf = [0 as u8; 200];
        let mut fmt = FmtWrite::new(&mut buf);
        write!(&mut fmt, "closure_table_size len={}\n", (*TABLE).len).unwrap();
        fmt.print();
        (*TABLE).len
    }
}
