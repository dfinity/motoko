//! Extensive sanity checks for geberational GC features.
//! * Write barrier coverage by memory snapshot comparisons.
//! * Memory sanity check, including a full heap scan.
#![allow(dead_code)]

use core::ptr::null_mut;

use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::visit_pointer_fields;

use super::remembered_set::RememberedSet;

struct Heap {
    pub limits: Limits,
    pub roots: Roots,
}

struct Roots {
    pub static_roots: Value,
}

struct Limits {
    pub base: usize,
    pub free: usize,
}

// Note: Both the snapshot and the remembered set will be collected by the GC.
static mut SNAPSHOT: *mut Blob = null_mut();
static mut REMEMBERED_SET: Option<RememberedSet> = None;

/// (Re-)create the write barrier barrier's remembered set.
pub unsafe fn init_write_barrier<M: Memory>(mem: &mut M) {
    REMEMBERED_SET = Some(RememberedSet::new(mem));
}

/// Record write in object (skewed), to be called for each pointers store.
/// May also be conservatively called when writing a scalar value.
pub unsafe fn record_write<M: Memory>(mem: &mut M, object: Value) {
    match &mut REMEMBERED_SET {
        None => return,
        Some(remembered_set) => {
            remembered_set.insert(mem, object);
        }
    }
}

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(mem: &mut M, heap_free: usize) {
    let length = Bytes(heap_free as u32);
    let blob = alloc_blob(mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
    init_write_barrier(mem);
}

/// Verify write barrier coverag by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot(heap_base: usize, heap_free: usize, static_roots: Value) {
    if SNAPSHOT.is_null() {
        return;
    }
    assert!(heap_base <= heap_free);
    verify_static_roots(static_roots.as_array());
    verify_heap(heap_base, heap_free);
    SNAPSHOT = null_mut(); // will be collected by the GC
    REMEMBERED_SET = None; // will be collected by the GC
}

unsafe fn verify_static_roots(static_roots: *mut Array) {
    for index in 0..static_roots.len() {
        let current = static_roots.get(index).as_obj();
        assert_eq!(current.tag(), TAG_MUTBOX); // check tag
        let mutbox = current as *mut MutBox;
        let current_field = &mut (*mutbox).field;
        verify_field(current, current_field);
    }
}

unsafe fn verify_heap(heap_base: usize, heap_free: usize) {
    assert!(SNAPSHOT.len().as_usize() <= heap_free);
    let mut pointer = heap_base;
    while pointer < SNAPSHOT.len().as_usize() {
        let current = pointer as *mut Obj;
        let previous = (SNAPSHOT.payload_addr() as usize + pointer) as *mut Obj;
        assert!(current.tag() == previous.tag());
        visit_pointer_fields(
            &mut (),
            current,
            current.tag(),
            0,
            |_, current_field| {
                verify_field(current, current_field);
            },
            |_, slice_start, arr| {
                assert!(slice_start == 0);
                arr.len()
            },
        );
        pointer += object_size(current as usize).to_bytes().as_usize();
    }
}

unsafe fn verify_field(current_object: *mut Obj, current_field: *mut Value) {
    let memory_copy = SNAPSHOT.payload_addr() as usize;
    let previous_field = (memory_copy + current_field as usize) as *mut Value;
    if *previous_field != *current_field && !recorded(current_object) {
        panic!("Missing write barrier at {:#x}", current_field as usize);
    }
}

unsafe fn recorded(object: *mut Obj) -> bool {
    REMEMBERED_SET
        .as_ref()
        .unwrap()
        .contains(Value::from_ptr(object as usize))
}
