//! Extensive sanity checks for experimental GC features.
//! * Write barrier coverage by memory snapshot comparisons.
#![allow(dead_code)]

use core::ptr::null_mut;

use super::write_barrier::{N_UPDATED_FIELDS, UPDATED_FIELDS};
use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::visit_pointer_fields;

static mut SNAPSHOT: *mut Blob = null_mut();

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(mem: &mut M, hp: u32) {
    let length = Bytes(hp);
    let blob = alloc_blob(mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
    N_UPDATED_FIELDS = 0;
}

/// Verify write barrier coverag by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot(heap_base: u32, hp: u32, static_roots: Value) {
    assert!(heap_base <= hp);
    verify_static_roots(static_roots.as_array());
    verify_heap(heap_base as usize, hp as usize);
}

unsafe fn verify_static_roots(static_roots: *mut Array) {
    for index in 0..static_roots.len() {
        let current = static_roots.get(index).as_obj();
        assert_eq!(current.tag(), TAG_MUTBOX); // check tag
        let mutbox = current as *mut MutBox;
        let current_field = &mut (*mutbox).field;
        verify_field(current_field);
    }
}

unsafe fn verify_heap(base: usize, limit: usize) {
    if SNAPSHOT.is_null() {
        return;
    }
    println!(100, "Heap verification starts...");
    assert!(SNAPSHOT.len().as_usize() <= limit);
    let mut pointer = base;
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
                verify_field(current_field);
            },
            |_, slice_start, arr| {
                assert!(slice_start == 0);
                arr.len()
            },
        );
        pointer += object_size(current as usize).to_bytes().as_usize();
    }
    println!(100, "Heap verification stops...");
}

unsafe fn verify_field(current_field: *mut Value) {
    let memory_copy = SNAPSHOT.payload_addr() as usize;
    let previous_field = (memory_copy + current_field as usize) as *mut Value;
    if *previous_field != *current_field {
        // TODO: Faster search
        let mut found = false;
        for updated_field_idx in 0..N_UPDATED_FIELDS {
            if UPDATED_FIELDS[updated_field_idx] == current_field as usize {
                found = true;
                break;
            }
        }
        if !found {
            panic!("Missing write barrier at {:#x}", current_field as usize);
        }
    }
}
