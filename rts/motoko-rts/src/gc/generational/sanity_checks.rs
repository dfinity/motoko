//! Extensive sanity checks for geberational GC features.
//! * Write barrier coverage by memory snapshot comparisons.
//! * Memory sanity check, including a full heap scan.
#![allow(dead_code)]

use core::ptr::null_mut;

use super::write_barrier::REMEMBERED_SET;
use super::{Heap, Limits, Roots};
use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

static mut SNAPSHOT: *mut Blob = null_mut();

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(heap: &mut Heap<M>) {
    let length = Bytes(heap.limits.free as u32);
    let blob = alloc_blob(heap.mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
}

/// Verify write barrier coverag by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot<M: Memory>(heap: &Heap<M>, verify_roots: bool) {
    assert!(heap.limits.base <= heap.limits.free);
    if verify_roots {
        verify_static_roots(heap.roots.static_roots.as_array(), heap.limits.free);
    }
    verify_heap(&heap.limits);
}

unsafe fn verify_static_roots(static_roots: *mut Array, last_free: usize) {
    for index in 0..static_roots.len() {
        let current = static_roots.get(index).as_obj();
        assert_eq!(current.tag(), TAG_MUTBOX); // check tag
        let mutbox = current as *mut MutBox;
        let current_field = &mut (*mutbox).field;
        if relevant_field(current_field, last_free) {
            verify_field(current_field);
        }
    }
}

unsafe fn verify_heap(limits: &Limits) {
    if SNAPSHOT.is_null() {
        return;
    }
    println!(100, "Heap verification starts...");
    assert!(SNAPSHOT.len().as_usize() <= limits.free);
    let mut pointer = limits.base;
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
                if relevant_field(current_field, limits.last_free) {
                    verify_field(current_field);
                }
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

unsafe fn relevant_field(current_field: *mut Value, last_free: usize) -> bool {
    if (current_field as usize) < last_free {
        let value = *current_field;
        value.is_ptr() && value.get_raw() as usize >= last_free
    } else {
        false
    }
}

unsafe fn verify_field(current_field: *mut Value) {
    let memory_copy = SNAPSHOT.payload_addr() as usize;
    let previous_field = (memory_copy + current_field as usize) as *mut Value;
    if *previous_field != *current_field && !recorded(current_field as u32) {
        panic!("Missing write barrier at {:#x}", current_field as usize);
    }
}

unsafe fn recorded(value: u32) -> bool {
    match &REMEMBERED_SET {
        None => panic!("No remembered set"),
        Some(remembered_set) => {
            let mut iterator = remembered_set.iterate();
            while iterator.has_next() {
                if iterator.current().get_raw() == value {
                    return true;
                }
                iterator.next();
            }
            false
        }
    }
}

pub struct MemoryChecker<'a> {
    limits: &'a Limits,
    roots: &'a Roots,
}

pub unsafe fn check_memory(limits: &Limits, roots: &Roots) {
    let checker = MemoryChecker { limits, roots };
    checker.check_memory();
}

impl<'a> MemoryChecker<'a> {
    unsafe fn check_memory(&self) {
        println!(100, "Memory check starts...");
        println!(100, "  Static roots...");
        self.check_static_roots();
        if (*self.roots.continuation_table_ptr_loc).is_ptr() {
            println!(100, "  Continuation table...");
            self.check_object(*self.roots.continuation_table_ptr_loc);
        }
        println!(100, "  Heap...");
        self.check_heap();
        println!(100, "Memory check stops...");
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let obj = root_array.get(i).as_obj();
            assert_eq!(obj.tag(), TAG_MUTBOX);
            assert!((obj as usize) < self.limits.base);
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.limits.base as usize) {
                let object = *field_addr;
                self.check_object(object);
            }
        }
    }

    unsafe fn check_object(&self, object: Value) {
        self.check_object_header(object);
        visit_pointer_fields(
            &mut (),
            object.as_obj(),
            object.tag(),
            0,
            |_, field_address| {
                (&self).check_object_header(*field_address);
            },
            |_, _, arr| arr.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_ptr());
        let pointer = object.get_ptr();
        assert!(pointer < self.limits.free);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        let forward = object.forward();
        assert!(forward.is_null_ptr() || forward.get_ptr() == pointer);
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.limits.base;
        while pointer < self.limits.free {
            let object = Value::from_ptr(pointer as usize);
            if object.tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            pointer += object_size(pointer as usize).to_bytes().as_usize();
        }
    }
}
