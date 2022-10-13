//! Extensive sanity checks for experimental GC features.
//! * Write barrier coverage by memory snapshot comparisons.
#![allow(dead_code)]

use core::ptr::null_mut;

use super::write_barrier::REMEMBERED_SET;
use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

static mut SNAPSHOT: *mut Blob = null_mut();

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(mem: &mut M, hp: u32) {
    let length = Bytes(hp);
    let blob = alloc_blob(mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
}

/// Verify write barrier coverag by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot(
    heap_base: u32,
    last_free: u32,
    hp: u32,
    static_roots: Value,
    verify_roots: bool,
) {
    assert!(heap_base <= hp);
    if verify_roots {
        verify_static_roots(static_roots.as_array(), last_free as usize);
    }
    verify_heap(heap_base as usize, last_free as usize, hp as usize);
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

unsafe fn verify_heap(base: usize, last_free: usize, limit: usize) {
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
                if relevant_field(current_field, last_free) {
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

pub struct MemoryChecker {
    heap_base: u32,
    last_hp: u32,
    hp: u32,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
}

pub unsafe fn check_memory(
    heap_base: u32,
    last_hp: u32,
    hp: u32,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
) {
    let checker = MemoryChecker {
        heap_base,
        last_hp,
        hp,
        static_roots,
        continuation_table_ptr_loc,
    };
    checker.check_memory();
}

impl MemoryChecker {
    unsafe fn check_memory(&self) {
        println!(100, "Memory check starts...");
        println!(100, "  Static roots...");
        self.check_static_roots();
        if (*self.continuation_table_ptr_loc).is_ptr() {
            println!(100, "  Continuation table...");
            self.check_object(*self.continuation_table_ptr_loc);
        }
        println!(100, "  Heap...");
        self.check_heap();
        println!(100, "Memory check stops...");
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.static_roots.as_array();
        for i in 0..root_array.len() {
            let obj = root_array.get(i).as_obj();
            assert_eq!(obj.tag(), TAG_MUTBOX);
            assert!((obj as u32) < self.heap_base);
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.heap_base as usize) {
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
        let pointer = object.get_ptr() as u32;
        assert!(pointer < self.hp);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        let forward = object.forward();
        assert!(forward.is_null_ptr() || forward.get_ptr() == pointer as usize);
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.heap_base;
        while pointer < self.hp {
            let object = Value::from_ptr(pointer as usize);
            if object.tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            let object_size = object_size(pointer as usize).to_bytes().as_usize();
            pointer += object_size as u32;
        }
    }
}
