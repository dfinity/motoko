//! Extensive sanity checks for generational GC features.
//! * Write barrier coverage by memory snapshot comparisons.
//! * Memory sanity check, including a full heap scan.
#![allow(dead_code)]

use core::ptr::null_mut;

use super::write_barrier::REMEMBERED_SET;
use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

static mut SNAPSHOT: *mut Blob = null_mut();

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(mem: &mut M) {
    let length = Bytes(mem.get_heap_pointer() as u32);
    let blob = alloc_blob(mem, length).get_object_address() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
}

/// Verify write barrier coverage by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot<M: Memory>(mem: &mut M, verify_roots: bool) {
    if SNAPSHOT.is_null() {
        return;
    }
    assert!(mem.get_heap_base() <= mem.get_heap_pointer());
    if verify_roots {
        verify_static_roots(
            mem.get_roots().static_roots.as_array(),
            mem.get_heap_pointer(),
        );
    }
    verify_heap(mem);
    (SNAPSHOT as *const Obj).object_id().free_object_id();
    SNAPSHOT = null_mut();
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

unsafe fn verify_heap<M: Memory>(mem: &mut M) {
    assert!(SNAPSHOT.len().as_usize() <= mem.get_heap_pointer());
    let mut pointer = mem.get_heap_base();
    while pointer < SNAPSHOT.len().as_usize() {
        let tag = *(pointer as *const Tag);
        if has_object_header(tag) {
            let current = pointer as *mut Obj;
            let previous = (SNAPSHOT.payload_addr() as usize + pointer) as *mut Obj;
            assert!(current.tag() == previous.tag());
            visit_pointer_fields(
                &mut (),
                current,
                current.tag(),
                0,
                |_, current_field| {
                    if relevant_field(current_field, mem.get_last_heap_pointer()) {
                        verify_field(current_field);
                    }
                },
                |_, slice_start, arr| {
                    assert!(slice_start == 0);
                    arr.len()
                },
            );
        }
        pointer += block_size(pointer).to_bytes().as_usize();
    }
}

unsafe fn relevant_field(current_field: *mut Value, last_free: usize) -> bool {
    if (current_field as usize) < last_free {
        let value = *current_field;
        value.is_object_id() && value.get_object_address() as usize >= last_free
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
        Some(remembered_set) => remembered_set.contains(Value::from_raw(value)),
    }
}

pub struct MemoryChecker<'a, M: Memory> {
    mem: &'a mut M,
}

pub unsafe fn check_memory<M: Memory>(mem: &mut M) {
    let checker = MemoryChecker { mem };
    checker.check_memory();
}

impl<'a, M: Memory> MemoryChecker<'a, M> {
    unsafe fn check_memory(&self) {
        let roots = self.mem.get_roots();
        self.check_static_roots(roots.static_roots);
        if (*roots.continuation_table_location).is_object_id() {
            self.check_object(*roots.continuation_table_location);
        }
        self.check_heap();
    }

    unsafe fn check_static_roots(&self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for i in 0..root_array.len() {
            let obj = root_array.get(i).as_obj();
            assert_eq!(obj.tag(), TAG_MUTBOX);
            assert!((obj as usize) < self.mem.get_heap_base());
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.mem.get_heap_base()) {
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
                // Ignore null pointers used in text_iter.
                if (*field_address).get_object_address() as *mut Obj != null_mut() {
                    (&self).check_object_header(*field_address);
                }
            },
            |_, _, arr| arr.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_object_id());
        let pointer = object.get_object_address();
        assert!(pointer < self.mem.get_heap_pointer());
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.mem.get_heap_base();
        while pointer < self.mem.get_heap_pointer() {
            let tag = *(pointer as *const Tag);
            if has_object_header(tag) {
                let object = pointer as *mut Obj;
                assert!(has_object_header((*object).tag));
                self.check_object(object.object_id());
            }
            pointer += block_size(pointer as usize).to_bytes().as_usize();
        }
    }
}
