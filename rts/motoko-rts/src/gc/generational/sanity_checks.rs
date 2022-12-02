//! Extensive sanity checks for generational GC features.
//! * Write barrier coverage by memory snapshot comparisons.
//! * Memory sanity check, including a full heap scan.
#![allow(dead_code)]

use core::ptr::null_mut;

use super::write_barrier::REMEMBERED_SET;
use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_collectable_blob, Memory};
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

static mut SNAPSHOT: *mut Blob = null_mut();

/// Take a memory snapshot. To be initiated after GC run.
pub unsafe fn take_snapshot<M: Memory>(mem: &mut M) {
    let length = Bytes(mem.heap_pointer());
    let blob = alloc_collectable_blob(mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
}

/// Verify write barrier coverage by comparing the memory against the previous snapshot.
/// To be initiated before the next GC run. No effect if no snapshpot has been taken.
pub unsafe fn verify_snapshot<M: Memory>(mem: &mut M, verify_roots: bool) {
    if SNAPSHOT.is_null() {
        return;
    }
    assert!(mem.heap_base() <= mem.heap_pointer());
    if verify_roots {
        verify_static_roots(mem);
    }
    verify_heap(mem);
    SNAPSHOT = null_mut();
}

unsafe fn verify_static_roots<M: Memory>(mem: &mut M) {
    let static_roots = mem.roots().static_roots.as_array();
    for index in 0..static_roots.len() {
        let current = static_roots.get(index).as_obj();
        assert_eq!(current.tag(), TAG_MUTBOX); // check tag
        let mutbox = current as *mut MutBox;
        let current_field = &mut (*mutbox).field;
        if relevant_field(mem, current_field) {
            verify_field(current_field);
        }
    }
}

unsafe fn verify_heap<M: Memory>(mem: &mut M) {
    assert!(SNAPSHOT.len().as_usize() <= mem.heap_pointer() as usize);
    let mut pointer = mem.heap_base() as usize;
    while pointer < SNAPSHOT.len().as_usize() {
        let current = pointer as *mut Obj;
        let previous = (SNAPSHOT.payload_addr() as usize + pointer) as *mut Obj;
        assert!(current.tag() == previous.tag());
        visit_pointer_fields(
            mem,
            current,
            current.tag(),
            0,
            |mem, current_field| {
                if relevant_field(mem, current_field) {
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
}

unsafe fn relevant_field<M: Memory>(mem: &mut M, current_field: *mut Value) -> bool {
    if (current_field as usize) < mem.last_heap_pointer() as usize {
        let value = *current_field;
        value.is_ptr() && value.get_ptr() as usize >= mem.last_heap_pointer() as usize
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
        self.check_static_roots();
        self.check_continuation_table();
        self.check_heap();
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.mem.roots().static_roots.as_array();
        for i in 0..root_array.len() {
            let obj = root_array.get(i).as_obj();
            assert_eq!(obj.tag(), TAG_MUTBOX);
            assert!((obj as usize) < self.mem.heap_base() as usize);
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.mem.heap_base() as usize) {
                let object = *field_addr;
                self.check_object(object);
            }
        }
    }

    unsafe fn check_continuation_table(&self) {
        let continuation_table = *self.mem.roots().continuation_table_address;
        if continuation_table.is_ptr() {
            self.check_object(continuation_table);
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
                if (*field_address).get_ptr() as *mut Obj != null_mut() {
                    (&self).check_object_header(*field_address);
                }
            },
            |_, _, arr| arr.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_ptr());
        let pointer = object.get_ptr();
        assert!(pointer < self.mem.heap_pointer() as usize);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.mem.heap_base() as usize;
        while pointer < self.mem.heap_pointer() as usize {
            let object = Value::from_ptr(pointer as usize);
            if object.tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            pointer += object_size(pointer as usize).to_bytes().as_usize();
        }
    }
}
