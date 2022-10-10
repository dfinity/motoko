//! Write barrier implementation and testing utilities

use core::ptr::null_mut;

use crate::mem_utils::memcpy_bytes;
use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::visit_pointer_fields;

const MAX_UPDATES: usize = 1024;

static mut UPDATED_FIELDS: [usize; MAX_UPDATES] = [0usize; MAX_UPDATES];

static mut N_UPDATED_FIELDS: usize = 0;

// loc: updated location (object field)
// new: value being written
//
// Called before writing the value, so `*loc` gives the old (current) value.
#[no_mangle]
pub unsafe extern "C" fn write_barrier(loc: usize) {
    //println!(100, "Write barrier {:#x}", loc);
    
    // Make sure we unskewed the object when calculating the field
    assert_eq!(loc & 0b1, 0);

    assert!(N_UPDATED_FIELDS < MAX_UPDATES);

    UPDATED_FIELDS[N_UPDATED_FIELDS] = loc;
    N_UPDATED_FIELDS += 1;
}
static mut SNAPSHOT: *mut Blob = null_mut();

pub unsafe fn take_snapshot<M: Memory>(mem: &mut M, hp: u32) {
    let length = Bytes(hp);
    let blob = alloc_blob(mem, length).get_ptr() as *mut Blob;
    memcpy_bytes(blob.payload_addr() as usize, 0, length);
    SNAPSHOT = blob;
    N_UPDATED_FIELDS = 0;
}

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
            }
        );
        pointer += object_size(current as usize).to_bytes().as_usize();
    }
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
