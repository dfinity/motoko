//! Write barrier implementation and testing utilities

use crate::memory::{alloc_blob, Memory};
use crate::types::*;
use crate::visitor::visit_pointer_fields;

static mut UPDATED_FIELDS: [usize; 1024] = [0usize; 1024];

static mut N_UPDATED_FIELDS: usize = 0;

static mut HEAP_COPY: *mut Blob = core::ptr::null_mut();

// loc: updated location (object field)
// new: value being written
//
// Called before writing the value, so `*loc` gives the old (current) value.
#[no_mangle]
pub unsafe extern "C" fn write_barrier(loc: usize) {
    assert!(N_UPDATED_FIELDS < 1024);

    println!(100, "Write barrier recording {:#x}", loc);

    UPDATED_FIELDS[N_UPDATED_FIELDS] = loc;
    N_UPDATED_FIELDS += 1;
}

/// Copy the current heap, store it in heap as a blob. Update `HEAP_COPY_BLOB` to the location of
/// the blob. This copy will then be used to check write barrier correctness, by `check_heap_copy`.
pub unsafe fn copy_heap<M: Memory>(mem: &mut M, heap_base: u32, hp: u32) {
    let blob_size = Bytes(hp - heap_base);
    let blob = alloc_blob(mem, blob_size).unskew() as *mut Blob;
    let mut payload = blob.payload_addr() as *mut u32;

    let mut p = heap_base;
    while p < hp {
        *payload = *(p as *const u32);
        payload = payload.add(1);
        p += core::mem::size_of::<u32>() as u32;
    }

    HEAP_COPY = blob;
    N_UPDATED_FIELDS = 0;
}

pub unsafe fn check_heap_copy(heap_base: u32, hp: u32) {
    if HEAP_COPY.is_null() {
        return;
    }

    assert!((HEAP_COPY as u32) < hp);

    let mut copy_ptr = HEAP_COPY.payload_addr() as *mut u32;

    let mut p = heap_base;
    while p < HEAP_COPY as u32 {
        let obj_heap = p as *mut Obj;
        let obj_copy = copy_ptr as *mut Obj;
        let diff = obj_copy as usize - obj_heap as usize;

        let tag = obj_heap.tag();

        assert_eq!(tag, obj_copy.tag());

        visit_pointer_fields(obj_heap, tag, heap_base as usize, |obj_field_ptr| {
            let copy_field_ptr = (obj_field_ptr as usize + diff) as *mut SkewedPtr;

            let obj_field_value = *obj_field_ptr;
            let copy_field_value = *copy_field_ptr;

            if obj_field_value != copy_field_value {
                // Field was updated, check that we called the write barrier
                let mut found = false;
                for updated_field_idx in 0..N_UPDATED_FIELDS {
                    if UPDATED_FIELDS[updated_field_idx] == obj_field_ptr as usize {
                        found = true;
                        break;
                    }
                }

                if !found {
                    panic!(
                        "Updated field {:#x} of object {:#x} ({}) not found in \
                         updated fields recorded by write barrier",
                        obj_heap as usize,
                        obj_field_ptr as usize,
                        tag_str(tag),
                    );
                }
            }
        });

        let size: Words<u32> = object_size(obj_heap as usize);

        p += size.to_bytes().0;
        copy_ptr = copy_ptr.add(size.0 as usize);
    }
}
