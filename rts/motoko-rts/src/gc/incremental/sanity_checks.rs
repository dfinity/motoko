//! Memory sanity checker
#![allow(dead_code)]

use motoko_rts_macros::ic_mem_fn;

use crate::memory::Memory;
use crate::visitor::visit_pointer_fields;
use crate::{types::*, visitor::pointer_to_dynamic_heap};

#[ic_mem_fn(ic_only)]
pub unsafe fn check_memory<M: Memory>(_mem: &mut M) {
    use crate::memory::ic;

    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *crate::continuation_table::continuation_table_loc(),
    };
    let checker = MemoryChecker { limits, roots };
    checker.check_memory();
}

struct Limits {
    pub base: usize,
    pub free: usize,
}

struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
}

struct MemoryChecker {
    limits: Limits,
    roots: Roots,
}

impl MemoryChecker {
    unsafe fn check_memory(&self) {
        self.check_static_roots();
        if self.roots.continuation_table.is_ptr() {
            self.check_object(self.roots.continuation_table);
        }
        self.check_heap();
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let mutbox = root_array.get(i).as_mutbox();
            assert!((mutbox as usize) < self.limits.base);
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.limits.base) {
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
