//! Memory sanity checker
#![allow(dead_code)]

use crate::visitor::visit_pointer_fields;
use crate::{types::*, visitor::pointer_to_dynamic_heap};

use super::{Limits, Roots};

pub unsafe fn check_memory(limits: &Limits, roots: &Roots) {
    let checker = MemoryChecker { limits, roots };
    checker.check_memory();
}

pub struct MemoryChecker<'a> {
    limits: &'a Limits,
    roots: &'a Roots,
}

impl<'a> MemoryChecker<'a> {
    unsafe fn check_memory(&self) {
        self.check_static_roots();
        if (*self.roots.continuation_table_ptr_loc).is_ptr() {
            self.check_object(*self.roots.continuation_table_ptr_loc);
        }
        self.check_heap();
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
