//! Incremental GC sanity checker
#![allow(dead_code)]

use core::ptr::null_mut;

use crate::gc::generational::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::visitor::visit_pointer_fields;
use crate::{types::*, visitor::pointer_to_dynamic_heap};

use super::free_list::FreeBlock;
use super::mark_stack::MarkStack;

#[cfg(feature = "ic")]
pub unsafe fn check_mark_completeness<M: Memory>(mem: &mut M) {
    let mark_stack = MarkStack::new(mem);
    let visited = RememberedSet::new(mem);
    let mut checker = MarkPhaseChecker {
        mem,
        mark_stack,
        visited,
    };
    checker.check_mark_completeness();
}

pub unsafe fn check_memory<M: Memory>(mem: &mut M, allow_marked_objects: bool) {
    let checker = MemoryChecker {
        mem,
        allow_marked_objects,
    };
    checker.check_memory();
}

struct MarkPhaseChecker<'a, M: Memory> {
    mem: &'a mut M,
    mark_stack: MarkStack,
    visited: RememberedSet,
}

impl<'a, M: Memory> MarkPhaseChecker<'a, M> {
    // Check that the set of marked objects by the incremental GC is the same set or a
    // subset of the objects being marked by a conventional stop-the-world mark phase.
    // The incremental GC may mark more objects due to concurrent allocations and
    // concurrent pointer modifications.
    unsafe fn check_mark_completeness(&mut self) {
        self.mark_roots();
        self.mark_all_reachable();
    }

    unsafe fn mark_roots(&mut self) {
        self.mark_static_roots();
        self.mark_continuation_table();
    }

    unsafe fn mark_static_roots(&mut self) {
        let root_array = self.mem.roots().static_roots.as_array();
        for i in 0..root_array.len() {
            let mutbox = root_array.get(i).as_mutbox();
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.mem.heap_base() as usize {
                assert_ne!(value.get_raw(), 0);
                self.mark_object(value);
            }
        }
    }

    unsafe fn mark_continuation_table(&mut self) {
        let continuation_table = *self.mem.roots().continuation_table_address;
        if continuation_table.is_ptr() {
            assert_ne!(continuation_table.get_raw(), 0);
            self.mark_object(continuation_table);
        }
    }

    unsafe fn mark_object(&mut self, value: Value) {
        assert_ne!(value.get_raw(), 0);
        let object = value.as_obj();
        // The incremental GC must also have marked this object.
        assert!(object.is_marked());
        if !self.visited.contains(value) {
            self.visited.insert(self.mem, value);
            self.mark_stack.push(self.mem, value);
        }
    }

    unsafe fn mark_all_reachable(&mut self) {
        while let Some(object) = self.mark_stack.pop() {
            self.mark_fields(object.get_ptr() as *mut Obj);
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.mem.heap_base() as usize,
            |gc, field_address| {
                assert_ne!((*field_address).get_raw(), 0);
                gc.mark_object(*field_address);
            },
            |_, _, array| array.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_ptr());
        let pointer = object.get_ptr();
        assert!(pointer < self.mem.heap_pointer() as usize);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
    }
}

struct MemoryChecker<'a, M: Memory> {
    mem: &'a mut M,
    allow_marked_objects: bool,
}

impl<'a, M: Memory> MemoryChecker<'a, M> {
    /// Check whether all objects and pointers in the memory have plausible values.
    unsafe fn check_memory(&self) {
        self.check_static_roots();
        self.check_continuation_table();
        self.check_heap();
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.mem.roots().static_roots.as_array();
        for i in 0..root_array.len() {
            let mutbox = root_array.get(i).as_mutbox();
            assert!((mutbox as usize) < self.mem.heap_base() as usize);
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
                // skip null pointers used in text_iter
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
        assert_ne!(pointer as *mut Obj, null_mut());
        assert!(pointer < self.mem.heap_pointer() as usize);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        if !self.allow_marked_objects {
            assert!(!(pointer as *mut Obj).is_marked());
        }
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.mem.heap_base() as usize;
        while pointer < self.mem.heap_pointer() as usize {
            let object = Value::from_ptr(pointer as usize);
            let tag = object.tag();
            if tag >= TAG_FREE_BLOCK_MIN {
                let block = pointer as *mut FreeBlock;
                pointer += block.size().as_usize();
            } else {
                if object.tag() != TAG_ONE_WORD_FILLER {
                    self.check_object(object);
                }
                pointer += object_size(pointer as usize).to_bytes().as_usize();
            }
        }
    }
}
