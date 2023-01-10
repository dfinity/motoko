//! Incremental GC sanity checker
#![allow(dead_code)]

use core::ptr::null_mut;

use crate::gc::generational::remembered_set::RememberedSet;
use crate::memory::Memory;
use crate::visitor::visit_pointer_fields;
use crate::{types::*, visitor::pointer_to_dynamic_heap};

use super::mark_stack::MarkStack;

#[cfg(feature = "ic")]
pub unsafe fn check_mark_completeness<M: Memory>(mem: &mut M) {
    let heap = get_heap();
    let mark_stack = MarkStack::new(mem);
    let visited = RememberedSet::new(mem);
    let mut checker = MarkPhaseChecker {
        mem,
        heap,
        mark_stack,
        visited,
    };
    checker.check_mark_completeness();
}

#[cfg(feature = "ic")]
pub unsafe fn check_memory(allow_marked_objects: bool) {
    let heap = get_heap();
    let checker = MemoryChecker {
        heap,
        allow_marked_objects,
    };
    checker.check_memory();
}

#[cfg(feature = "ic")]
unsafe fn get_heap() -> Heap {
    use crate::memory::ic;
    let limits = Limits {
        base: ic::get_aligned_heap_base() as usize,
        free: ic::HP as usize,
    };
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *crate::continuation_table::continuation_table_loc(),
    };
    Heap { limits, roots }
}

struct Limits {
    pub base: usize,
    pub free: usize,
}

struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
}

struct Heap {
    limits: Limits,
    roots: Roots,
}

struct MarkPhaseChecker<'a, M: Memory> {
    mem: &'a mut M,
    heap: Heap,
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
        let root_array = self.heap.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let mutbox = root_array.get(i).as_mutbox();
            let value = (*mutbox).field;
            if value.is_ptr() && value.get_ptr() >= self.heap.limits.base {
                assert_ne!(value.get_raw(), 0);
                self.mark_object(value);
            }
        }
    }

    unsafe fn mark_continuation_table(&mut self) {
        if self.heap.roots.continuation_table.is_ptr() {
            assert_ne!(self.heap.roots.continuation_table.get_raw(), 0);
            self.mark_object(self.heap.roots.continuation_table);
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
            self.heap.limits.base,
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
        assert!(pointer < self.heap.limits.free);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
    }
}

struct MemoryChecker {
    heap: Heap,
    allow_marked_objects: bool,
}

impl MemoryChecker {
    /// Check whether all objects and pointers in the memory have plausible values.
    unsafe fn check_memory(&self) {
        self.check_static_roots();
        self.check_continuation_table();
        self.check_heap();
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.heap.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let mutbox = root_array.get(i).as_mutbox();
            assert!((mutbox as usize) < self.heap.limits.base);
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.heap.limits.base) {
                let object = *field_addr;
                self.check_object(object);
            }
        }
    }

    unsafe fn check_continuation_table(&self) {
        if self.heap.roots.continuation_table.is_ptr() {
            self.check_object(self.heap.roots.continuation_table);
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
        assert!(pointer < self.heap.limits.free);
        let tag = object.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        if !self.allow_marked_objects {
            assert!(!(pointer as *mut Obj).is_marked());
        }
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.heap.limits.base;
        while pointer < self.heap.limits.free {
            let object = Value::from_ptr(pointer as usize);
            if object.tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            pointer += block_size(pointer as usize).to_bytes().as_usize();
        }
    }
}
