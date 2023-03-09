//! Incremental GC sanity checker.
use core::ptr::null_mut;

use crate::{
    constants::WORD_SIZE,
    memory::Memory,
    remembered_set::RememberedSet,
    types::{block_size, has_object_header, Obj, Tag, Value, NULL_OBJECT_ID, TAG_NULL, TAG_OBJECT},
    visitor::visit_pointer_fields,
};

use super::{mark_stack::MarkStack, roots::visit_roots};

pub unsafe fn check_memory<M: Memory>(mem: &mut M, mode: CheckerMode) {
    let mark_stack = MarkStack::new();
    let visited = RememberedSet::new(mem);
    let mut checker = MemoryChecker {
        mode,
        mem,
        mark_stack,
        visited,
    };
    checker.run();
}

/// Sanity check modes.
#[derive(PartialEq)]
pub enum CheckerMode {
    MarkCompletion,    // Check mark phase completion.
    CompactCompletion, // Check update phase completion.
}

struct MemoryChecker<'a, M: Memory> {
    mode: CheckerMode,
    mem: &'a mut M,
    mark_stack: MarkStack,
    visited: RememberedSet,
}

impl<'a, M: Memory> MemoryChecker<'a, M> {
    // Check whether all reachable objects and object ids in the memory have a plausible state.
    // Various check modes:
    // * MarkCompletion:
    ///  Check that the set of marked objects by the incremental GC is the same set or a superset
    ///  of the objects being marked by a conventional stop-the-world mark phase. The incremental
    ///  GC may mark more objects due to concurrent allocations and concurrent object id writes.
    // * CompactCompletion:
    //   Check that the compact phase left the heap in a consistent state, with valid object id
    ///  entries in the object table, valid references, and all mark bits been cleared.
    unsafe fn run(&mut self) {
        if self.mode == CheckerMode::MarkCompletion {
            self.check_mark_completeness();
        }
        self.check_heap();
    }

    unsafe fn check_mark_completeness(&mut self) {
        // This may extend the heap, no using this at the end of the compaction phase.
        self.mark_stack.allocate(self.mem);
        self.check_roots();
        self.check_all_reachable();
        self.mark_stack.free();
    }

    unsafe fn check_roots(&mut self) {
        visit_roots(
            self.mem.get_roots(),
            self.mem.get_heap_base(),
            None,
            self,
            |gc, field| {
                gc.check_reachable_object(field);
            },
        );
    }

    unsafe fn check_reachable_object(&mut self, value: Value) {
        self.check_object_header(value);
        let object = value.get_object_address() as *mut Obj;
        if let CheckerMode::MarkCompletion = self.mode {
            // The incremental GC must have marked this reachable object.
            // Mark object returns false if it has been previously marked.
            assert!(object.is_marked());
        }
        if !self.visited.contains(value) {
            self.visited.insert(self.mem, value);
            self.mark_stack.push(self.mem, object);
        }
    }

    unsafe fn check_all_reachable(&mut self) {
        loop {
            let object = self.mark_stack.pop();
            if object == null_mut() {
                break;
            }
            self.check_reachable_fields(object);
        }
    }

    unsafe fn check_reachable_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            0,
            |gc, field_address| {
                let value = *field_address;
                // Ignore null pointers used in `text_iter`.
                if value != NULL_OBJECT_ID {
                    if value.get_object_address() >= gc.mem.get_heap_base() {
                        gc.check_reachable_object(value);
                    } else {
                        gc.check_object_header(value);
                    }
                }
            },
            |_, _, array| array.len(),
        );
    }

    unsafe fn check_object_header(&self, value: Value) {
        let tag = value.tag();
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        let object = value.get_object_address() as *mut Obj;
        assert!(object.object_id() == value);
        assert!((object as usize) < self.mem.get_heap_pointer());
        assert_eq!(object as u32 % WORD_SIZE, 0);
    }

    unsafe fn check_valid_references(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            0,
            |gc, field_address| {
                let value = *field_address;
                // Ignore null pointers used in `text_iter`.
                if value != NULL_OBJECT_ID {
                    gc.check_object_header(value);
                }
            },
            |_, _, array| array.len(),
        );
    }

    unsafe fn check_heap(&mut self) {
        let mut pointer = self.mem.get_heap_base();
        while pointer < self.mem.get_heap_pointer() {
            let tag = *(pointer as *const Tag);
            if has_object_header(tag) {
                let object = pointer as *mut Obj;
                if self.mode == CheckerMode::CompactCompletion {
                    assert!(!object.is_marked());
                }
                let value = object.object_id();
                assert_eq!(value.get_object_address(), object as usize);
                self.check_object_header(value);
                self.check_valid_references(object);
            }
            pointer += block_size(pointer as usize).to_bytes().as_usize();
        }
    }
}
