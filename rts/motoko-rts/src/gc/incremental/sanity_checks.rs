//! Incremental GC sanity checker.
//! Serves for verifying both the young and old generation collection.
//! Two checks:
//! * Mark completion: No unmarked reachable objects.
//! * Full-heap scan: Plausible objects and object ids (references).

use crate::{
    constants::WORD_SIZE,
    memory::Memory,
    remembered_set::RememberedSet,
    types::{block_size, has_object_header, Obj, Tag, Value, NULL_OBJECT_ID, TAG_NULL, TAG_OBJECT},
    visitor::visit_pointer_fields,
};

use super::{mark_stack::MarkStack, roots::visit_roots};

/// Sanity check at the end of the marking phase for the old or young generation collection.
/// Check that the set of marked objects by the incremental GC of the old generation is the
/// same set or a superset of the objects being marked by a conventional stop-the-world mark
/// algorithm. The incremental GC may mark more objects due to concurrent promotions from young
/// generation and concurrent object id writes (changing object references while marking).
pub unsafe fn check_mark_completion<M: Memory>(mem: &mut M, generation_start: usize) {
    let mark_stack = MarkStack::new();
    let visited = RememberedSet::new(mem);
    let mut checker = MarkCompletionChecker {
        mem,
        generation_start,
        mark_stack,
        visited,
    };
    checker.check_mark_completeness();
}

struct MarkCompletionChecker<'a, M: Memory> {
    mem: &'a mut M,
    generation_start: usize,
    mark_stack: MarkStack,
    visited: RememberedSet,
}

impl<'a, M: Memory> MarkCompletionChecker<'a, M> {
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
        check_object_header(self.mem, value);
        let object = value.get_object_address() as *mut Obj;
        if object as usize >= self.generation_start {
            // The GC must have marked this reachable object in the generation
            // that is subject to garbage collection.
            assert!(object.is_marked());
        }
        if !self.visited.contains(value) {
            self.visited.insert(self.mem, value);
            self.mark_stack.push(self.mem, value);
        }
    }

    unsafe fn check_all_reachable(&mut self) {
        loop {
            let value = self.mark_stack.pop();
            if value == NULL_OBJECT_ID {
                break;
            }
            let object = value.get_object_address() as *mut Obj;
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
                        check_object_header(gc.mem, value);
                    }
                }
            },
            |_, _, array| array.len(),
        );
    }
}

/// Full-heap sanity check that can be used for both young and old generation collection.
/// Scans the entire heap and checks that all objects have valid object ids, self-referencing via
/// the object table and that all object ids in references point to valid objects.
/// Optionally, checks that all mark bits been cleared for objects above or equal the generation
/// start address, which must be the case after the compact phase.
/// Note: Not to be called during an unfinished compact phase, since garbage object have then
/// have dangling/invalid references (if referring to other garbage that has already been recycled).
pub unsafe fn check_heap<M: Memory>(
    mem: &mut M,
    generation_start: usize,
    allow_marked_objects: bool,
) {
    let mut pointer = mem.get_heap_base();
    while pointer < mem.get_heap_pointer() {
        let tag = *(pointer as *const Tag);
        if has_object_header(tag) {
            let object = pointer as *mut Obj;
            if !allow_marked_objects && pointer >= generation_start {
                assert!(!object.is_marked());
            }
            let value = object.object_id();
            assert_eq!(value.get_object_address(), object as usize);
            check_object_header(mem, value);
            check_valid_references(mem, object);
        }
        pointer += block_size(pointer as usize).to_bytes().as_usize();
    }
}

unsafe fn check_object_header<M: Memory>(mem: &mut M, value: Value) {
    let tag = value.tag();
    assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
    let object = value.get_object_address() as *mut Obj;
    assert!(object.object_id() == value);
    assert!((object as usize) < mem.get_heap_pointer());
    assert_eq!(object as u32 % WORD_SIZE, 0);
}

unsafe fn check_valid_references<M: Memory>(mem: &mut M, object: *mut Obj) {
    visit_pointer_fields(
        mem,
        object,
        object.tag(),
        0,
        |mem, field_address| {
            let value = *field_address;
            // Ignore null pointers used in `text_iter`.
            if value != NULL_OBJECT_ID {
                check_object_header(mem, value);
            }
        },
        |_, _, array| array.len(),
    );
}
