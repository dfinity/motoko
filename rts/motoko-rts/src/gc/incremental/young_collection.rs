//! Young generation garbage collection used by the incremental GC.
//!
//! Young generation collection runs:
//! * Always before a GC increment of the old generation, for simplifying incremental collection.
//! * After a certain amount of new allocations, i.e. when young generation has exceeded a threshold.
//!
//! This aims at fast reclamation of short-lived objects, to reduce GC latency, which is particularly
//! critical in an incremental GC. Young generation collection blocks the mutator, i.e. does not run
//! incrementally.
//!
//! The young generation collection requires an extra root set of old-to-young pointers. Those pointers
//! are caught by a write barrier and recorded in a remembered set (`YOUNG_REMEMBERED_SET`).
//!
//! The remembered set lives in the young generation and is freed during the young generation collection.
//!
//! The compaction phase can use the simple object movement enabled by the central object table provided
//! for the incremental GC.
//!
//! New allocation marking policy:
//! * New objects are allocated in the young generation and not marked (to allow fast reclamation).
//! * When young objects are promoted to the old generation, they are marked if and only if the
//!   incremental GC of the old generation is active (i.e. is in mark or compact phase).
//!
//! See the note in `old_collection.rs` for the rationale why a mark bit in the object header is used
//! instead of a mark bitmap.

use core::ptr::null_mut;

use crate::{
    constants::WORD_SIZE,
    gc::common::{Limits, Roots},
    mem_utils::memcpy_words,
    memory::Memory,
    types::{block_size, has_object_header, Obj, Tag, Value, Words, TAG_ARRAY_SLICE_MIN},
    visitor::visit_pointer_fields,
};

use super::{
    array_slicing::slice_array, mark_stack::MarkStack, roots::visit_roots,
    write_barrier::YOUNG_REMEMBERED_SET,
};

pub struct YoungCollection<'a, M: Memory> {
    mem: &'a mut M,
    limits: Limits,
    roots: Roots,
    mark_stack: MarkStack,
    // While incremental collection is active on the old generation,
    // promoted objects must be marked to survive the incremental GC run.
    mark_promoted_objects: bool,
}

impl<'a, M: Memory> YoungCollection<'a, M> {
    pub fn new(
        mem: &'a mut M,
        limits: Limits,
        roots: Roots,
        mark_promoted_objects: bool,
    ) -> YoungCollection<'a, M> {
        let mark_stack = MarkStack::new();
        YoungCollection {
            mem,
            limits,
            roots,
            mark_stack,
            mark_promoted_objects,
        }
    }

    fn generation_base(&self) -> usize {
        self.limits.last_free
    }

    fn generation_end(&self) -> usize {
        self.limits.free
    }

    pub unsafe fn run(&mut self) {
        self.mark_phase();
        self.compact_phase();
        // Do not immediately create the new remembered set as there may run
        // a potential subsequent GC increment of the old generation.
    }

    pub fn get_new_limits(&self) -> Limits {
        self.limits
    }

    unsafe fn mark_phase(&mut self) {
        self.mark_stack.allocate(self.mem);
        self.mark_roots();
        self.mark_all_reachable();
        self.mark_stack.free();
    }

    unsafe fn mark_roots(&mut self) {
        let remembered_set = YOUNG_REMEMBERED_SET.take().unwrap();
        visit_roots(
            self.roots,
            self.generation_base(),
            Some(&remembered_set),
            self,
            |gc, value| {
                gc.mark_object(value);
            },
        );
    }

    unsafe fn mark_object(&mut self, value: Value) {
        let object = value.get_object_address() as *mut Obj;
        assert!(object as usize >= self.generation_base());
        assert_eq!(object as u32 % WORD_SIZE, 0);
        if object.is_marked() {
            return;
        }
        object.mark();
        self.mark_stack.push(self.mem, object);
    }

    unsafe fn mark_all_reachable(&mut self) {
        loop {
            let object = self.mark_stack.pop();
            if object == null_mut() {
                break;
            }
            self.mark_fields(object);
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.generation_base(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, _, array| {
                let length = slice_array(array);
                if (*array).header.tag >= TAG_ARRAY_SLICE_MIN {
                    gc.mark_stack.push(gc.mem, array as *mut Obj);
                }
                length
            },
        );
    }

    unsafe fn compact_phase(&mut self) {
        // Need to visit all objects in the generation, since mark bits may need to be
        // cleared and/or garbage object ids must be freed.
        assert!(YOUNG_REMEMBERED_SET.is_none()); // No longer valid as it will be collected.
        let mut free = self.generation_base();
        let mut address = free;
        while address < self.generation_end() {
            let block = address as *mut Tag;
            let size = block_size(block as usize);
            if has_object_header(*block) {
                self.compact_object(block as *mut Obj, size, &mut free);
            }
            address += size.to_bytes().as_usize();
        }
        self.limits.free = free;
        self.limits.last_free = free;
    }

    unsafe fn compact_object(&self, object: *mut Obj, size: Words<u32>, free: &'a mut usize) {
        let object_id = object.object_id();
        if object.is_marked() {
            if !self.mark_promoted_objects {
                // If the incremental GC is not active, the object mark must be cleared.
                // Otherwise, the mark bit must stay according to the allocation policy
                // during an active incremental marking or compaction phase.
                object.unmark();
            }
            let old_address = object as usize;
            let new_address = *free;
            if new_address != old_address {
                memcpy_words(new_address, old_address, size);
                object_id.set_new_address(new_address);
            }
            *free += size.to_bytes().as_usize();
        } else {
            // Free the id of a garbage object in the object table.
            object_id.free_object_id();
        }
    }
}
