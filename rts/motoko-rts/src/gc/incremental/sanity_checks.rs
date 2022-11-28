//! Incremental GC sanity checker
#![allow(dead_code)]

use crate::constants::WORD_SIZE;
use crate::gc::mark_compact::bitmap::{alloc_bitmap, free_bitmap, get_bit, set_bit};
use crate::gc::mark_compact::mark_stack::{
    alloc_mark_stack, free_mark_stack, pop_mark_stack, push_mark_stack,
};
use crate::memory::Memory;
use crate::visitor::visit_pointer_fields;
use crate::{types::*, visitor::pointer_to_dynamic_heap};

use super::free_list::FreeBlock;

#[cfg(feature = "ic")]
pub unsafe fn check_mark_completeness<M: Memory>(mem: &mut M) {
    let heap = get_heap();
    let mut checker = MarkPhaseChecker { mem, heap };
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
}

impl<'a, M: Memory> MarkPhaseChecker<'a, M> {
    // Check that the set of marked objects by the incremental GC is the same set or a
    // subset of the objects being marked by a conventional stop-the-world mark phase.
    // The incremental GC may mark more objects due to concurrent allocations and
    // concurrent pointer modifications.
    unsafe fn check_mark_completeness(&mut self) {
        self.allocate_mark_structures();
        self.mark_roots();
        self.mark_all_reachable();
        self.free_mark_structures();
    }

    unsafe fn allocate_mark_structures(&mut self) {
        let memory_size = Bytes(self.heap.limits.free as u32 - self.heap.limits.base as u32);
        let heap_prefix_words = self.heap.limits.base as u32 / WORD_SIZE;
        alloc_bitmap(self.mem, memory_size, heap_prefix_words);
        alloc_mark_stack(self.mem);
    }

    unsafe fn free_mark_structures(&self) {
        free_mark_stack();
        free_bitmap();
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
                self.mark_object(value);
            }
        }
    }

    unsafe fn mark_continuation_table(&mut self) {
        if self.heap.roots.continuation_table.is_ptr() {
            self.mark_object(self.heap.roots.continuation_table);
        }
    }

    unsafe fn mark_object(&mut self, value: Value) {
        let object = value.as_obj();
        // The incremental GC must also have marked this object.
        assert!(object.is_marked());
        let object_index = object as u32 / WORD_SIZE;
        if !get_bit(object_index) {
            set_bit(object_index);
            push_mark_stack(self.mem, object as usize, object.tag());
        }
    }

    unsafe fn mark_all_reachable(&mut self) {
        while let Some((object, _)) = pop_mark_stack() {
            self.mark_fields(object as *mut Obj);
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.heap.limits.base,
            |gc, field_address| {
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
                (&self).check_object_header(*field_address);
            },
            |_, _, arr| arr.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_ptr());
        let pointer = object.get_ptr();
        assert!(pointer < self.heap.limits.free);
        let tag = object.tag();

        if !(tag >= TAG_OBJECT && tag <= TAG_NULL) {
            println!(100, "ERROR {pointer:#x} {tag} {tag:#x}");
        }

        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        if !self.allow_marked_objects {
            assert!(!(pointer as *mut Obj).is_marked());
        }
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.heap.limits.base;
        while pointer < self.heap.limits.free {
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
