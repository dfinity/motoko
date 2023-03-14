//! Incremental GC sanity checker.
//! Serves for verifying both the young and old generation collection.
//! Two checks:
//! * Mark completion: No unmarked reachable objects.
//! * Full-heap scan: Plausible objects and object ids (references).

use crate::{
    constants::WORD_SIZE,
    mem_utils::memzero,
    memory::{alloc_blob_internal, Memory},
    types::{
        block_size, has_object_header, Bytes, Obj, Tag, Value, NULL_OBJECT_ID, TAG_NULL, TAG_OBJECT,
    },
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
    let visited = SimpleMarkBitmap::new(mem);
    let mut checker = MarkCompletionChecker {
        mem,
        generation_start,
        mark_stack,
        visited,
    };
    checker.check_mark_completeness();
}

struct SimpleMarkBitmap {
    heap_base: usize,
    blob: Value,
}

const BITMAP_FRACTION: usize = (WORD_SIZE * u8::BITS) as usize;

impl SimpleMarkBitmap {
    /// Allocate new zero-sized bitmap.
    pub unsafe fn new<M: Memory>(mem: &mut M) -> SimpleMarkBitmap {
        assert!(mem.get_heap_base() <= mem.get_heap_pointer());
        let heap_size = mem.get_heap_pointer() - mem.get_heap_base();
        let bitmap_size = Bytes(((heap_size + BITMAP_FRACTION) / BITMAP_FRACTION) as u32);
        let blob = alloc_blob_internal(mem, bitmap_size);
        memzero(
            blob.as_blob_mut().payload_addr() as usize,
            bitmap_size.to_words(),
        );
        SimpleMarkBitmap {
            heap_base: mem.get_heap_base(),
            blob,
        }
    }

    unsafe fn word_index(&self, address: usize) -> usize {
        debug_assert!(address >= self.heap_base);
        let offset = address - self.heap_base;
        debug_assert_eq!(offset % WORD_SIZE as usize, 0);
        offset / (WORD_SIZE as usize)
    }

    unsafe fn get_byte(&self, index: usize) -> *mut u8 {
        debug_assert!(index < self.blob.as_blob().len().as_usize());
        let bitmap_base = self.blob.as_blob_mut().payload_addr();
        bitmap_base.add(index) as *mut u8
    }

    pub unsafe fn is_marked(&self, address: usize) -> bool {
        let word_index = self.word_index(address);
        let byte_index = word_index / u8::BITS as usize;
        let bit_index = word_index % u8::BITS as usize;
        let byte = self.get_byte(byte_index);
        (*byte >> bit_index) & 0b1 != 0
    }

    pub unsafe fn mark(&mut self, address: usize) {
        let word_index = self.word_index(address);
        let byte_index = word_index / u8::BITS as usize;
        let bit_index = word_index % u8::BITS as usize;
        let byte = self.get_byte(byte_index);
        *byte |= 0b1 << bit_index;
    }
}

struct MarkCompletionChecker<'a, M: Memory> {
    mem: &'a mut M,
    generation_start: usize,
    mark_stack: MarkStack,
    visited: SimpleMarkBitmap,
}

impl<'a, M: Memory> MarkCompletionChecker<'a, M> {
    unsafe fn check_mark_completeness(&mut self) {
        assert_eq!(self.mem.get_heap_base(), self.visited.heap_base);
        self.mark_stack.allocate(self.mem);
        self.check_roots();
        self.check_all_reachable();
        self.mark_stack.free();
        assert_eq!(self.mem.get_heap_base(), self.visited.heap_base);
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
        if !self.visited.is_marked(value.get_object_address()) {
            self.visited.mark(value.get_object_address());
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
/// Optionally, checks that all mark bits been cleared for objects above or equal a defined address,
/// which must be the case after the compact phase for the old generation and the non-promoted
/// remainder of the collected young generation.
/// Note: Not to be called during an unfinished compact phase, since garbage object have then
/// have dangling/invalid references (if referring to other garbage that has already been recycled).
pub unsafe fn check_heap<M: Memory>(mem: &mut M, forbid_marked_objects: Option<usize>) {
    let mut pointer = mem.get_heap_base();
    while pointer < mem.get_heap_pointer() {
        let tag = *(pointer as *const Tag);
        if has_object_header(tag) {
            let object = pointer as *mut Obj;
            if forbid_marked_objects.is_some() && pointer >= forbid_marked_objects.unwrap() {
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
