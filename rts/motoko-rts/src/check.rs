use crate::{
    types::{Value, TAG_NULL, TAG_OBJECT, TAG_MUTBOX, MutBox, TAG_ONE_WORD_FILLER, object_size, Obj}, visitor::{pointer_to_dynamic_heap, visit_pointer_fields}, memory::Memory, mem_utils::{memcpy_bytes, memzero},
};
use motoko_rts_macros::ic_mem_fn;

static mut _SERIALIZING: bool = false;
pub static mut ARTIFICIAL_FORWARDING: bool = false;

#[ic_mem_fn(ic_only)]
unsafe fn set_serialization_status<M: Memory>(_mem: &mut M, active: bool) {
    _SERIALIZING = active;
}

#[ic_mem_fn(ic_only)]
unsafe fn check_forwarding_pointer<M: Memory>(_mem: &mut M, value: Value) -> Value {
    const TRUE_VALUE: u32 = 1;
    assert!(value.is_ptr() && value.get_raw() != TRUE_VALUE);
    value.check_forwarding_pointer();
    if !_SERIALIZING {
        assert!(value.tag() >= TAG_OBJECT && value.tag() <= TAG_NULL);
    }
    value.forward()
}

#[ic_mem_fn(ic_only)]
unsafe fn set_artificial_forwarding<M: Memory>(_mem: &mut M, active: bool) {
    ARTIFICIAL_FORWARDING = active;
}

const INVALID_TAG_BITMASK: u32 = 0x8000_0000;

#[ic_mem_fn]
/// Forward the object to a new copy and clear the content in the source object.
pub unsafe fn create_artificial_forward<M: Memory>(mem: &mut M, source: Value) {
    if !ARTIFICIAL_FORWARDING {
        return;
    }
    assert!(source.is_ptr());
    let size = object_size(source.get_ptr() as usize);
    let target = mem.alloc_words(size);
    memcpy_bytes(target.get_ptr() as usize, source.get_ptr() as usize, size.to_bytes());
    let target_object = target.get_ptr() as *mut Obj;
    (*target_object).forward = target;
    let source_object = source.get_ptr() as *mut Obj;
    assert!(source.forward() == source);
    assert_eq!(source.tag(), target.tag());
    assert_eq!(size.as_usize(), object_size(target_object as usize).as_usize());
    memzero(source_object as usize, size);
    assert!(size.to_bytes().as_u32() < INVALID_TAG_BITMASK);
    (*source_object).tag = size.to_bytes().as_u32() | INVALID_TAG_BITMASK; // encode length in invalid tag
    (*source_object).forward = target;
}

pub struct MemoryChecker {
    heap_base: usize,
    heap_end: usize,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value
}

#[ic_mem_fn(ic_only)]
pub unsafe fn check_memory<M: crate::memory::Memory>(_mem: &mut M) {
    use crate::memory::ic;
    let heap_base = if ic::ALIGN {
        ic::get_aligned_heap_base()
    } else {
        ic::get_heap_base()
    };
    let checker = MemoryChecker { 
        heap_base: heap_base as usize,
        heap_end: ic::HP as usize,
        static_roots: ic::get_static_roots(),
        continuation_table_ptr_loc: crate::continuation_table::continuation_table_loc()
    };
    checker.check_memory();
}

impl MemoryChecker {
    pub unsafe fn check_memory(&self) {
        // println!(100, "Memory check starts...");
        // println!(100, " Checking static roots...");
        self.check_static_roots();
        if (*self.continuation_table_ptr_loc).is_ptr() {
            // println!(100, " Checking continuation table...");
            self.check_object(*self.continuation_table_ptr_loc);
        }
        // println!(100, " Checking heap...");
        self.check_heap();
        // println!(100, "Memory check stops...");
    }

    unsafe fn check_static_roots(&self) {
        let root_array = self.static_roots.as_array();
        for i in 0..root_array.len() {
            let obj = root_array.get(i).as_obj();
            assert_eq!(obj.tag(), TAG_MUTBOX);
            assert!((obj as usize) < self.heap_base);
            let mutbox = obj as *mut MutBox;
            let field_addr = &mut (*mutbox).field;
            if pointer_to_dynamic_heap(field_addr, self.heap_base as usize) {
                let object = *field_addr;
                self.check_object(object);
            }
        }
    }

    unsafe fn check_object(&self, object: Value) {
        self.check_object_header(object);
        let tag = (object.get_ptr() as *mut Obj).tag();
        if tag & INVALID_TAG_BITMASK == 0 {
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
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(object.is_ptr());
        let pointer = object.get_ptr();
        assert!(pointer < self.heap_end);
        object.check_forwarding_pointer();
        if object.forward().get_ptr() != object.get_ptr() {
            self.check_object(object.forward());
        } else {
            let tag = object.tag();
            assert!(tag >= TAG_OBJECT && tag <= TAG_NULL);
        }
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.heap_base;
        while pointer < self.heap_end {
            let object = Value::from_ptr(pointer as usize);
            if (object.get_ptr() as *mut Obj).tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            pointer += Self::object_size(object);
        }
    }

    unsafe fn object_size(object: Value) -> usize {
        let tag = (object.get_ptr() as *mut Obj).tag();
        if tag & INVALID_TAG_BITMASK != 0 {
            (tag & !INVALID_TAG_BITMASK) as usize
        } else {
            object_size(object.get_ptr() as usize).to_bytes().as_usize()
        }
    }
}