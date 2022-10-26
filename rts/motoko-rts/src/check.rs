use crate::{
    memory::Memory,
    types::{Value, TAG_NULL, TAG_OBJECT, TAG_MUTBOX, MutBox, TAG_ONE_WORD_FILLER, object_size, Obj}, visitor::{pointer_to_dynamic_heap, visit_pointer_fields},
};
use motoko_rts_macros::ic_mem_fn;

#[ic_mem_fn(ic_only)]
unsafe fn follow_forwarding_pointer<M: Memory>(_mem: &mut M, value: Value) -> Value {
    const TRUE_VALUE: u32 = 1;
    if !(value.is_ptr() && value.get_raw() != TRUE_VALUE) {
        println!(100, "ERROR PTR {:#x}", value.get_raw());
    }
    assert!(value.is_ptr() && value.get_raw() != TRUE_VALUE);
    if !value.forward().is_ptr() {
        println!(100, "ERROR FWD {:#x} {:#x}", value.get_raw(), value.forward().get_raw());
    }
    assert!(value.forward().is_ptr());
    assert_eq!(value.forward().get_ptr(), value.get_ptr());
    assert!(value.tag() >= TAG_OBJECT && value.tag() <= TAG_NULL);
    // use crate::memory::ic;
    // let heap_base = if ic::ALIGN {
    //     ic::get_aligned_heap_base()
    // } else {
    //     ic::get_heap_base()
    // };
    //check_memory(heap_base as usize, ic::HP as usize, ic::get_static_roots(), crate::continuation_table::continuation_table_loc());
    value.forward()
}

pub struct MemoryChecker {
    heap_base: usize,
    heap_end: usize,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value
}

pub unsafe fn check_memory(
    heap_base: usize,
    heap_end: usize,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value) {
    let checker = MemoryChecker { 
        heap_base,
        heap_end,
        static_roots,
        continuation_table_ptr_loc
    };
    checker.check_memory();
}

impl MemoryChecker {
    unsafe fn check_memory(&self) {
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
        visit_pointer_fields(
            &mut (),
            object.as_obj(),
            object.tag(),
            0,
            |_, field_address| {
                if Self::is_ptr(*field_address) {
                    (&self).check_object_header(*field_address);
                }
            },
            |_, _, arr| arr.len(),
        );
    }

    unsafe fn check_object_header(&self, object: Value) {
        assert!(Self::is_ptr(object));
        let pointer = object.get_ptr();
        assert!(pointer < self.heap_end);
        let tag = object.tag();
        const COERCION_FAILURE: u32 = 0xfffffffe;
        assert!(tag >= TAG_OBJECT && tag <= TAG_NULL || tag == COERCION_FAILURE);
        let forward = object.forward();
        assert_eq!(forward.get_raw(), object.get_raw());
    }

    unsafe fn check_heap(&self) {
        let mut pointer = self.heap_base;
        while pointer < self.heap_end {
            let object = Value::from_ptr(pointer as usize);
            if (object.get_ptr() as *mut Obj).tag() != TAG_ONE_WORD_FILLER {
                self.check_object(object);
            }
            pointer += object_size(pointer as usize).to_bytes().as_usize();
        }
    }

    unsafe fn is_ptr(value: Value) -> bool {
        const TRUE_VALUE: u32 = 1;
        value.is_ptr() && value.get_raw() != TRUE_VALUE
    }
}