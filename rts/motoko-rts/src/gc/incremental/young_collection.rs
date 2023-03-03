use core::ptr::null_mut;

use crate::{
    constants::WORD_SIZE,
    gc::common::{Limits, Roots},
    memory::Memory,
    types::{MutBox, Obj, Value, TAG_ARRAY_SLICE_MIN, TAG_MUTBOX},
    visitor::{pointer_to_dynamic_heap, visit_pointer_fields},
};

use super::{
    array_slicing::slice_array,
    mark_stack::MarkStack,
    write_barrier::{reset_young_remembered_set, YOUNG_REMEMBERED_SET},
};

pub struct YoungCollection<'a, M: Memory> {
    mem: &'a mut M,
    limits: Limits,
    roots: Roots,
    mark_stack: MarkStack,
}

impl<'a, M: Memory> YoungCollection<'a, M> {
    pub fn new(mem: &'a mut M, limits: Limits, roots: Roots) -> YoungCollection<'a, M> {
        let mark_stack = MarkStack::new();
        YoungCollection {
            mem,
            limits,
            roots,
            mark_stack,
        }
    }

    fn generation_base(&self) -> usize {
        self.limits.last_free
    }

    pub unsafe fn run(&mut self) -> Limits {
        self.mark_phase();
        self.compact_phase();
        reset_young_remembered_set(self.mem, self.limits.free);
        self.limits.clone()
    }

    unsafe fn mark_phase(&mut self) {
        self.mark_stack.allocate(self.mem);
        self.mark_root_set();
        self.mark_all_reachable();
        self.mark_stack.free();
    }

    unsafe fn mark_root_set(&mut self) {
        self.mark_static_roots();
        self.mark_continuation_table();
        self.mark_additional_young_root_set();
    }

    unsafe fn mark_static_roots(&mut self) {
        let root_array = self.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            assert_eq!(object.tag(), TAG_MUTBOX);
            assert!((object as usize) < self.limits.base);
            self.mark_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn mark_root_mutbox_fields(&mut self, mutbox: *mut MutBox) {
        let field_address = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field_address, self.generation_base()) {
            self.mark_object(*field_address);
        }
    }

    unsafe fn mark_continuation_table(&mut self) {
        let continuation_table = *self.roots.continuation_table_ptr_loc;
        if continuation_table.is_object_id()
            && continuation_table.get_object_address() >= self.generation_base()
        {
            self.mark_object(continuation_table);
        }
    }

    unsafe fn mark_additional_young_root_set(&mut self) {
        let mut iterator = YOUNG_REMEMBERED_SET.as_ref().unwrap().iterate();
        while iterator.has_next() {
            let location = iterator.current().get_raw() as *mut Value;
            let value = *location;
            // Check whether the location still refers to  a young object as this may have
            // changed due to subsequent writes to that location after the barrier recording.
            if value.points_to_or_beyond(self.generation_base()) {
                self.mark_object(value);
            }
            iterator.next();
        }
        // This remembered set is discarded in this GC run.
        // A new one is created for the next GC increment/run.
        YOUNG_REMEMBERED_SET = None;
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

    pub unsafe fn compact_phase(&mut self) {
        println!(100, "INCREMENTAL YOUNG GENERATION COMPACT PHASE");
    }
}
