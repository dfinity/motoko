use core::ptr::null_mut;

use crate::{
    constants::WORD_SIZE,
    gc::common::{Limits, Roots},
    memory::Memory,
    types::{Obj, Value, TAG_ARRAY_SLICE_MIN},
    visitor::visit_pointer_fields,
};

use super::{
    array_slicing::slice_array,
    mark_stack::MarkStack,
    roots::visit_roots,
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
        let roots = self.roots.clone();
        let remembered_set = YOUNG_REMEMBERED_SET.as_ref().unwrap();
        visit_roots(
            roots,
            self.generation_base(),
            remembered_set,
            self,
            |gc, value| {
                gc.mark_object(value);
            },
        );
        // The remembered set is no longer needed and will be collected by this GC run.
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
