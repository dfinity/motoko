use crate::gc::functions::{
    mark_stack::{MarkStack, StackEntry},
    visited_set::{VisitedSet, VisitedValue},
};
use motoko_rts_macros::ic_mem_fn;

use crate::{
    memory::Memory,
    persistence::stable_functions::is_flexible_function_id,
    types::{Value, NULL_POINTER, TAG_MUTBOX, TAG_NEW_CLOSURE, TAG_OBJECT, TAG_SOME},
};

use super::{resolve_stable_function_id, FunctionId, PersistentVirtualTable, VirtualTableEntry};

// Note: For a type-directed selective visiting, we need to revisit the
// same object if it occurs with a different static type.

extern "C" {
    fn moc_visit_stable_functions(object: Value, type_id: u64);
}

pub struct FunctionGC {
    visited_set: VisitedSet,
    mark_stack: MarkStack,
    virtual_table: *mut PersistentVirtualTable,
}

impl FunctionGC {
    unsafe fn new<M: Memory>(
        mem: &mut M,
        virtual_table: *mut PersistentVirtualTable,
    ) -> FunctionGC {
        let mark_set = VisitedSet::new(mem);
        let mark_stack = MarkStack::new(mem);
        FunctionGC {
            visited_set: mark_set,
            mark_stack,
            virtual_table,
        }
    }

    unsafe fn run(&mut self) {
        self.clear_mark_bits();
        loop {
            match self.mark_stack.pop() {
                None => return,
                Some(StackEntry { object, type_id }) => {
                    debug_assert_ne!(object, NULL_POINTER);
                    if object.tag() == TAG_SOME {
                        // skip null boxes, not visited
                    } else if object.tag() == TAG_NEW_CLOSURE {
                        self.visit_stable_closure(object);
                    } else {
                        // Specialized field visitor, as optimization.
                        moc_visit_stable_functions(object, type_id);
                    }
                }
            }
        }
    }

    unsafe fn visit_stable_closure(&mut self, object: Value) {
        let closure = object.as_closure();
        let function_id = (*closure).funid;
        assert!(!is_flexible_function_id(function_id));
        self.mark_function(function_id);
        let closure_type_id = self.get_closure_type_id(function_id);
        moc_visit_stable_functions(object, closure_type_id);
    }

    unsafe fn get_function_entry(&mut self, function_id: FunctionId) -> *mut VirtualTableEntry {
        self.virtual_table
            .get(resolve_stable_function_id(function_id))
    }

    unsafe fn mark_function(&mut self, function_id: FunctionId) {
        let entry = self.get_function_entry(function_id);
        (*entry).marked = true;
    }

    unsafe fn get_closure_type_id(&mut self, function_id: FunctionId) -> u64 {
        let entry = self.get_function_entry(function_id);
        (*entry).gc_type_id
    }

    unsafe fn clear_mark_bits(&mut self) {
        for index in 0..self.virtual_table.length() {
            let entry = self.virtual_table.get(index);
            (*entry).marked = false;
        }
    }
}

static mut COLLECTOR_STATE: Option<FunctionGC> = None;

static mut SKIP_FUNCTION_GC: bool = false;

#[no_mangle]
pub unsafe fn skip_persistent_function_gc() {
    SKIP_FUNCTION_GC = true;
}

unsafe fn skip_garbage_collection(virtual_table: *mut PersistentVirtualTable) {
    for index in 0..virtual_table.length() {
        let entry = virtual_table.get(index);
        (*entry).marked = true;
    }
}

// Garbage collect the stable functions in the old version on an upgrade.
pub unsafe fn garbage_collect_functions<M: Memory>(
    mem: &mut M,
    virtual_table: *mut PersistentVirtualTable,
    old_actor: Value,
) {
    if SKIP_FUNCTION_GC {
        skip_garbage_collection(virtual_table);
        return;
    }
    assert_eq!(old_actor.tag(), TAG_OBJECT);
    COLLECTOR_STATE = Some(FunctionGC::new(mem, virtual_table));
    const ACTOR_TYPE_ID: u64 = 0;
    stable_functions_gc_visit(mem, old_actor, ACTOR_TYPE_ID);
    COLLECTOR_STATE.as_mut().unwrap().run();
    COLLECTOR_STATE = None;
}

#[ic_mem_fn]
unsafe fn stable_functions_gc_visit<M: Memory>(mem: &mut M, object: Value, type_id: u64) {
    let state = COLLECTOR_STATE.as_mut().unwrap();
    // TODO: also remember type for the marked object. Revisit the same object if
    // it has a different static type.
    let item = VisitedValue { object, type_id };
    if object != NULL_POINTER && !state.visited_set.contains(item) {
        state.visited_set.insert(mem, item);
        state.mark_stack.push(mem, StackEntry { object, type_id });
    }
}

#[no_mangle]
unsafe fn unwrap_closure_field(value: Value) -> Value {
    if value != NULL_POINTER && value.tag() == TAG_MUTBOX {
        let mutbox = value.as_mutbox();
        (*mutbox).field
    } else {
        value
    }
}
