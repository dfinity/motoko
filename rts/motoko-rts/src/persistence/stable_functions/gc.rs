use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::remembered_set::RememberedSet,
    memory::Memory,
    persistence::stable_functions::is_flexible_function_id,
    types::{Value, NULL_POINTER, TAG_CLOSURE, TAG_OBJECT, TAG_SOME},
    visitor::enhanced::visit_pointer_fields,
};

use super::{
    mark_stack::{MarkStack, StackEntry},
    resolve_stable_function_id, FunctionId, PersistentVirtualTable,
};

// Note: For a type-directed selective visiting, we need to revisit the
// same object if it occurs with a different static type.

// Currently fields in closure (captures) are not yet discovered in a type-directed way.
// This sentinel denotes that there is no static type known and the generic visitor is to be invoked.
// TODO: Optimization: Use expected closure types to select a compiler-generated specialized visitor.
const UNKNOWN_TYPE_ID: u64 = u64::MAX;

extern "C" {
    fn moc_visit_stable_functions(object: Value, type_id: u64);
}

pub struct FunctionGC {
    mark_set: RememberedSet,
    mark_stack: MarkStack,
    virtual_table: *mut PersistentVirtualTable,
}

impl FunctionGC {
    unsafe fn new<M: Memory>(
        mem: &mut M,
        virtual_table: *mut PersistentVirtualTable,
    ) -> FunctionGC {
        let mark_set = RememberedSet::new(mem);
        let mark_stack = MarkStack::new(mem);
        FunctionGC {
            mark_set,
            mark_stack,
            virtual_table,
        }
    }

    unsafe fn run<M: Memory>(&mut self, mem: &mut M) {
        self.clear_mark_bits();
        loop {
            match self.mark_stack.pop() {
                None => return,
                Some(StackEntry { object, type_id }) => {
                    debug_assert_ne!(object, NULL_POINTER);
                    if object.tag() == TAG_SOME {
                        // skip null boxes, not visited
                    } else if object.tag() == TAG_CLOSURE {
                        self.visit_stable_closure(mem, object);
                    } else if type_id == UNKNOWN_TYPE_ID {
                        self.generic_visit(mem, object);
                    } else {
                        // Specialized field visitor, as optimization.
                        moc_visit_stable_functions(object, type_id);
                    }
                }
            }
        }
    }

    unsafe fn generic_visit<M: Memory>(&mut self, mem: &mut M, object: Value) {
        visit_pointer_fields(
            mem,
            object.as_obj(),
            object.tag(),
            |mem, field| {
                stable_functions_gc_visit(mem, *field, UNKNOWN_TYPE_ID);
            },
            |_, slice_start, arr| {
                assert!(slice_start == 0);
                arr.len()
            },
        );
    }

    unsafe fn visit_stable_closure<M: Memory>(&mut self, mem: &mut M, object: Value) {
        let closure = object.as_closure();
        let function_id = (*closure).funid;
        assert!(!is_flexible_function_id(function_id));
        self.mark_function(function_id);
        self.generic_visit(mem, object);
    }

    unsafe fn mark_function(&mut self, function_id: FunctionId) {
        let entry = self
            .virtual_table
            .get(resolve_stable_function_id(function_id));
        (*entry).marked = true;
    }

    unsafe fn clear_mark_bits(&mut self) {
        for index in 0..self.virtual_table.length() {
            let entry = self.virtual_table.get(index);
            (*entry).marked = false;
        }
    }
}

static mut COLLECTOR_STATE: Option<FunctionGC> = None;

// Garbage collect the stable functions in the old version on an upgrade.
pub unsafe fn garbage_collect_functions<M: Memory>(
    mem: &mut M,
    virtual_table: *mut PersistentVirtualTable,
    old_actor: Value,
) {
    assert_eq!(old_actor.tag(), TAG_OBJECT);
    COLLECTOR_STATE = Some(FunctionGC::new(mem, virtual_table));
    const ACTOR_TYPE_ID: u64 = 0;
    stable_functions_gc_visit(mem, old_actor, ACTOR_TYPE_ID);
    COLLECTOR_STATE.as_mut().unwrap().run(mem);
    COLLECTOR_STATE = None;
}

#[ic_mem_fn]
unsafe fn stable_functions_gc_visit<M: Memory>(mem: &mut M, object: Value, type_id: u64) {
    let state = COLLECTOR_STATE.as_mut().unwrap();
    // TODO: also remember type for the marked object. Revisit the same object if
    // it has a different static type.
    if object != NULL_POINTER && !state.mark_set.contains(object) {
        state.mark_set.insert(mem, object);
        state.mark_stack.push(mem, StackEntry { object, type_id });
    }
}
