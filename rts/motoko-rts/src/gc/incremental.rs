use motoko_rts_macros::ic_mem_fn;

use crate::{continuation_table, memory::Memory, types::Value};

use self::mark_stack::MarkStack;

pub mod mark_stack;
#[cfg(debug_assertions)]
pub mod sanity_checks;
pub mod write_barrier;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_incremental_gc<M: Memory>(mem: &mut M) {
    incremental_gc(mem)
}

#[ic_mem_fn(ic_only)]
unsafe fn incremental_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;

    #[cfg(debug_assertions)]
    sanity_checks::check_memory(mem);

    let heap_base = ic::get_aligned_heap_base() as usize;
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table: *continuation_table::continuation_table_loc(),
    };
    empty_call_stack_increment(mem, heap_base, roots);
}

static mut MARK_STACK: Option<MarkStack> = None;
static mut HEAP_BASE: usize = usize::MAX;

struct Roots {
    pub static_roots: Value,
    pub continuation_table: Value,
    // If new roots are added in future, extend `mark_roots()`.
}

/// Special GC increment invoked when the call stack is guaranteed to be empty.
/// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
/// * The mark phase is only started on an empty call stack.
/// * The moving phase can only be completed on an empty call stack.
unsafe fn empty_call_stack_increment<M: Memory>(mem: &mut M, heap_base: usize, roots: Roots) {
    if MARK_STACK.is_none() {
        HEAP_BASE = heap_base;
        MARK_STACK = Some(MarkStack::new(mem));
        mark_roots(mem, roots);
    }
    increment(mem);
}

unsafe fn mark_object<M: Memory>(mem: &mut M, value: Value) {
    println!(100, "Mark object {:#x}", value.get_ptr());
    assert!((value.get_ptr() >= HEAP_BASE));
    let object = value.as_obj();
    assert!(object.tag() >= crate::types::TAG_OBJECT && object.tag() <= crate::types::TAG_NULL);
    MARK_STACK.as_mut().unwrap().push(mem, value);
}

unsafe fn increment<M: Memory>(_mem: &mut M) {
    println!(100, "GC increment");
}

unsafe fn mark_roots<M: Memory>(mem: &mut M, roots: Roots) {
    mark_static_roots(mem, roots.static_roots);
    mark_potential_object(mem, roots.continuation_table);
}

unsafe fn mark_static_roots<M: Memory>(mem: &mut M, static_roots: Value) {
    let root_array = static_roots.as_array();
    for index in 0..root_array.len() {
        let mutbox = root_array.get(index).as_mutbox();
        assert!((mutbox as usize) < HEAP_BASE);
        mark_potential_object(mem, (*mutbox).field);
    }
}

unsafe fn mark_potential_object<M: Memory>(mem: &mut M, value: Value) {
    if value.is_ptr() && value.get_ptr() >= HEAP_BASE {
        mark_object(mem, value)
    }
}
