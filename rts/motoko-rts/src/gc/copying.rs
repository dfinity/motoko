use core::ptr::null_mut;

use crate::constants::WORD_SIZE;
use crate::gc::incremental::object_table::OBJECT_TABLE;
use crate::mem_utils::{memcpy_bytes, memcpy_words};
use crate::memory::Memory;
use crate::types::*;

use motoko_rts_macros::ic_mem_fn;

#[no_mangle]
#[cfg(feature = "ic")]
unsafe extern "C" fn initialize_copying_gc(heap_base: u32) {
    crate::memory::ic::initialize_memory(heap_base);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_copying_gc<M: Memory>(mem: &mut M) {
    // Half of the heap.
    // NB. This expression is evaluated in compile time to a constant.
    let max_live: Bytes<u64> =
        Bytes(u64::from((crate::constants::WASM_HEAP_SIZE / 2).as_u32()) * u64::from(WORD_SIZE));

    if super::should_do_gc(max_live) {
        copying_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn copying_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::update_statistics;
    use crate::memory::ic;

    let old_heap_size = mem.get_heap_pointer();
    copying_gc_internal(
        mem,
        ic::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
    );
    update_statistics(old_heap_size);
}

pub unsafe fn copying_gc_internal<M: Memory>(
    mem: &mut M,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
) {
    // Does not work with object table.
    assert_eq!(OBJECT_TABLE, null_mut());

    let begin_from_space = mem.get_heap_base();
    let end_from_space = mem.get_heap_pointer();
    let begin_to_space = end_from_space;

    let static_roots = static_roots.as_array();

    // Evacuate roots
    evac_static_roots(mem, begin_from_space, begin_to_space, static_roots);

    if (*continuation_table_ptr_loc).is_object_id() {
        evac(
            mem,
            begin_from_space,
            begin_to_space,
            continuation_table_ptr_loc as usize,
        );
    }

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < mem.get_heap_pointer() {
        let size = block_size(p);
        scav(mem, begin_from_space, begin_to_space, p);
        p += size.to_bytes().as_usize();
    }

    let end_to_space = mem.get_heap_pointer();

    // Copy to-space to the beginning of from-space
    memcpy_bytes(
        begin_from_space,
        begin_to_space,
        Bytes((end_to_space - begin_to_space) as u32),
    );

    // Reset the heap pointer and last heap pointer
    let new_hp = begin_from_space + (end_to_space - begin_to_space);
    mem.shrink_heap(new_hp);
}

/// Evacuate (copy) an object in from-space to to-space.
///
/// Arguments:
///
/// - begin_from_space: Where the dynamic heap starts. Used for two things:
///
///   - An object is static if its address is below this value. These objects either don't point to
///     dynamic heap, or are listed in static_roots array. Objects in static_roots are scavenged
///     separately in `evac_static_roots` below. So we skip these objects here.
///
///   - After all objects are evacuated we move to-space to from-space, to be able to do that the
///     pointers need to point to their (eventual) locations in from-space, which is calculated with
///     `address_in_to_space - begin_to_space + begin_from_space`.
///
/// - begin_to_space: Where to-space starts. See above for how this is used.
///
/// - ptr_loc: Location of the object to evacuate, e.g. an object field address.
///
unsafe fn evac<M: Memory>(
    mem: &mut M,
    begin_from_space: usize,
    begin_to_space: usize,
    ptr_loc: usize,
) {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut Value;

    // Check object alignment to avoid undefined behavior. See also static_checks module.
    debug_assert_eq!((*ptr_loc).get_object_address() as u32 % WORD_SIZE, 0);

    // Update the field if the object is already evacuated
    if (*ptr_loc).tag() == TAG_FWD_PTR {
        let block = (*ptr_loc).get_object_address() as *const FwdPtr;
        let fwd = (*block).fwd;
        *ptr_loc = fwd;
        return;
    }

    let obj = (*ptr_loc).get_object_address() as *mut Obj;
    let obj_size = block_size(obj as usize);

    // Allocate space in to-space for the object
    let obj_addr = mem.alloc_words(obj_size);

    // Copy object to to-space
    memcpy_words(obj_addr, obj as usize, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (obj_addr - begin_to_space) + begin_from_space;
    let new_id = Value::new_object_id(mem, obj_loc);

    // Set forwarding pointer
    let fwd = obj as *mut FwdPtr;
    (*fwd).tag = TAG_FWD_PTR;
    (*fwd).fwd = new_id;

    // Update evacuated field
    *ptr_loc = new_id;
}

unsafe fn scav<M: Memory>(
    mem: &mut M,
    begin_from_space: usize,
    begin_to_space: usize,
    block_address: usize,
) {
    let tag = *(block_address as *const Tag);
    if !has_object_header(tag) {
        return;
    }
    let obj = block_address as *mut Obj;

    crate::visitor::visit_pointer_fields(
        mem,
        obj,
        tag,
        begin_from_space,
        |mem, field_addr| {
            evac(mem, begin_from_space, begin_to_space, field_addr as usize);
        },
        |_, _, arr| arr.len(),
    );
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots<M: Memory>(
    mem: &mut M,
    begin_from_space: usize,
    begin_to_space: usize,
    roots: *mut Array,
) {
    // The array and the objects pointed by the array are all static so we don't evacuate them. We
    // only evacuate fields of objects in the array.
    for i in 0..roots.len() {
        let obj = roots.get(i);
        scav(
            mem,
            begin_from_space,
            begin_to_space,
            obj.get_object_address(),
        );
    }
}
