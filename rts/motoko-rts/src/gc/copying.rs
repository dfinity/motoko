use crate::constants::WORD_SIZE;
use crate::mem_utils::{memcpy_bytes, memcpy_words};
use crate::memory::Memory;
use crate::types::*;

use motoko_rts_macros::ic_mem_fn;

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
    use crate::memory::ic;

    copying_gc_internal(
        mem,
        ic::get_heap_base(),
        // get_hp
        || ic::HP as usize,
        // set_hp
        |hp| ic::HP = hp,
        ic::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
        // note_live_size
        |live_size| ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size),
        // note_reclaimed
        |reclaimed| ic::RECLAIMED += Bytes(u64::from(reclaimed.as_u32())),
    );

    ic::LAST_HP = ic::HP;
}

pub unsafe fn copying_gc_internal<
    M: Memory,
    GetHp: Fn() -> usize,
    SetHp: FnMut(u32),
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    mem: &mut M,
    heap_base: u32,
    get_hp: GetHp,
    mut set_hp: SetHp,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
    note_live_size: NoteLiveSize,
    note_reclaimed: NoteReclaimed,
) {
    let begin_from_space = heap_base as usize;
    let end_from_space = get_hp();
    let begin_to_space = end_from_space;

    let static_roots = static_roots.as_array();

    // Evacuate roots
    evac_static_roots(mem, begin_from_space, begin_to_space, static_roots);

    if (*continuation_table_ptr_loc).is_ptr() {
        evac(
            mem,
            begin_from_space,
            begin_to_space,
            continuation_table_ptr_loc as usize,
        );
    }

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < get_hp() {
        let size = object_size(p);
        scav(mem, begin_from_space, begin_to_space, p);
        p += size.to_bytes().as_usize();
    }

    let end_to_space = get_hp();

    // Note the stats
    let new_live_size = end_to_space - begin_to_space;
    note_live_size(Bytes(new_live_size as u32));

    let reclaimed = (end_from_space - begin_from_space) - (end_to_space - begin_to_space);
    note_reclaimed(Bytes(reclaimed as u32));

    // Copy to-space to the beginning of from-space
    memcpy_bytes(
        begin_from_space,
        begin_to_space,
        Bytes((end_to_space - begin_to_space) as u32),
    );

    // Reset the heap pointer
    let new_hp = begin_from_space + (end_to_space - begin_to_space);
    set_hp(new_hp as u32);
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

    let obj = (*ptr_loc).as_obj();

    // Check object alignment to avoid undefined behavior. See also static_checks module.
    debug_assert_eq!(obj as u32 % WORD_SIZE, 0);

    // Update the field if the object is already evacuated
    if obj.tag() == TAG_FWD_PTR {
        let fwd = (*(obj as *const FwdPtr)).fwd;
        *ptr_loc = fwd;
        return;
    }

    let obj_size = object_size(obj as usize);

    // Allocate space in to-space for the object
    let obj_addr = mem.alloc_words(obj_size, 0).get_ptr();

    // Copy object to to-space
    memcpy_words(obj_addr, obj as usize, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (obj_addr - begin_to_space) + begin_from_space;

    // Set forwarding pointer
    let fwd = obj as *mut FwdPtr;
    (*fwd).header.tag = TAG_FWD_PTR;
    (*fwd).fwd = Value::from_ptr(obj_loc);

    // Update evacuated field
    *ptr_loc = Value::from_ptr(obj_loc);
}

unsafe fn scav<M: Memory>(mem: &mut M, begin_from_space: usize, begin_to_space: usize, obj: usize) {
    let obj = obj as *mut Obj;

    crate::visitor::visit_pointer_fields(
        mem,
        obj,
        obj.tag(),
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
        scav(mem, begin_from_space, begin_to_space, obj.get_ptr());
    }
}
