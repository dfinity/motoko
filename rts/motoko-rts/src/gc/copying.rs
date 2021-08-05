use crate::mem_utils::{memcpy_bytes, memcpy_words};
use crate::space::Space;
use crate::types::*;

#[cfg(feature = "ic")]
unsafe fn schedule_copying_gc() {
    if super::should_do_gc() {
        copying_gc();
    }
}

#[cfg(feature = "ic")]
unsafe fn copying_gc() {
    use crate::memory::ic;

    copying_gc_internal(
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
        |reclaimed| ic::RECLAIMED += Bytes(reclaimed.0 as u64),
    );

    ic::LAST_HP = ic::HP;
}

pub unsafe fn copying_gc_internal<
    GetHp: Fn() -> usize,
    SetHp: FnMut(u32),
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    heap_base: u32,
    get_hp: GetHp,
    mut set_hp: SetHp,
    static_roots: SkewedPtr,
    continuation_table_loc: *mut SkewedPtr,
    note_live_size: NoteLiveSize,
    note_reclaimed: NoteReclaimed,
) {
    let mut to_space = Space::new();

    let heap_base = heap_base as usize;
    let end_from_space = get_hp();
    let begin_to_space = end_from_space;

    let static_roots = static_roots.as_array();

    // Evacuate roots
    evac_static_roots(heap_base, &mut to_space, static_roots);

    if (*continuation_table_loc).unskew() >= heap_base {
        evac(heap_base, &mut to_space, continuation_table_loc as usize);
    }

    // Scavenge to-space (TODO FIXME)
    let mut to_space_page = Some(to_space.first_page());
    while let Some(page) = to_space_page {
        let mut p = page.start();

        // TODO: This won't work when we have slop at the end
        let page_end = page.end();

        while p < page_end {
            let size = object_size(p);
            scav(heap_base, &mut to_space, p);
            p += size.to_bytes().0 as usize;
        }

        to_space_page = page.next();
    }

    // Note the stats (TODO)
    // let new_live_size = end_to_space - begin_to_space;
    // note_live_size(Bytes(new_live_size as u32));

    // let reclaimed = (end_from_space - heap_base) - (end_to_space - begin_to_space);
    // note_reclaimed(Bytes(reclaimed as u32));
}

/// Evacuate (copy) an object in from-space to to-space.
///
/// Arguments:
///
/// - heap_base: Where the dynamic heap starts. Used for two things:
///
///   - An object is static if its address is below this value. These objects either don't point to
///     dynamic heap, or are listed in static_roots array. Objects in static_roots are scavenged
///     separately in `evac_static_roots` below. So we skip these objects here.
///
///   - After all objects are evacuated we move to-space to from-space, to be able to do that the
///     pointers need to point to their (eventual) locations in from-space, which is calculated with
///     `address_in_to_space - begin_to_space + heap_base`.
///
/// - begin_to_space: Where to-space starts. See above for how this is used.
///
/// - ptr_loc: Location of the object to evacuate, e.g. an object field address.
///
unsafe fn evac(heap_base: usize, to_space: &mut Space, ptr_loc: usize) {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut SkewedPtr;

    let obj = (*ptr_loc).unskew() as *mut Obj;

    // Update the field if the object is already evacauted
    if obj.tag() == TAG_FWD_PTR {
        let fwd = (*(obj as *const FwdPtr)).fwd;
        *ptr_loc = fwd;
        return;
    }

    let obj_size = object_size(obj as usize);

    // Allocate space in to-space for the object
    let obj_addr = to_space.alloc_words(obj_size).unskew() as usize;

    // Copy object to to-space
    memcpy_words(obj_addr, obj as usize, obj_size);

    // Set forwarding pointer
    let fwd = obj as *mut FwdPtr;
    (*fwd).header.tag = TAG_FWD_PTR;
    (*fwd).fwd = skew(obj_addr);

    // Update evacuated field
    *ptr_loc = skew(obj_addr);
}

unsafe fn scav(heap_base: usize, to_space: &mut Space, obj: usize) {
    let obj = obj as *mut Obj;

    crate::visitor::visit_pointer_fields(obj, obj.tag(), heap_base, |field_addr| {
        evac(heap_base, to_space, field_addr as usize);
    });
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots(heap_base: usize, to_space: &mut Space, roots: *mut Array) {
    // The array and the objects pointed by the array are all static so we don't evacuate them. We
    // only evacuate fields of objects in the array.
    for i in 0..roots.len() {
        let obj = roots.get(i);
        scav(heap_base, to_space, obj.unskew());
    }
}
