use crate::heap::Heap;
use crate::mem::{memcpy_bytes, memcpy_words};
use crate::types::*;

use motoko_rts_macros::{ic_fn, ic_heap_fn};

#[ic_heap_fn]
pub unsafe fn copying_gc<H: Heap>(heap: &mut H) {
    let begin_from_space = heap.get_heap_base() as usize;
    let end_from_space = heap.get_hp() as usize;
    let begin_to_space = end_from_space;
    let mut end_to_space = begin_to_space;

    let static_roots = heap.get_static_roots().as_array();

    // Evacuate roots
    evac_static_roots(
        heap,
        begin_from_space,
        begin_to_space,
        &mut end_to_space,
        static_roots,
    );

    let closure_table_loc = heap.get_closure_table_loc();
    if (*closure_table_loc).unskew() >= begin_from_space {
        evac(
            heap,
            begin_from_space,
            begin_to_space,
            &mut end_to_space,
            closure_table_loc as usize,
        );
    }

    // Scavenge to-space
    let mut p = begin_to_space;
    while p < end_to_space {
        // NB: end_to_space keeps changing within this loop
        let size = object_size(p);
        scav(heap, begin_from_space, begin_to_space, &mut end_to_space, p);
        p += size.to_bytes().0 as usize;
    }

    // Note the stats
    let new_live_size = end_to_space - begin_to_space;
    heap.note_live_size(Bytes(new_live_size as u32));

    let reclaimed = (end_from_space - begin_from_space) - (end_to_space - begin_to_space);
    heap.note_reclaimed(Bytes(reclaimed as u32));

    // Copy to-space to the beginning of from-space
    memcpy_bytes(
        begin_from_space,
        begin_to_space,
        Bytes((end_to_space - begin_to_space) as u32),
    );

    // Reset the heap pointer
    let new_hp = begin_from_space + (end_to_space - begin_to_space);
    heap.set_hp(new_hp as u32);
}

/// Evacuate (copy) an object in from-space to to-space, update end_to_space. If the object was
/// already evacuated end_to_space is not changed.
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
///     `end_to_space - begin_to_space + begin_from_space`.
///
/// - begin_to_space: Where to-space starts. See above for how this is used.
///
/// - end_to_space: Where the object in `ptr_loc` will be copied.
///
/// - ptr_loc: Location of the object to evacuate, e.g. an object field address.
///
unsafe fn evac<H: Heap>(
    heap: &mut H,
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    ptr_loc: usize,
) {
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
    let obj_size_bytes = obj_size.to_bytes();

    // Grow memory if needed
    heap.grow_memory(*end_to_space + obj_size_bytes.0 as usize);

    // Copy object to to-space
    memcpy_words(*end_to_space, obj as usize, obj_size);

    // Final location of the object after copying to-space back to from-space
    let obj_loc = (*end_to_space - begin_to_space) + begin_from_space;

    // Set forwarding pointer
    let fwd = obj as *mut FwdPtr;
    (*fwd).header.tag = TAG_FWD_PTR;
    (*fwd).fwd = skew(obj_loc);

    // Update evacuated field
    *ptr_loc = skew(obj_loc);

    // Update end of to-space
    *end_to_space += obj_size_bytes.0 as usize
}

unsafe fn scav<H: Heap>(
    heap: &mut H,
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    obj: usize,
) {
    let obj = obj as *mut Obj;

    crate::visitor::visit_pointer_fields(obj, begin_from_space, |field_addr| {
        evac(
            heap,
            begin_from_space,
            begin_to_space,
            end_to_space,
            field_addr as usize,
        );
    });
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots<H: Heap>(
    heap: &mut H,
    begin_from_space: usize,
    begin_to_space: usize,
    end_to_space: &mut usize,
    roots: *mut Array,
) {
    // The array and the objects pointed by the array are all static so we don't evacuate them. We
    // only evacuate fields of objects in the array.
    for i in 0..roots.len() {
        let obj = roots.get(i);
        scav(
            heap,
            begin_from_space,
            begin_to_space,
            end_to_space,
            obj.unskew(),
        );
    }
}
