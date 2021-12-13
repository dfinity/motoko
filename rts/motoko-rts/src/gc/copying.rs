use crate::constants::WORD_SIZE;
use crate::mem_utils::memcpy_words;
use crate::page_alloc::{LargePageHeader, Page, PageAlloc};
use crate::space::Space;
use crate::types::*;

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn schedule_copying_gc() {
    let max_live: Bytes<u64> =
        Bytes(u64::from((crate::constants::WASM_HEAP_SIZE / 2).as_u32()) * u64::from(WORD_SIZE));

    if super::should_do_gc(
        crate::allocation_space::ALLOCATION_SPACE.assume_init_ref(),
        max_live,
    ) {
        copying_gc();
    }
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn copying_gc() {
    let mut to_space = Space::new(crate::page_alloc::ic::IcPageAlloc {});

    copying_gc_internal(
        crate::allocation_space::ALLOCATION_SPACE.assume_init_mut(),
        &mut to_space,
        crate::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
        // note_live_size
        |_live_size| {}, // TODO
        // note_reclaimed
        |_reclaimed| {}, // TODO
    );

    crate::allocation_space::free_and_update_allocation_space(to_space);
}

// TODO: Update stats
pub unsafe fn copying_gc_internal<
    P: PageAlloc,
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    from_space: &mut Space<P>,
    to_space: &mut Space<P>,
    static_roots: Value,
    continuation_table_loc: *mut Value,
    _note_live_size: NoteLiveSize,
    _note_reclaimed: NoteReclaimed,
) {
    // Reset mark bits of large objects
    for large_object_page in from_space.large_object_pages.iter() {
        (large_object_page.add(1) as *mut Obj).unmark_large();
    }

    let static_roots = static_roots.as_array();

    // Evacuate roots
    evac_static_roots(to_space, static_roots);

    if (*continuation_table_loc).is_ptr() {
        evac(to_space, continuation_table_loc as usize);
    }

    // Scavenge to-space

    // Current page we're scavenging
    let mut to_space_page_idx = to_space.first_page();

    // A space will have at least one page, so the `unwrap` below does not fail.
    let mut scav_page = to_space.get_page(to_space_page_idx).unwrap();
    let mut scav_ptr = scav_page.contents_start();

    loop {
        // Scavenge from where we left to the end of the space
        while scav_ptr != to_space.allocation_pointer() && scav_ptr != scav_page.end() {
            let size = object_size(scav_ptr);
            scav(to_space, scav_ptr);
            scav_ptr += size.to_bytes().as_usize();
        }

        // If we're at the end of the current page move on to the next page
        if scav_ptr == scav_page.end() {
            let next_page_idx = to_space_page_idx.next();
            if let Some(page) = to_space.get_page(next_page_idx) {
                to_space_page_idx = next_page_idx;
                scav_page = page;
                scav_ptr = scav_page.contents_start();
                continue;
            }
        }

        // Otherwise we finished scavenging to-space, scavenge large objects, and loop if we
        // evacuated anything
        let mut did_work = false;

        for large_page_header in core::mem::take(&mut to_space.evacuated_large_object_pages) {
            let large_object = large_page_header.add(1) as *mut Obj;
            // println!(100, "Scavenging large object {:#x}", large_object as usize);
            did_work |= scav(to_space, large_object as usize);
            to_space.large_object_pages.push(large_page_header);
        }

        if !did_work {
            break;
        } else {
            // println!(100, "Looping");
        }
    }
}

/// Evacuate (copy) an object in from-space to to-space. Returns `false` when the object does not
/// need to be evacuated (already evacuated, or a filler object).
unsafe fn evac<P: PageAlloc>(to_space: &mut Space<P>, ptr_loc: usize) -> bool {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut Value;

    let obj = (*ptr_loc).as_obj();

    // Check object alignment to avoid undefined behavior. See also static_checks module.
    debug_assert_eq!(obj as u32 % WORD_SIZE, 0);

    if obj.is_large() {
        return evac_large(to_space, obj);
    }

    let tag = obj.tag();

    // Update the field if the object is already evacauted
    if tag == TAG_FWD_PTR {
        let fwd = (*(obj as *const FwdPtr)).fwd;
        *ptr_loc = fwd;
        return false; // already evacuated
    } else if tag == TAG_ONE_WORD_FILLER || tag == TAG_FREE_SPACE {
        return false; // nothing to evacuate
    }

    let obj_size = object_size(obj as usize);

    // Allocate space in to-space for the object
    let obj_addr = to_space.alloc_words(obj_size).get_ptr();

    // Copy object to to-space
    memcpy_words(obj_addr, obj as usize, obj_size);

    // Set forwarding pointer
    let fwd = obj as *mut FwdPtr;
    fwd.set_tag();
    fwd.set_forwarding(obj_addr);

    // Update evacuated field
    *ptr_loc = Value::from_ptr(obj_addr);

    true
}

/// Returns `false` when the object is already evacuated.
unsafe fn evac_large<P: PageAlloc>(to_space: &mut Space<P>, obj: *mut Obj) -> bool {
    if obj.is_marked() {
        return false;
    }

    // Object not marked, it should be in from-space large object list. Mark it and move it to
    // to-space large object list.
    obj.mark_large();

    // println!(100, "Evacuating large object {:#x}", obj as usize);

    let page_header = (obj as *mut LargePageHeader).sub(1);

    to_space.evacuated_large_object_pages.push(page_header);

    true
}

/// Returns `false` when nothing is evacuated.
unsafe fn scav<P: PageAlloc>(to_space: &mut Space<P>, obj: usize) -> bool {
    let obj = obj as *mut Obj;

    let mut evacuated = false;

    crate::visitor::visit_pointer_fields(obj, obj.tag(), |field_addr| {
        evacuated |= evac(to_space, field_addr as usize);
    });

    evacuated
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots<P: PageAlloc>(to_space: &mut Space<P>, roots: *mut Array) {
    // The array and the objects pointed by the array are all static so we don't evacuate them. We
    // only evacuate fields of objects in the array.
    for i in 0..roots.len() {
        let obj = roots.get(i);
        scav(to_space, obj.get_ptr());
    }
}
