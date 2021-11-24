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
        crate::allocation_space::ALLOCATION_SPACE.as_ref().unwrap(),
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
        crate::allocation_space::ALLOCATION_SPACE.as_mut().unwrap(),
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
    for large_object in from_space.iter_large_pages() {
        large_object.unmark_large();
    }

    let static_roots = static_roots.as_array();

    // Evacuate roots
    evac_static_roots(from_space, to_space, static_roots);

    if (*continuation_table_loc).is_ptr() {
        evac(from_space, to_space, continuation_table_loc as usize);
    }

    // Scavenge to-space

    // Current page we're scavenging
    let mut to_space_page_idx = to_space.first_page();

    // Scavenge more?
    let mut work = true;

    // A space will have at least one page, so the `unwrap` below does not fail.
    let mut scav_page = to_space.get_page(to_space_page_idx).unwrap();
    let mut scav_ptr = scav_page.contents_start();

    loop {
        // Scavenge from where we left to the end of the space
        while scav_ptr != to_space.allocation_pointer() && scav_ptr != scav_page.end() {
            let size = object_size(scav_ptr);
            scav(from_space, to_space, scav_ptr);
            scav_ptr += size.to_bytes().as_usize();
        }

        // If we're at the end of the current page move on to the next page
        if scav_ptr == scav_page.end() {
            let next_page_idx = to_space_page_idx.next();
            if let Some(page) = to_space.get_page(next_page_idx) {
                scav_page = page;
                scav_ptr = scav_page.contents_start();
                continue;
            }
        }

        // Otherwise we finished scavenging to-space, scavenge large objects, and loop if we
        // evacuated anything
        let mut did_work = false;

        let mut evacuated_large_objects = to_space.iter_evacuated_large_pages();
        let mut large_object: Option<*mut Obj> = evacuated_large_objects.next();
        while let Some(large_object_) = large_object {
            // println!(100, "Scavenging large object {:#x}", large_object_ as usize);
            did_work |= scav(from_space, to_space, large_object_ as usize);
            large_object = evacuated_large_objects.next();

            let mut large_object_header = (large_object_ as *mut LargePageHeader).sub(1);

            // Check for obvious cycles -- this caught a bug before
            debug_assert_ne!((*large_object_header).next, large_object_header);
            debug_assert_ne!((*large_object_header).prev, large_object_header);

            (*large_object_header).next = to_space.large_object_pages;
            if !to_space.large_object_pages.is_null() {
                (*to_space.large_object_pages).prev = large_object_header;
            }
            (*large_object_header).prev = core::ptr::null_mut();
            to_space.large_object_pages = large_object_header;
        }

        to_space.evacuated_large_object_pages = core::ptr::null_mut();

        if !did_work {
            break;
        }
    }
}

/// Evacuate (copy) an object in from-space to to-space. Returns `false` when the object does not
/// need to be evacuated (already evacuated, or a filler object).
unsafe fn evac<P: PageAlloc>(
    from_space: &mut Space<P>,
    to_space: &mut Space<P>,
    ptr_loc: usize,
) -> bool {
    // Field holds a skewed pointer to the object to evacuate
    let ptr_loc = ptr_loc as *mut Value;

    let obj = (*ptr_loc).as_obj();

    // Check object alignment to avoid undefined behavior. See also static_checks module.
    debug_assert_eq!(obj as u32 % WORD_SIZE, 0);

    if obj.is_large() {
        return evac_large(from_space, to_space, obj);
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
unsafe fn evac_large<P: PageAlloc>(
    from_space: &mut Space<P>,
    to_space: &mut Space<P>,
    obj: *mut Obj,
) -> bool {
    if obj.is_marked() {
        return false;
    }

    // Object not marked, it should be in from-space large object list. Mark it and move it to
    // to-space large object list.
    obj.mark_large();

    let page_header = (obj as *mut LargePageHeader).sub(1);

    if (*page_header).prev.is_null() {
        debug_assert_eq!((*from_space).large_object_pages, page_header);
        (*from_space).large_object_pages = (*page_header).next;
        if !(*page_header).next.is_null() {
            debug_assert_eq!((*(*page_header).next).prev, page_header);
            (*(*page_header).next).prev = core::ptr::null_mut();
        }
    } else {
        debug_assert_eq!((*(*page_header).prev).next, page_header);
        (*(*page_header).prev).next = (*page_header).next;
        if !(*page_header).next.is_null() {
            debug_assert_eq!((*(*page_header).next).prev, page_header);
            (*(*page_header).next).prev = (*page_header).prev;
        }
    }

    (*page_header).prev = core::ptr::null_mut();
    (*page_header).next = to_space.evacuated_large_object_pages;
    if !(*to_space).evacuated_large_object_pages.is_null() {
        (*(*to_space).evacuated_large_object_pages).prev = page_header;
    }
    (*to_space).evacuated_large_object_pages = page_header;

    true
}

/// Returns `false` when nothing is evacuated.
unsafe fn scav<P: PageAlloc>(
    from_space: &mut Space<P>,
    to_space: &mut Space<P>,
    obj: usize,
) -> bool {
    let obj = obj as *mut Obj;

    let mut evacuated = false;

    crate::visitor::visit_pointer_fields(obj, obj.tag(), |field_addr| {
        evacuated |= evac(from_space, to_space, field_addr as usize);
    });

    evacuated
}

// We have a special evacuation routine for "static roots" array: we don't evacuate elements of
// "static roots", we just scavenge them.
unsafe fn evac_static_roots<P: PageAlloc>(
    from_space: &mut Space<P>,
    to_space: &mut Space<P>,
    roots: *mut Array,
) {
    // The array and the objects pointed by the array are all static so we don't evacuate them. We
    // only evacuate fields of objects in the array.
    for i in 0..roots.len() {
        let obj = roots.get(i);
        scav(from_space, to_space, obj.get_ptr());
    }
}
