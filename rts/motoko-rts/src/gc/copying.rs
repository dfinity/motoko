use crate::constants::WORD_SIZE;
use crate::mem_utils::memcpy_words;
use crate::page_alloc::{LargePageHeader, Page, PageAlloc};
use crate::space::Space;
use crate::types::*;

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn schedule_copying_gc() {
    if super::should_do_gc(crate::allocation_space::ALLOCATION_SPACE.as_ref().unwrap()) {
        copying_gc();
    }
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn copying_gc() {
    let mut to_space = Space::new(crate::page_alloc::ic::IcPageAlloc {});

    copying_gc_internal(
        crate::allocation_space::ALLOCATION_SPACE.as_ref().unwrap(),
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
    from_space: &Space<P>,
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
    evac_static_roots(to_space, static_roots);

    if (*continuation_table_loc).is_ptr() {
        evac(to_space, continuation_table_loc as usize);
    }

    // Scavenge to-space

    // Current page we're scavenging
    let mut to_space_page_idx = to_space.first_page();

    // Scavenge more?
    let mut work = true;

    while work {
        work = false;

        while let Some(page) = to_space.get_page(to_space_page_idx) {
            // Where we're at in the current page we're scavenging
            let mut p = page.contents_start();

            // Where the current page ends
            let page_end = page.end();

            // Scavenge until the end of the current page
            // NB. when we're scavenging the last page allocation pointer changes as we scavenge
            while p != to_space.allocation_pointer() && p != page_end {
                let size = object_size(p);
                scav(to_space, p);
                p += size.to_bytes().0 as usize;
            }

            let next_page_idx = to_space_page_idx.next();

            if to_space.get_page(next_page_idx).is_none() {
                // Current page is the last page. We may need to scavenge this page more after
                // evacuating large objects, so don't bump page index yet.
                break;
            } else {
                to_space_page_idx = next_page_idx;
            }
        }

        // Scavenge to-space large objects. If we evacuate in this step then we need to scavenge
        // newly evacuated objects.
        // TODO: No need to scavenge all of the large objects, just the new allocation will be
        // enough. We will need to maintain a "new large objects" list for this, and move scavenged
        // large objects to `to_space.large_object_pages`.
        for large_object in to_space.iter_large_pages() {
            work |= scav(to_space, large_object as usize);
        }
    }
}

/// Evacuate (copy) an object in from-space to to-space. Returns `false` when the object does not
/// need evacuated (already evacuated, or a "filler" object).
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

    let page_header = (obj as *mut LargePageHeader).sub(1);

    if !(*page_header).prev.is_null() {
        debug_assert_eq!((*(*page_header).prev).next, page_header);
        (*(*page_header).prev).next = (*page_header).next;
    }

    if !(*page_header).next.is_null() {
        debug_assert_eq!((*(*page_header).next).prev, page_header);
        (*(*page_header).next).prev = (*page_header).prev;
    }

    (*page_header).prev = core::ptr::null_mut();
    (*page_header).next = to_space.large_object_pages;
    if !(*to_space).large_object_pages.is_null() {
        (*(*to_space).large_object_pages).prev = page_header;
    }
    (*to_space).large_object_pages = page_header;

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
