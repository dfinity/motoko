//! Implements threaded compaction as described in "High-Performance Garbage Collection for
//! Memory-Constrained Environments" section 5.1.2, which is an improved version of the original
//! threaded compaction algorithm described in The Garbage Collection Handbook section 3.3.

pub mod mark_stack;

use crate::bitmap::{Bitmap, BITMAP_ITER_END};
use crate::constants::WORD_SIZE;
use crate::mem_utils::memcpy_words;
use crate::page_alloc::{Page, PageAlloc};
use crate::space::Space;
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};
use mark_stack::MarkStack;

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn schedule_compacting_gc() {
    if super::should_do_gc(crate::allocation_space::ALLOCATION_SPACE.as_ref().unwrap()) {
        compacting_gc();
    }
}

#[cfg(feature = "ic")]
#[no_mangle]
unsafe fn compacting_gc() {
    compacting_gc_internal(
        crate::page_alloc::ic::IcPageAlloc {},
        crate::allocation_space::ALLOCATION_SPACE.as_mut().unwrap(),
        crate::get_heap_base(),
        crate::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
        // note_live_size
        |_live_size| {}, // TODO
        // note_reclaimed
        |_reclaimed| {}, // TODO
    );
}

pub unsafe fn compacting_gc_internal<
    P: PageAlloc,
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    page_alloc: P,
    space: &mut Space<P>,
    heap_base: u32,
    static_roots: SkewedPtr,
    continuation_table_ptr_loc: *mut Value,
    _note_live_size: NoteLiveSize,
    _note_reclaimed: NoteReclaimed,
) {
    mark_compact(
        page_alloc,
        space,
        heap_base,
        static_roots,
        continuation_table_ptr_loc,
    );

    // TODO: Update stats
}

unsafe fn mark_compact<P: PageAlloc>(
    page_alloc: P,
    space: &mut Space<P>,
    heap_base: u32,
    static_roots: SkewedPtr,
    continuation_table_ptr_loc: *mut Value,
) {
    // Allocate bitmaps
    for page in space.iter_pages() {
        let page_size = Bytes(page.size() as u32).to_words();
        page.set_bitmap(Some(Bitmap::new(page_size.0)));
    }

    let mut stack = MarkStack::new(page_alloc.clone());

    mark_static_roots(space, &mut stack, static_roots, heap_base);

    if (*continuation_table_ptr_loc).is_ptr() {
        // TODO: No need to check if continuation table is already marked
        mark_object(space, &mut stack, *continuation_table_ptr_loc);
        // Similar to `mark_root_mutbox_fields`, `continuation_table_ptr_loc` is in static heap so it
        // will be readable when we unthread continuation table
        thread(continuation_table_ptr_loc);
    }

    mark_stack(space, &mut stack, heap_base);

    update_refs(space, heap_base);

    stack.free();

    // Free bitmaps
    for page in space.iter_pages() {
        let bitmap = page.take_bitmap().unwrap();
        bitmap.free();
    }
}

unsafe fn mark_static_roots<P: PageAlloc>(
    space: &Space<P>,
    mark_stack: &mut MarkStack<P>,
    static_roots: Value,
    heap_base: u32,
) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).as_obj();
        // Root array should only has pointers to other static MutBoxes
        debug_assert_eq!(obj.tag(), TAG_MUTBOX); // check tag
        debug_assert!((obj as u32) < heap_base); // check that MutBox is static
        mark_root_mutbox_fields(space, mark_stack, obj as *mut MutBox, heap_base);
    }
}

/// Specialized version of `mark_fields` for root `MutBox`es.
unsafe fn mark_root_mutbox_fields<P: PageAlloc>(
    space: &Space<P>,
    mark_stack: &mut MarkStack<P>,
    mutbox: *mut MutBox,
    heap_base: u32,
) {
    let field_addr = &mut (*mutbox).field;
    // TODO: Not sure if this check is necessary?
    if pointer_to_dynamic_heap(space, field_addr, heap_base as usize) {
        // TODO: We should be able to omit the "already marked" check here as no two root MutBox
        // can point to the same object (I think)
        mark_object(space, mark_stack, *field_addr);
        // It's OK to thread forward pointers here as the static objects won't be moved, so we will
        // be able to unthread objects pointed by these fields later.
        thread(field_addr);
    }
}

unsafe fn mark_object<P: PageAlloc>(space: &Space<P>, mark_stack: &mut MarkStack<P>, obj: Value) {
    let obj_tag = obj.tag();
    let obj = obj.get_ptr();

    let obj_page = space.get_address_page(obj);
    let obj_bitmap = obj_page.get_bitmap().unwrap();

    let obj_bit_idx = (obj - obj_page.contents_start()) as u32 / WORD_SIZE;

    if obj_bitmap.get(obj_bit_idx) {
        // Already marked
        return;
    }

    obj_bitmap.set(obj_bit_idx);

    mark_stack.push(obj, obj_tag);
}

unsafe fn mark_stack<P: PageAlloc>(
    space: &Space<P>,
    mark_stack: &mut MarkStack<P>,
    heap_base: u32,
) {
    while let Some((obj, tag)) = mark_stack.pop() {
        mark_fields(space, mark_stack, obj as *mut Obj, tag, heap_base);
    }
}

unsafe fn mark_fields<P: PageAlloc>(
    space: &Space<P>,
    mark_stack: &mut MarkStack<P>,
    obj: *mut Obj,
    obj_tag: Tag,
    heap_base: u32,
) {
    visit_pointer_fields(space, obj, obj_tag, heap_base as usize, |field_addr| {
        let field_value = *field_addr;
        mark_object(space, mark_stack, field_value);

        // Thread if backwards or self pointer
        if field_value.get_ptr() <= obj as usize {
            thread(field_addr);
        }
    });
}

/// Linearly scan the heap, for each live object:
///
/// - Mark step threads all backwards pointers and pointers from roots, so unthread to update those
///   pointers to the objects new location.
///
/// - Move the object
///
/// - Thread forward pointers of the object
///
unsafe fn update_refs<P: PageAlloc>(space: &Space<P>, heap_base: u32) {
    // Next object will be moved to this page
    let mut to_page_idx = space.first_page();

    // TODO: Update rustc, use unwrap_unchecked
    let mut to_page = space.get_page(to_page_idx).unwrap();

    // Next object will be moved to this address in `to_page`
    let mut to_addr = to_page.contents_start();

    // Traverse all marked bits in all pages
    let mut from_page_idx = space.first_page();

    while let Some(page) = space.get_page(from_page_idx) {
        let page_start = page.contents_start();

        let bitmap = page.get_bitmap().unwrap();
        let mut bitmap_iter = bitmap.iter();
        let mut bit = bitmap_iter.next();

        while bit != BITMAP_ITER_END {
            let p = (page_start + (bit * WORD_SIZE) as usize) as *mut Obj;

            // Get the object header first, to be able to check whether it will fit the current page or we
            // need to move on to the next page
            let obj_tag = get_header(p);
            let obj_size = object_size_(p as usize, obj_tag);

            if to_addr + obj_size.to_bytes().as_usize() > to_page.end() {
                // Object does not fit into the current page, move on to the next page
                // We know there must be more pages in the space as we compact the space and don't
                // allocate in it
                to_page_idx = to_page_idx.next();
                to_page = space.get_page(to_page_idx).unwrap();
                to_addr = to_page.contents_start();
            }

            // Update backwards references to the object's new location and restore object header
            unthread(p, to_addr as u32);

            // Move the object
            if to_addr != p as usize {
                memcpy_words(to_addr, p as usize, obj_size);
            }

            // Thread forward pointers of the object
            thread_fwd_pointers(space, to_addr as *mut Obj, heap_base);

            to_addr += obj_size.to_bytes().as_usize();

            bit = bitmap_iter.next();
        }

        from_page_idx = from_page_idx.next();
    }
}

/// Thread forwards pointers in object
unsafe fn thread_fwd_pointers<P: PageAlloc>(space: &Space<P>, obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(space, obj, obj.tag(), heap_base as usize, |field_addr| {
        if (*field_addr).get_ptr() > obj as usize {
            thread(field_addr)
        }
    });
}

/// Thread a pointer field
unsafe fn thread(field: *mut Value) {
    // Store pointed object's header in the field, field address in the pointed object's header
    let pointed = (*field).as_obj();
    let pointed_header = pointed.tag();
    *field = Value::from_raw(pointed_header);
    (*pointed).tag = field as u32;
}

/// Unthread all references at given header, replacing with `new_loc`. Restores object header.
unsafe fn unthread(obj: *mut Obj, new_loc: u32) {
    // NOTE: For this to work heap addresses need to be greater than the largest value for object
    // headers. Currently this holds. TODO: Document this better.
    let mut header = (*obj).tag;
    while header > TAG_NULL {
        // TODO: is `header > TAG_NULL` the best way to distinguish a tag from a pointer?
        let tmp = (*(header as *mut Obj)).tag;
        (*(header as *mut Value)) = Value::from_ptr(new_loc as usize);
        header = tmp;
    }
    // At the end of the chain is the original header for the object
    debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);
    (*obj).tag = header;
}

/// Follow a chain, return object header. Does not unthread.
unsafe fn get_header(obj: *mut Obj) -> Tag {
    let mut header = (*obj).tag;
    while header > TAG_NULL {
        header = (*(header as *mut Obj)).tag;
    }
    // At the end of the chain is the original header for the object
    debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);
    header
}
