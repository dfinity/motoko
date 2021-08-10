//! Implements threaded compaction as described in "High-Performance Garbage Collection for
//! Memory-Constrained Environments" section 5.1.2, which is an improved version of the original
//! threaded compaction algorithm described in The Garbage Collection Handbook section 3.3.

pub mod mark_stack;

use crate::bitmap::Bitmap;
use crate::page_alloc::{Page, PageAlloc};
use crate::space::Space;
use crate::types::{Bytes, SkewedPtr};

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
        crate::allocation_space::ALLOCATION_SPACE.as_mut().unwrap(),
        crate::get_heap_base(),
        crate::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
        // note_live_size
        |live_size| {}, // TODO
        // note_reclaimed
        |reclaimed| {}, // TODO
    );
}

pub unsafe fn compacting_gc_internal<
    P: PageAlloc,
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    space: &mut Space<P>,
    heap_base: u32,
    static_roots: SkewedPtr,
    continuation_table_ptr_loc: *mut SkewedPtr,
    _note_live_size: NoteLiveSize,
    _note_reclaimed: NoteReclaimed,
) {
    mark_compact(space, heap_base, static_roots, continuation_table_ptr_loc);

    // TODO: Update stats
}

unsafe fn mark_compact<P: PageAlloc>(
    space: &mut Space<P>,
    heap_base: u32,
    static_roots: SkewedPtr,
    continuation_table_ptr_loc: *mut SkewedPtr,
) {
    // Allocate bitmaps
    {
        let mut page = Some(space.first_page());
        while let Some(page_) = page {
            let page_size_words =
                Bytes(page_.end() as u32 - page_.contents_start() as u32).to_words();
            page_.set_bitmap(Some(Bitmap::new(page_size_words.0)));
            page = page_.next();
        }
    }

    /*
    let mem_size = Bytes(heap_end - heap_base);

    alloc_bitmap(mem, mem_size);
    alloc_mark_stack(mem);

    mark_static_roots(mem, static_roots, heap_base);

    if (*continuation_table_ptr_loc).unskew() >= heap_base as usize {
        // TODO: No need to check if continuation table is already marked
        mark_object(mem, *continuation_table_ptr_loc, heap_base);
        // Similar to `mark_root_mutbox_fields`, `continuation_table_ptr_loc` is in static heap so it
        // will be readable when we unthread continuation table
        thread(continuation_table_ptr_loc);
    }

    mark_stack(mem, heap_base);

    update_refs(set_hp, heap_base);

    free_mark_stack();
    */

    // Free bitmaps
    {
        let mut page = Some(space.first_page());
        while let Some(page_) = page {
            let bitmap = page_.take_bitmap().unwrap();
            bitmap.free();
            page = page_.next();
        }
    }
}

/*
unsafe fn mark_static_roots<M: Memory>(mem: &mut M, static_roots: SkewedPtr, heap_base: u32) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).unskew() as *mut Obj;
        // Root array should only has pointers to other static MutBoxes
        debug_assert_eq!(obj.tag(), TAG_MUTBOX); // check tag
        debug_assert!((obj as u32) < heap_base); // check that MutBox is static
        mark_root_mutbox_fields(mem, obj as *mut MutBox, heap_base);
    }
}

unsafe fn mark_object<M: Memory>(mem: &mut M, obj: SkewedPtr, heap_base: u32) {
    let obj_tag = obj.tag();
    let obj = obj.unskew() as u32;

    let obj_idx = (obj - heap_base) / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    push_mark_stack(mem, obj as usize, obj_tag);
}

unsafe fn mark_stack<M: Memory>(mem: &mut M, heap_base: u32) {
    while let Some((obj, tag)) = pop_mark_stack() {
        mark_fields(mem, obj as *mut Obj, tag, heap_base);
    }
}

unsafe fn mark_fields<M: Memory>(mem: &mut M, obj: *mut Obj, obj_tag: Tag, heap_base: u32) {
    visit_pointer_fields(obj, obj_tag, heap_base as usize, |field_addr| {
        let field_value = *field_addr;
        mark_object(mem, field_value, heap_base);

        // Thread if backwards pointer
        if field_value.unskew() < obj as usize {
            thread(field_addr);
        }
    });
}

/// Specialized version of `mark_fields` for root `MutBox`es.
unsafe fn mark_root_mutbox_fields<M: Memory>(mem: &mut M, mutbox: *mut MutBox, heap_base: u32) {
    let field_addr = &mut (*mutbox).field;
    // TODO: Not sure if this check is necessary?
    if pointer_to_dynamic_heap(field_addr, heap_base as usize) {
        // TODO: We should be able to omit the "already marked" check here as no two root MutBox
        // can point to the same object (I think)
        mark_object(mem, *field_addr, heap_base);
        // It's OK to thread forward pointers here as the static objects won't be moved, so we will
        // be able to unthread objects pointed by these fields later.
        thread(field_addr);
    }
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
unsafe fn update_refs<SetHp: Fn(u32)>(set_hp: SetHp, heap_base: u32) {
    let mut free = heap_base;

    let mut bitmap_iter = iter_bits();
    let mut bit = bitmap_iter.next();
    while bit != BITMAP_ITER_END {
        let p = (heap_base + (bit * WORD_SIZE)) as *mut Obj;
        let p_new = free;

        // Update backwards references to the object's new location and restore object header
        unthread(p, p_new);

        // Move the object
        let p_size_words = object_size(p as usize);
        if p_new as usize != p as usize {
            memcpy_words(p_new as usize, p as usize, p_size_words);
        }

        free += p_size_words.to_bytes().0;

        // Thread forward pointers of the object
        thread_fwd_pointers(p_new as *mut Obj, heap_base);

        bit = bitmap_iter.next();
    }

    set_hp(free);
}

/// Thread forwards pointers in object
unsafe fn thread_fwd_pointers(obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(obj, obj.tag(), heap_base as usize, |field_addr| {
        if (*field_addr).unskew() > field_addr as usize {
            thread(field_addr)
        }
    });
}

/// Thread a pointer field
unsafe fn thread(field: *mut SkewedPtr) {
    // Store pointed object's header in the field, field address in the pointed object's header
    let pointed = (*field).unskew() as *mut Obj;
    let pointed_header = pointed.tag();
    *field = SkewedPtr(pointed_header as usize);
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
        (*(header as *mut SkewedPtr)) = skew(new_loc as usize);
        header = tmp;
    }
    // At the end of the chain is the original header for the object
    debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);
    (*obj).tag = header;
}
*/
