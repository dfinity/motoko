//! Implements "threaded compaction" as described in The Garbage Collection Handbook section 3.3.

use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, iter_bits, set_bit, BITMAP_ITER_END};
use crate::heap::Heap;
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::mem::memcpy_words;
use crate::types::*;
use crate::visitor::visit_pointer_fields;

use motoko_rts_macros::ic_heap_fn;

#[ic_heap_fn(ic_only)]
unsafe fn compacting_gc<H: Heap>(heap: &mut H) {
    compacting_gc_internal(
        heap,
        crate::heap::ic::get_heap_base(),
        || crate::heap::ic::HP,
        |hp| crate::heap::ic::HP = hp,
        crate::heap::ic::get_static_roots(),
        crate::closure_table::closure_table_loc(),
        |live_size| {
            crate::heap::ic::MAX_LIVE = ::core::cmp::max(crate::heap::ic::MAX_LIVE, live_size)
        },
        |reclaimed| crate::heap::ic::RECLAIMED += Bytes(reclaimed.0 as u64),
    );
}

pub unsafe fn compacting_gc_internal<
    H: Heap,
    GetHp: Fn() -> u32,
    SetHp: Fn(u32),
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    heap: &mut H,
    heap_base: u32,
    get_hp: GetHp,
    set_hp: SetHp,
    static_roots: SkewedPtr,
    closure_table_loc: *mut SkewedPtr,
    note_live_size: NoteLiveSize,
    note_reclaimed: NoteReclaimed,
) {
    let old_hp = get_hp();

    mark_compact(
        heap,
        set_hp,
        heap_base,
        old_hp,
        static_roots,
        closure_table_loc,
    );

    let reclaimed = old_hp - get_hp();
    note_reclaimed(Bytes(reclaimed));

    let live = get_hp() - heap_base;
    note_live_size(Bytes(live));
}

unsafe fn mark_compact<H: Heap, SetHp: Fn(u32)>(
    heap: &mut H,
    set_hp: SetHp,
    heap_base: u32,
    heap_end: u32,
    static_roots: SkewedPtr,
    closure_table_loc: *mut SkewedPtr,
) {
    let heap_size = Bytes(heap_end - heap_base);

    alloc_bitmap(heap, heap_size);
    alloc_mark_stack(heap);

    mark_static_roots(heap, static_roots, heap_base);

    if (*closure_table_loc).unskew() >= heap_base as usize {
        push_mark_stack(heap, *closure_table_loc, heap_base);
    }

    mark_stack(heap, heap_base);

    thread_roots(static_roots, heap_base);

    if (*closure_table_loc).unskew() >= heap_base as usize {
        thread(closure_table_loc);
    }

    update_fwd_refs(heap_base);
    update_bwd_refs(set_hp, heap_base);

    free_mark_stack();
    free_bitmap();
}

unsafe fn mark_static_roots<H: Heap>(heap: &mut H, static_roots: SkewedPtr, heap_base: u32) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).unskew() as *mut Obj;
        mark_fields(heap, obj, heap_base);
    }
}

unsafe fn push_mark_stack<H: Heap>(heap: &mut H, obj: SkewedPtr, heap_base: u32) {
    let obj = obj.unskew() as u32;

    let obj_idx = (obj - heap_base) / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    mark_stack::push_mark_stack(heap, obj as usize);
}

unsafe fn mark_stack<H: Heap>(heap: &mut H, heap_base: u32) {
    while let Some(obj) = pop_mark_stack() {
        mark_fields(heap, obj as *mut Obj, heap_base);
    }
}

unsafe fn mark_fields<H: Heap>(heap: &mut H, obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(obj, heap_base as usize, |field_addr| {
        push_mark_stack(heap, *field_addr, heap_base);
    });
}

unsafe fn thread_roots(static_roots: SkewedPtr, heap_base: u32) {
    // Static roots
    let root_array = static_roots.as_array();
    for i in 0..root_array.len() {
        thread_obj_fields(root_array.get(i).unskew() as *mut Obj, heap_base);
    }
    // No need to thread closure table here as it's on heap and we already marked it
}

/// Scan the heap, update forward references. At the end of this pass all fields will be threaded
/// and forward references will be updated, pointing to the object's new location.
unsafe fn update_fwd_refs(heap_base: u32) {
    let mut free = heap_base;

    let mut bitmap_iter = iter_bits();
    let mut bit = bitmap_iter.next();
    while bit != BITMAP_ITER_END {
        let p = (heap_base + (bit * WORD_SIZE)) as *mut Obj;

        // Update forward references to the object to the object's new location and restore
        // object header
        unthread(p, free);

        // Thread fields
        thread_obj_fields(p, heap_base);

        free += object_size(p as usize).to_bytes().0;

        bit = bitmap_iter.next();
    }
}

/// Expects all fields to be threaded. Updates backward references and moves objects to their new
/// locations.
unsafe fn update_bwd_refs<SetHp: Fn(u32)>(set_hp: SetHp, heap_base: u32) {
    let mut free = heap_base;

    let mut bitmap_iter = iter_bits();
    let mut bit = bitmap_iter.next();
    while bit != BITMAP_ITER_END {
        let p = (heap_base + (bit * WORD_SIZE)) as *mut Obj;

        // Update backward references to the object's new location and restore object header
        unthread(p, free);

        // All references to the object now point to the new location, move the object
        let p_size_words = object_size(p as usize);
        if free as usize != p as usize {
            memcpy_words(free as usize, p as usize, p_size_words);
        }

        free += p_size_words.to_bytes().0;

        bit = bitmap_iter.next();
    }

    set_hp(free);
}

unsafe fn thread_obj_fields(obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(obj, heap_base as usize, |field_addr| thread(field_addr));
}

unsafe fn thread(field: *mut SkewedPtr) {
    // Store pointed object's header in the field, field address in the pointed object's header
    let pointed = (*field).unskew() as *mut Obj;
    let pointed_header = pointed.tag();
    *field = SkewedPtr(pointed_header as usize);
    (*pointed).tag = field as u32;
}

/// Unthread all references, replacing with `new_loc`
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
