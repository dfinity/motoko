//! Implements "threaded compaction" as described in The Garbage Collection Handbook section 3.3.

use crate::bitmap::{alloc_bitmap, free_bitmap, get_bit, iter_bits, set_bit, BITMAP_ITER_END};
use crate::mark_stack::{self, alloc_mark_stack, free_mark_stack, pop_mark_stack};
use crate::mem::memcpy_words;
use crate::types::*;
use crate::visitor::visit_pointer_fields;

#[no_mangle]
unsafe extern "C" fn compacting_gc() {
    compacting_gc_internal(
        || super::get_heap_base(),
        || super::HP,
        |hp| super::HP = hp,
        |live_size| super::note_live_size(live_size),
        |reclaimed| super::note_reclaimed(reclaimed),
        || super::get_static_roots(),
        || super::closure_table_loc(),
        |ptr| crate::alloc::grow_memory(ptr),
    );
}

pub unsafe fn compacting_gc_internal<
    GetHeapBase: Fn() -> u32,
    GetHp: Fn() -> u32,
    SetHp: FnMut(u32),
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
    GetStaticRoots: Fn() -> SkewedPtr,
    GetClosureTableLoc: Fn() -> *mut SkewedPtr,
    GrowMemory: Fn(usize) + Copy,
>(
    get_heap_base: GetHeapBase,
    get_hp: GetHp,
    set_hp: SetHp,
    note_live_size: NoteLiveSize,
    note_reclaimed: NoteReclaimed,
    get_static_roots: GetStaticRoots,
    get_closure_table_loc: GetClosureTableLoc,
    grow_memory: GrowMemory,
) {
    let old_hp = get_hp();
    let heap_base = get_heap_base();

    mark_compact(
        set_hp,
        heap_base,
        old_hp,
        get_static_roots(),
        get_closure_table_loc(),
    );

    let reclaimed = old_hp - old_hp;
    note_reclaimed(Bytes(reclaimed));

    let new_live_size = old_hp - heap_base;
    note_live_size(Bytes(new_live_size));
}

unsafe fn mark_compact<SetHp: FnMut(u32)>(
    set_hp: SetHp,
    heap_base: u32,
    heap_end: u32,
    static_roots: SkewedPtr,
    closure_table_loc: *mut SkewedPtr,
) {
    let heap_size = Bytes(heap_end - heap_base);

    alloc_bitmap(heap_size);
    alloc_mark_stack();

    mark_static_roots(static_roots, heap_base);

    if (*closure_table_loc).unskew() >= heap_base as usize {
        push_mark_stack(*closure_table_loc, heap_base);
    }

    mark_stack(heap_base);

    thread_roots(static_roots, heap_base);

    if (*closure_table_loc).unskew() >= heap_base as usize {
        thread(closure_table_loc);
    }

    update_fwd_refs(heap_base);
    update_bwd_refs(set_hp, heap_base);

    free_mark_stack();
    free_bitmap();
}

unsafe fn mark_static_roots(static_roots: SkewedPtr, heap_base: u32) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).unskew() as *mut Obj;
        mark_fields(obj, heap_base);
    }
}

unsafe fn push_mark_stack(obj: SkewedPtr, heap_base: u32) {
    let obj = obj.unskew() as u32;

    let obj_idx = (obj - heap_base) / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    mark_stack::push_mark_stack(obj as usize);
}

unsafe fn mark_stack(heap_base: u32) {
    while let Some(obj) = pop_mark_stack() {
        mark_fields(obj as *mut Obj, heap_base);
    }
}

unsafe fn mark_fields(obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(obj, heap_base as usize, |field_addr| {
        push_mark_stack(*field_addr, heap_base);
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
unsafe fn update_bwd_refs<SetHp: FnMut(u32)>(mut set_hp: SetHp, heap_base: u32) {
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
