//! Experimental GC, currently very simple generational GC.
//! Two generations: young and old
//! Full heap mark, then compection decision (young only, full collection, or no collection)
//! Based on the Motoko RTS mark & compact GC.

use crate::gc::mark_compact::bitmap::{
    alloc_bitmap, free_bitmap, get_bit, iter_bits, set_bit, BITMAP_ITER_END,
};
use crate::gc::mark_compact::mark_stack::{
    alloc_mark_stack, free_mark_stack, pop_mark_stack, push_mark_stack,
};

use crate::constants::WORD_SIZE;
use crate::mem_utils::memcpy_words;
use crate::memory::Memory;
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

use motoko_rts_macros::ic_mem_fn;

static mut MARKED_OLD_SPACE: usize = 0;
static mut MARKED_YOUNG_SPACE: usize = 0;

struct HeapLimits {
    pub base: usize,
    pub last_free: usize,
    pub free: usize,
}

static mut HEAP_LIMITS: HeapLimits = HeapLimits {
    base: 0,
    last_free: 0,
    free: 0,
};

static mut FORCE_YOUNG_GC: bool = true; // for test runs

#[ic_mem_fn(ic_only)]
unsafe fn schedule_experimental_gc<M: Memory>(mem: &mut M) {
    // 512 MiB slack for mark stack + allocation area for the next message
    let slack: u64 = 512 * 1024 * 1024;
    let heap_size_bytes: u64 =
        u64::from(crate::constants::WASM_HEAP_SIZE.as_u32()) * u64::from(WORD_SIZE);
    // Larger than necessary to keep things simple
    let max_bitmap_size_bytes = heap_size_bytes / 32;
    // NB. `max_live` is evaluated in compile time to a constant
    let max_live: Bytes<u64> = Bytes(heap_size_bytes - slack - max_bitmap_size_bytes);

    if super::should_do_gc(max_live) {
        experimental_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn experimental_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;

    println!(100, "INFO: Experimental GC starts ...");
    FORCE_YOUNG_GC = false;

    experimental_gc_internal(
        mem,
        ic::get_aligned_heap_base(),
        // get_hp
        || ic::HP as usize,
        // get_last_hp
        || ic::LAST_HP as usize,
        // set_hp
        |hp| ic::HP = hp,
        ic::get_static_roots(),
        crate::continuation_table::continuation_table_loc(),
        // note_live_size
        |live_size| ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size),
        // note_reclaimed
        |reclaimed| ic::RECLAIMED += Bytes(u64::from(reclaimed.as_u32())),
    );

    println!(100, "INFO: Experimental GC stops ...");

    ic::LAST_HP = ic::HP;
}

pub unsafe fn experimental_gc_internal<
    M: Memory,
    GetHp: Fn() -> usize,
    GetLastHp: Fn() -> usize,
    SetHp: Fn(u32),
    NoteLiveSize: Fn(Bytes<u32>),
    NoteReclaimed: Fn(Bytes<u32>),
>(
    mem: &mut M,
    heap_base: u32,
    get_hp: GetHp,
    get_last_hp: GetLastHp,
    set_hp: SetHp,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
    note_live_size: NoteLiveSize,
    note_reclaimed: NoteReclaimed,
) {
    HEAP_LIMITS = HeapLimits {
        base: heap_base as usize,
        last_free: core::cmp::max(get_last_hp(), heap_base as usize), // max because of aligned heap base
        free: get_hp(),
    };

    let old_hp = get_hp() as u32;

    assert_eq!(heap_base % 32, 0);

    mark_compact(
        mem,
        set_hp,
        heap_base,
        old_hp,
        static_roots,
        continuation_table_ptr_loc,
    );

    let reclaimed = old_hp - (get_hp() as u32);
    note_reclaimed(Bytes(reclaimed));

    let live = get_hp() as u32 - heap_base;
    note_live_size(Bytes(live));
}

unsafe fn mark_compact<M: Memory, SetHp: Fn(u32)>(
    mem: &mut M,
    set_hp: SetHp,
    heap_base: u32,
    heap_end: u32,
    static_roots: Value,
    continuation_table_ptr_loc: *mut Value,
) {
    let mem_size = Bytes(heap_end - heap_base);
    MARKED_OLD_SPACE = 0;
    MARKED_YOUNG_SPACE = 0;

    alloc_bitmap(mem, mem_size, heap_base / WORD_SIZE);
    alloc_mark_stack(mem);

    mark_phase(mem, heap_base, static_roots, continuation_table_ptr_loc);

    let total_space = old_generation_size();
    let ratio = old_survival_rate();
    println!(
        1000,
        "MARKED OLD {MARKED_OLD_SPACE} OF {total_space} RATIO {ratio:.3}"
    );

    let total_space = young_generation_size();
    let ratio = young_survival_rate();
    println!(
        1000,
        "MARKED YOUNG {MARKED_YOUNG_SPACE} OF {total_space} RATIO {ratio:.3}"
    );
    
    let strategy = decide_strategy();
    println!(100, "STRATEGY: {strategy:?}");
    
    let mut free = heap_end;
    if strategy != Strategy::None {
        thread_backward_phase(strategy, heap_base, static_roots, continuation_table_ptr_loc);
        free = move_phase(strategy, heap_base);
    }
    set_hp(free);

    free_mark_stack();
    free_bitmap();
}

unsafe fn mark_phase<M: Memory>(mem: &mut M, heap_base: u32, static_roots: Value, continuation_table_ptr_loc: *mut Value) {
    mark_static_roots(mem, static_roots, heap_base);

    if (*continuation_table_ptr_loc).is_ptr() {
        mark_object(mem, *continuation_table_ptr_loc);
    }

    mark_all_reachable(mem, heap_base);
}

unsafe fn old_generation_size() -> u32 {
    HEAP_LIMITS.last_free as u32 - HEAP_LIMITS.base as u32
}

unsafe fn old_generation_free_space() -> u32 {
    old_generation_size() - MARKED_OLD_SPACE as u32
}

unsafe fn old_survival_rate() -> f64 {
    MARKED_OLD_SPACE as f64 / old_generation_size() as f64
}

unsafe fn young_generation_size() -> u32 {
    HEAP_LIMITS.free as u32 - HEAP_LIMITS.last_free as u32
}

unsafe fn young_generation_free_space() -> u32 {
    young_generation_size() - MARKED_YOUNG_SPACE as u32
}

unsafe fn young_survival_rate() -> f64 {
    MARKED_YOUNG_SPACE as f64 / young_generation_size() as f64
}

unsafe fn mark_static_roots<M: Memory>(mem: &mut M, static_roots: Value, heap_base: u32) {
    let root_array = static_roots.as_array();

    // Static objects are not in the dynamic heap so don't need marking.
    for i in 0..root_array.len() {
        let obj = root_array.get(i).as_obj();
        // Root array should only have pointers to other static MutBoxes
        debug_assert_eq!(obj.tag(), TAG_MUTBOX); // check tag
        debug_assert!((obj as u32) < heap_base); // check that MutBox is static
        mark_root_mutbox_fields(mem, obj as *mut MutBox, heap_base);
    }
}

unsafe fn mark_object<M: Memory>(mem: &mut M, obj: Value) {
    let obj_tag = obj.tag();
    let obj = obj.get_ptr() as u32;

    // Check object alignment to avoid undefined behavior. See also static_checks module.
    debug_assert_eq!(obj % WORD_SIZE, 0);

    let obj_idx = obj / WORD_SIZE;

    if get_bit(obj_idx) {
        // Already marked
        return;
    }

    set_bit(obj_idx);
    push_mark_stack(mem, obj as usize, obj_tag);

    let obj_size = object_size(obj as usize).to_bytes().as_usize();

    if obj >= HEAP_LIMITS.last_free as u32 {
        MARKED_YOUNG_SPACE += obj_size;
    } else {
        MARKED_OLD_SPACE += obj_size;
    }
}

unsafe fn mark_all_reachable<M: Memory>(mem: &mut M, heap_base: u32) {
    while let Some((obj, tag)) = pop_mark_stack() {
        mark_fields(mem, obj as *mut Obj, tag, heap_base)
    }
}

unsafe fn mark_fields<M: Memory>(mem: &mut M, obj: *mut Obj, obj_tag: Tag, heap_base: u32) {
    visit_pointer_fields(
        mem,
        obj,
        obj_tag,
        heap_base as usize,
        |mem, field_addr| {
            let field_value = *field_addr;
            mark_object(mem, field_value);
        },
        |mem, slice_start, arr| {
            const SLICE_INCREMENT: u32 = 127;
            debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
            if arr.len() - slice_start > SLICE_INCREMENT {
                let new_start = slice_start + SLICE_INCREMENT;
                // push an entire (suffix) array slice
                push_mark_stack(mem, arr as usize, new_start);
                new_start
            } else {
                arr.len()
            }
        },
    );
}

/// Specialized version of `mark_fields` for root `MutBox`es.
unsafe fn mark_root_mutbox_fields<M: Memory>(mem: &mut M, mutbox: *mut MutBox, heap_base: u32) {
    let field_addr = &mut (*mutbox).field;
    if pointer_to_dynamic_heap(field_addr, heap_base as usize) {
        mark_object(mem, *field_addr);
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Strategy {
    Young,
    Full,
    None,
}

unsafe fn decide_strategy() -> Strategy {
    // TODO: Also determine whether free space is urgently needed (allocation on full heap), then go for full collection
    const SUBSTANTIAL_FREE_SPACE: u32 = 1024 * 1024 * 1024;
    const CRITICAL_MEMORY_LIMIT: u32 = (4096 - 256) * 1024 * 1024;
    if FORCE_YOUNG_GC || young_survival_rate() < 0.8 {
        Strategy::Young
    } else if old_survival_rate() < 0.5
        || old_generation_size() + young_generation_size() >= CRITICAL_MEMORY_LIMIT
            && old_survival_rate() < 0.95
        || old_generation_free_space() + young_generation_free_space() >= SUBSTANTIAL_FREE_SPACE
    {
        Strategy::Full
    } else {
        Strategy::None
    }
}

unsafe fn should_be_threaded(strategy: Strategy, obj: *mut Obj) -> bool {
    let address = obj as usize;
    match strategy {
        Strategy::Young => address >= HEAP_LIMITS.last_free,
        Strategy::Full => true,
        Strategy::None => false
    }
}

unsafe fn thread_backward_phase(strategy: Strategy, heap_base: u32, static_roots: Value, continuation_table_ptr_loc: *mut Value) {
    thread_all_backward_pointers(strategy, heap_base);
    
    // For static root, also forward pointers are threaded. 
    // Therefore, this must happen after the heap traversal for backwards pointer threading.
    thread_static_roots(strategy, static_roots, heap_base);

    if (*continuation_table_ptr_loc).is_ptr() {
        // Similar to `mark_root_mutbox_fields`, `continuation_table_ptr_loc` is in static heap so
        // it will be readable when we unthread the continuation table
        thread(strategy, continuation_table_ptr_loc);
    }
}

unsafe fn thread_static_roots(strategy: Strategy, static_roots: Value, heap_base: u32) {
    let root_array = static_roots.as_array();

    for i in 0..root_array.len() {
        let obj = root_array.get(i).as_obj();
        // Root array should only have pointers to other static MutBoxes
        debug_assert_eq!(obj.tag(), TAG_MUTBOX); // check tag
        debug_assert!((obj as u32) < heap_base); // check that MutBox is static
        thread_root_mutbox_fields(strategy, obj as *mut MutBox, heap_base);
    }
}

unsafe fn thread_root_mutbox_fields(strategy: Strategy, mutbox: *mut MutBox, heap_base: u32) {
    let field_addr = &mut (*mutbox).field;
    if pointer_to_dynamic_heap(field_addr, heap_base as usize) {
        // It's OK to thread forward pointers here as the static objects won't be moved, so we will
        // be able to unthread objects pointed by these fields later.
        thread(strategy, field_addr);
    }
}

unsafe fn thread_all_backward_pointers(strategy: Strategy, heap_base: u32) {
    let mut bitmap_iter = iter_bits();
    let mut bit = bitmap_iter.next();
    while bit != BITMAP_ITER_END {
        let obj = (bit * WORD_SIZE) as *mut Obj;
        let tag = obj.tag();
        
        thread_backward_pointer_fields(strategy, obj, tag, heap_base);
        
        bit = bitmap_iter.next();
    }
}

unsafe fn thread_backward_pointer_fields(strategy: Strategy, obj: *mut Obj, obj_tag: Tag, heap_base: u32) {
    assert!(obj_tag < TAG_ARRAY_SLICE_MIN);
    visit_pointer_fields(
        &mut (),
        obj,
        obj_tag,
        heap_base as usize,
        |_, field_addr| {
            let field_value = *field_addr;
            
            // Thread if backwards or self pointer
            if field_value.get_ptr() <= obj as usize {
                thread(strategy, field_addr);
            }
        },
        |_, slice_start, arr| {
            debug_assert!(slice_start == 0);
            arr.len()
        },
    );
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
/// Returns the new free pointer
///
unsafe fn move_phase(strategy: Strategy, heap_base: u32) -> u32 {
    let mut free = heap_base;

    let mut bitmap_iter = iter_bits();
    let mut bit = bitmap_iter.next();
    while bit != BITMAP_ITER_END {
        let p = (bit * WORD_SIZE) as *mut Obj;
        let compacting = strategy == Strategy::Full
            || p as u32 >= HEAP_LIMITS.last_free as u32 && strategy == Strategy::Young;
        if !compacting {
            free = p as u32;
        }
        let p_new = free;

        if compacting {
            // Update backwards references to the object's new location and restore object header
            unthread(strategy, p, p_new);
        }

        // Move the object
        let p_size_words = object_size(p as usize);
        if p_new as usize != p as usize {
            memcpy_words(p_new as usize, p as usize, p_size_words);
            // Update forward address
            let new_obj = p_new as *mut Obj;
            (*new_obj).forward = Value::from_ptr(p_new as usize);
        }

        free += p_size_words.to_bytes().as_u32();

        // Thread forward pointers of the object, even if not moved
        thread_fwd_pointers(strategy, p_new as *mut Obj, heap_base);

        bit = bitmap_iter.next();
    }

    free
}

/// Thread forward pointers in object
unsafe fn thread_fwd_pointers(strategy: Strategy, obj: *mut Obj, heap_base: u32) {
    visit_pointer_fields(
        &mut (),
        obj,
        obj.tag(),
        heap_base as usize,
        |_, field_addr| {
            if (*field_addr).get_ptr() > obj as usize {
                thread(strategy, field_addr)
            }
        },
        |_, _, arr| arr.len(),
    );
}

/// Thread a pointer field
unsafe fn thread(strategy: Strategy, field: *mut Value) {
    // Store pointed object's header in the field, field address in the pointed object's header
    let pointed = (*field).get_ptr() as *mut Obj;
    if should_be_threaded(strategy, pointed) {
        let pointed_header = pointed.tag();
        *field = Value::from_raw(pointed_header);
        (*pointed).tag = field as u32;
    }
}

/// Unthread all references at given header, replacing with `new_loc`. Restores object header.
unsafe fn unthread(strategy: Strategy, obj: *mut Obj, new_loc: u32) {
    assert!(should_be_threaded(strategy, obj));
    let mut header = obj.tag();

    // All objects and fields are word-aligned, and tags have the lowest bit set, so use the lowest
    // bit to distinguish a header (tag) from a field address.
    while header & 0b1 == 0 {
        let tmp = (header as *const Obj).tag();
        (*(header as *mut Value)) = Value::from_ptr(new_loc as usize);
        header = tmp;
    }

    // At the end of the chain is the original header for the object
    debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);

    (*obj).tag = header;
}
