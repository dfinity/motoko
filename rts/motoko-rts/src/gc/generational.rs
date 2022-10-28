//! Generational compacting GC.
//! Two generations: young and old.
//! Frequent collection of young generation, sporadic full collection (old + young).
//! Young generation collection requires an extra root set of old-to-young pointers.
//! A write barrier catches all pointers leading from old to young generation.
//! Compaction is based on the existing Motoko RTS threaded mark & compact GC.

pub mod remembered_set;
#[cfg(debug_assertions)]
mod sanity_checks;
pub mod write_barrier;

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

use self::write_barrier::REMEMBERED_SET;

#[ic_mem_fn(ic_only)]
unsafe fn schedule_generational_gc<M: Memory>(mem: &mut M) {
    let limits = get_limits();
    if decide_strategy(&limits).is_some() {
        generational_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn generational_gc<M: Memory>(mem: &mut M) {
    use crate::memory::ic;

    let old_limits = get_limits();
    let roots = Roots {
        static_roots: ic::get_static_roots(),
        continuation_table_ptr_loc: crate::continuation_table::continuation_table_loc(),
    };
    let heap = Heap {
        mem,
        limits: get_limits(),
        roots,
    };
    let strategy = decide_strategy(&heap.limits);
    #[cfg(debug_assertions)]
    let forced_gc = strategy.is_none();
    let strategy = strategy.unwrap_or(Strategy::Young);
    let mut gc = GenerationalGC::new(heap, strategy);

    #[cfg(debug_assertions)]
    sanity_checks::verify_snapshot(&gc.heap, false);

    gc.run();

    let new_limits = &gc.heap.limits;
    set_limits(&gc.heap.limits);
    update_statistics(&old_limits, new_limits);
    update_strategy(strategy, new_limits);

    #[cfg(debug_assertions)]
    if !forced_gc {
        #[cfg(debug_assertions)]
        sanity_checks::check_memory(&gc.heap.limits, &gc.heap.roots);
        #[cfg(debug_assertions)]
        sanity_checks::take_snapshot(&mut gc.heap);
    }

    write_barrier::init_write_barrier(gc.heap.mem);
}

#[cfg(feature = "ic")]
unsafe fn get_limits() -> Limits {
    use crate::memory::ic;
    Limits {
        base: ic::get_aligned_heap_base() as usize,
        last_free: core::cmp::max(ic::LAST_HP as usize, ic::get_aligned_heap_base() as usize), // max because of aligned heap base
        free: ic::HP as usize,
    }
}

#[cfg(feature = "ic")]
unsafe fn set_limits(limits: &Limits) {
    use crate::memory::ic;
    ic::HP = limits.free as u32;
    ic::LAST_HP = limits.free as u32;
}

#[cfg(feature = "ic")]
unsafe fn update_statistics(old_limits: &Limits, new_limits: &Limits) {
    use crate::memory::ic;
    let live_size = Bytes(new_limits.free as u32 - new_limits.base as u32);
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size);
    ic::RECLAIMED += Bytes(old_limits.free as u64 - new_limits.free as u64);
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Strategy {
    Young,
    Full,
}

#[cfg(feature = "ic")]
static mut OLD_GENERATION_THRESHOLD: usize = 32 * 1024 * 1024;

#[cfg(feature = "ic")]
static mut PASSED_CRITICAL_LIMIT: bool = false;

#[cfg(feature = "ic")]
const CRITICAL_MEMORY_LIMIT: usize = (4096 - 512) * 1024 * 1024;

#[cfg(feature = "ic")]
unsafe fn decide_strategy(limits: &Limits) -> Option<Strategy> {
    const YOUNG_GENERATION_THRESHOLD: usize = 8 * 1024 * 1024;

    let old_generation_size = limits.last_free - limits.base;
    let young_generation_size = limits.free - limits.last_free;

    if limits.free >= CRITICAL_MEMORY_LIMIT && !PASSED_CRITICAL_LIMIT {
        PASSED_CRITICAL_LIMIT = true;
        Some(Strategy::Full)
    } else if old_generation_size > OLD_GENERATION_THRESHOLD {
        Some(Strategy::Full)
    } else if young_generation_size > YOUNG_GENERATION_THRESHOLD {
        Some(Strategy::Young)
    } else {
        None
    }
}

#[cfg(feature = "ic")]
unsafe fn update_strategy(strategy: Strategy, limits: &Limits) {
    const GROWTH_RATE: f64 = 1.5;
    if strategy == Strategy::Full {
        OLD_GENERATION_THRESHOLD = (limits.free as f64 * GROWTH_RATE) as usize;
        if limits.free < CRITICAL_MEMORY_LIMIT {
            PASSED_CRITICAL_LIMIT = false
        }
    }
}

pub struct Heap<'a, M: Memory> {
    pub mem: &'a mut M,
    pub limits: Limits,
    pub roots: Roots,
}

pub struct Roots {
    pub static_roots: Value,
    pub continuation_table_ptr_loc: *mut Value,
}

pub struct Limits {
    pub base: usize,
    pub last_free: usize,
    pub free: usize,
}

pub struct GenerationalGC<'a, M: Memory> {
    pub heap: Heap<'a, M>,
    marked_space: usize,
    strategy: Strategy,
}

impl<'a, M: Memory> GenerationalGC<'a, M> {
    pub fn new(heap: Heap<M>, strategy: Strategy) -> GenerationalGC<M> {
        GenerationalGC {
            heap,
            marked_space: 0,
            strategy,
        }
    }

    pub unsafe fn run(&mut self) {
        assert_eq!(self.heap.limits.base % 32, 0);
        self.alloc_mark_structures();
        self.mark_phase();
        self.compact_phase();
        self.free_mark_structures();
    }

    unsafe fn alloc_mark_structures(&mut self) {
        const BITMAP_ALGNMENT: usize = 8 * WORD_SIZE as usize;
        let mark_offset = match self.strategy {
            Strategy::Young => self.heap.limits.last_free / BITMAP_ALGNMENT * BITMAP_ALGNMENT,
            Strategy::Full => self.heap.limits.base,
        };
        let mem_size = Bytes((self.heap.limits.free - mark_offset) as u32);
        alloc_bitmap(self.heap.mem, mem_size, mark_offset as u32 / WORD_SIZE);
        alloc_mark_stack(self.heap.mem);
    }

    unsafe fn free_mark_structures(&mut self) {
        free_mark_stack();
        free_bitmap();
    }

    unsafe fn mark_phase(&mut self) {
        self.marked_space = 0;
        self.mark_root_set();
        self.mark_all_reachable();
    }

    unsafe fn mark_root_set(&mut self) {
        self.mark_static_roots();

        if (*self.heap.roots.continuation_table_ptr_loc).is_ptr() {
            self.mark_object(*self.heap.roots.continuation_table_ptr_loc);
        }

        if self.strategy == Strategy::Young {
            self.mark_additional_young_root_set();
        }
    }

    unsafe fn mark_static_roots(&mut self) {
        let root_array = self.heap.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            debug_assert_eq!(object.tag(), TAG_MUTBOX);
            debug_assert!((object as usize) < self.heap.limits.base);
            self.mark_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn mark_additional_young_root_set(&mut self) {
        let mut iterator = REMEMBERED_SET.as_ref().unwrap().iterate();
        while iterator.has_next() {
            let location = iterator.current().get_raw() as *mut Value;
            let value = *location;
            // Check whether the location still refers to young object as this may have changed
            // due to subsequent writes to that location after the write barrier recording.
            if (value.get_raw() as usize) >= self.heap.limits.last_free && value.is_ptr() {
                self.mark_object(value);
            }
            iterator.next();
        }
    }

    unsafe fn mark_object(&mut self, object: Value) {
        let pointer = object.get_ptr() as u32;
        if self.strategy == Strategy::Young && pointer < self.heap.limits.last_free as u32 {
            return;
        }
        debug_assert_eq!(pointer % WORD_SIZE, 0);

        let obj_idx = pointer / WORD_SIZE;
        if get_bit(obj_idx) {
            return;
        }
        set_bit(obj_idx);

        push_mark_stack(self.heap.mem, pointer as usize, object.tag());
        self.marked_space += object_size(pointer as usize).to_bytes().as_usize();
    }

    unsafe fn mark_all_reachable(&mut self) {
        while let Some((obj, tag)) = pop_mark_stack() {
            self.mark_fields(obj as *mut Obj, tag)
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj, object_tag: Tag) {
        visit_pointer_fields(
            self,
            object,
            object_tag,
            self.heap.limits.base,
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                const SLICE_INCREMENT: u32 = 127;
                debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    // push an entire (suffix) array slice
                    push_mark_stack(gc.heap.mem, array as usize, new_start);
                    new_start
                } else {
                    array.len()
                }
            },
        );
    }

    unsafe fn mark_root_mutbox_fields(&mut self, mutbox: *mut MutBox) {
        let field_address = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field_address, self.heap.limits.base) {
            self.mark_object(*field_address);
        }
    }

    unsafe fn should_be_threaded(&self, object: *mut Obj) -> bool {
        let address = object as usize;
        match self.strategy {
            Strategy::Young => address >= self.heap.limits.last_free,
            Strategy::Full => address >= self.heap.limits.base,
        }
    }

    unsafe fn compact_phase(&mut self) {
        if self.is_compaction_beneficial() {
            self.thread_initial_phase();
            self.move_phase();
        }
    }

    fn is_compaction_beneficial(&self) -> bool {
        self.survival_rate() < 0.95
    }

    fn generation_size(&self) -> usize {
        let base = match self.strategy {
            Strategy::Young => self.heap.limits.last_free,
            Strategy::Full => self.heap.limits.base,
        };
        self.heap.limits.free - base
    }

    fn survival_rate(&self) -> f64 {
        self.marked_space as f64 / self.generation_size() as f64
    }

    unsafe fn thread_initial_phase(&mut self) {
        self.thread_all_backward_pointers();

        // For static roots, also forward pointers are threaded.
        // Therefore, this must happen after the heap traversal for backwards pointer threading.
        self.thread_static_roots();

        if (*self.heap.roots.continuation_table_ptr_loc).is_ptr() {
            self.thread(self.heap.roots.continuation_table_ptr_loc);
        }

        // For the young generation GC run, the forward pointers from the old generation must be threaded too.
        if self.strategy == Strategy::Young {
            self.thread_old_generation_pointers();
        }
    }

    unsafe fn thread_static_roots(&self) {
        let root_array = self.heap.roots.static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            debug_assert_eq!(object.tag(), TAG_MUTBOX);
            debug_assert!((object as usize) < self.heap.limits.base);
            self.thread_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn thread_root_mutbox_fields(&self, mutbox: *mut MutBox) {
        let field_addr = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field_addr, self.heap.limits.base) {
            self.thread(field_addr);
        }
    }

    unsafe fn thread_all_backward_pointers(&mut self) {
        let mut bitmap_iter = iter_bits();
        if self.strategy == Strategy::Young {
            bitmap_iter.advance(self.heap.limits.last_free as u32);
        }
        let mut bit = bitmap_iter.next();
        while bit != BITMAP_ITER_END {
            let object = (bit * WORD_SIZE) as *mut Obj;
            self.thread_backward_pointer_fields(object);
            bit = bitmap_iter.next();
        }
    }

    unsafe fn thread_backward_pointer_fields(&mut self, object: *mut Obj) {
        debug_assert!(object.tag() < TAG_ARRAY_SLICE_MIN);
        visit_pointer_fields(
            &mut (),
            object,
            object.tag(),
            self.heap.limits.base,
            |_, field_address| {
                let field_value = *field_address;
                // Thread if backwards or self pointer
                if field_value.get_ptr() <= object as usize {
                    (&self).thread(field_address);
                }
            },
            |_, _, array| array.len(),
        );
    }

    // Thread forward pointers in old generation leading to young generation
    unsafe fn thread_old_generation_pointers(&mut self) {
        let mut iterator = REMEMBERED_SET.as_ref().unwrap().iterate();
        while iterator.has_next() {
            let location = iterator.current().get_raw() as *mut Value;
            let value = *location;
            // value in the location may have changed since recording by the write barrer
            if (value.get_raw() as usize) >= self.heap.limits.last_free && value.is_ptr() {
                debug_assert!((location as usize) >= self.heap.limits.base);
                self.thread(location);
            }
            iterator.next();
        }
    }

    unsafe fn move_phase(&mut self) {
        REMEMBERED_SET = None; // no longer valid when the moving phase starts
        let mut free = self.heap.limits.base;

        let mut bitmap_iter = iter_bits();
        if self.strategy == Strategy::Young {
            bitmap_iter.advance(self.heap.limits.last_free as u32);
            free = self.heap.limits.last_free;
        }
        let mut bit = bitmap_iter.next();
        while bit != BITMAP_ITER_END {
            let old_pointer = (bit * WORD_SIZE) as *mut Obj;
            let new_pointer = free;

            // Update backwards references to the object's new location and restore object header
            self.unthread(old_pointer, new_pointer);

            // Move the object
            let object_size = object_size(old_pointer as usize);
            if new_pointer as usize != old_pointer as usize {
                memcpy_words(new_pointer as usize, old_pointer as usize, object_size);
                debug_assert!(object_size.as_usize() > size_of::<Obj>().as_usize());
            }

            free += object_size.to_bytes().as_usize();

            // Thread forward pointers of the object, even if not moved
            self.thread_forward_pointers(new_pointer as *mut Obj);

            bit = bitmap_iter.next();
        }

        self.heap.limits.free = free;
    }

    /// Thread forward pointers in object
    unsafe fn thread_forward_pointers(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            &mut (),
            object,
            object.tag(),
            self.heap.limits.base,
            |_, field_address| {
                if (*field_address).get_ptr() > object as usize {
                    (&self).thread(field_address)
                }
            },
            |_, _, array| array.len(),
        );
    }

    unsafe fn thread(&self, field: *mut Value) {
        let pointed = (*field).get_ptr() as *mut Obj;
        if self.should_be_threaded(pointed) {
            let pointed_header = pointed.tag();
            *field = Value::from_raw(pointed_header);
            (*pointed).tag = field as u32;
        }
    }

    unsafe fn unthread(&self, object: *mut Obj, new_location: usize) {
        debug_assert!(self.should_be_threaded(object));
        let mut header = object.tag();
        while header & 0b1 == 0 {
            let tmp = (header as *const Obj).tag();
            (*(header as *mut Value)) = Value::from_ptr(new_location);
            header = tmp;
        }
        debug_assert!(header >= TAG_OBJECT && header <= TAG_NULL);
        (*object).tag = header;
    }
}
