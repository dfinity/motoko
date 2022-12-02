//! Generational compacting GC.
//! Two generations: young and old.
//! Frequent collection of young generation, sporadic full collection (old + young).
//! Young generation collection requires an extra root set of old-to-young pointers.
//! A write barrier catches all pointers leading from old to young generation.
//! Compaction is based on the existing Motoko RTS threaded mark & compact GC.

pub mod mark_stack;
pub mod remembered_set;
#[cfg(debug_assertions)]
mod sanity_checks;
pub mod write_barrier;

use crate::gc::generational::mark_stack::{alloc_mark_stack, push_mark_stack};
use crate::gc::mark_compact::bitmap::{
    alloc_bitmap, free_bitmap, get_bit, iter_bits, set_bit, BITMAP_ITER_END,
};

use crate::constants::WORD_SIZE;
use crate::mem_utils::memcpy_words;
use crate::memory::Memory;
use crate::types::*;
use crate::visitor::{pointer_to_dynamic_heap, visit_pointer_fields};

use motoko_rts_macros::ic_mem_fn;

use self::mark_stack::{free_mark_stack, pop_mark_stack};
use self::write_barrier::REMEMBERED_SET;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_generational_gc<M: Memory>(mem: &mut M) {
    crate::memory::ic::initialize_memory(true);
    write_barrier::init_post_write_barrier(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_generational_gc<M: Memory>(mem: &mut M) {
    if decide_strategy(mem).is_some() {
        generational_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn generational_gc<M: Memory>(mem: &mut M) {
    #[cfg(debug_assertions)]
    sanity_checks::verify_snapshot(mem, false);

    let old_heap_pointer = mem.heap_pointer() as usize;
    let strategy = decide_strategy(mem);

    #[cfg(debug_assertions)]
    let forced_gc = strategy.is_none();

    let strategy = strategy.unwrap_or(Strategy::Young);
    let mut gc = GenerationalGC::new(mem, strategy);

    gc.run();

    set_limits(mem);
    update_statistics(mem, old_heap_pointer);
    update_strategy(mem, strategy);

    #[cfg(debug_assertions)]
    if !forced_gc {
        sanity_checks::check_memory(mem);
        sanity_checks::take_snapshot(mem);
    }

    write_barrier::init_post_write_barrier(mem);
}

#[cfg(feature = "ic")]
unsafe fn set_limits<M: Memory>(mem: &mut M) {
    let free = mem.heap_pointer();
    mem.set_heap_pointer(free);
    mem.set_last_heap_pointer(free);
}

#[cfg(feature = "ic")]
unsafe fn update_statistics<M: Memory>(mem: &mut M, old_heap_pointer: usize) {
    use crate::memory::ic;
    let live_size = Bytes(mem.heap_pointer() - mem.heap_base());
    ic::MAX_LIVE = ::core::cmp::max(ic::MAX_LIVE, live_size);
    ic::RECLAIMED += Bytes(old_heap_pointer as u64 - mem.heap_pointer() as u64);
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
unsafe fn decide_strategy<M: Memory>(mem: &mut M) -> Option<Strategy> {
    const YOUNG_GENERATION_THRESHOLD: usize = 8 * 1024 * 1024;

    assert!(mem.heap_base() <= mem.last_heap_pointer());
    let old_generation_size = (mem.last_heap_pointer() - mem.heap_base()) as usize;
    assert!(mem.last_heap_pointer() <= mem.heap_pointer());
    let young_generation_size = (mem.heap_pointer() - mem.last_heap_pointer()) as usize;

    if (mem.heap_pointer() as usize) >= CRITICAL_MEMORY_LIMIT && !PASSED_CRITICAL_LIMIT {
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
unsafe fn update_strategy<M: Memory>(mem: &mut M, strategy: Strategy) {
    const GROWTH_RATE: f64 = 2.0;
    if strategy == Strategy::Full {
        OLD_GENERATION_THRESHOLD =
            ((mem.heap_pointer() - mem.heap_base()) as f64 * GROWTH_RATE) as usize;
        if (mem.heap_pointer() as usize) < CRITICAL_MEMORY_LIMIT {
            PASSED_CRITICAL_LIMIT = false
        }
    }
}

pub struct GenerationalGC<'a, M: Memory> {
    pub mem: &'a mut M,
    marked_space: usize,
    strategy: Strategy,
}

impl<'a, M: Memory> GenerationalGC<'a, M> {
    pub fn new(mem: &mut M, strategy: Strategy) -> GenerationalGC<M> {
        GenerationalGC {
            mem,
            marked_space: 0,
            strategy,
        }
    }

    pub unsafe fn run(&mut self) {
        self.alloc_mark_structures();
        self.mark_phase();
        self.compact_phase();
        self.free_mark_structures();
    }

    unsafe fn alloc_mark_structures(&mut self) {
        const BITMAP_ALIGNMENT: u32 = 8 * WORD_SIZE;
        let heap_prefix = match self.strategy {
            Strategy::Young => self.mem.last_heap_pointer() / BITMAP_ALIGNMENT * BITMAP_ALIGNMENT,
            Strategy::Full => self.mem.heap_base(),
        };
        let heap_size = Bytes(self.mem.heap_pointer() - heap_prefix as u32);
        alloc_bitmap(self.mem, heap_size, heap_prefix as u32 / WORD_SIZE);
        alloc_mark_stack(self.mem);
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
        self.mark_continuation_table();
        if self.strategy == Strategy::Young {
            self.mark_additional_young_root_set();
        }
    }

    unsafe fn mark_static_roots(&mut self) {
        let root_array = self.mem.roots().static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            assert_eq!(object.tag(), TAG_MUTBOX);
            assert!((object as usize) < self.mem.heap_base() as usize);
            self.mark_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn mark_continuation_table(&mut self) {
        let continuation_table = *self.mem.roots().continuation_table_address;
        if continuation_table.is_ptr() && continuation_table.get_ptr() >= self.generation_base() {
            self.mark_object(continuation_table);
        }
    }

    unsafe fn mark_additional_young_root_set(&mut self) {
        let mut iterator = REMEMBERED_SET.as_ref().unwrap().iterate();
        while iterator.has_next() {
            let location = iterator.current().get_raw() as *mut Value;
            let value = *location;
            // Check whether the location still refers to young object as this may have changed
            // due to subsequent writes to that location after the write barrier recording.
            if value.points_to_or_beyond(self.mem.last_heap_pointer() as usize) {
                self.mark_object(value);
            }
            iterator.next();
        }
    }

    unsafe fn mark_object(&mut self, object: Value) {
        let pointer = object.get_ptr() as u32;
        assert!(pointer >= self.generation_base() as u32);
        assert_eq!(pointer % WORD_SIZE, 0);

        let obj_idx = pointer / WORD_SIZE;
        if get_bit(obj_idx) {
            return;
        }
        set_bit(obj_idx);

        push_mark_stack(self.mem, pointer as usize);
        self.marked_space += object_size(pointer as usize).to_bytes().as_usize();
    }

    unsafe fn mark_all_reachable(&mut self) {
        while let Some(obj) = pop_mark_stack() {
            self.mark_fields(obj as *mut Obj);
        }
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.generation_base(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);

                // Should become a debug assertion in future.
                gc.barrier_coverage_check(field_address);
            },
            |gc, slice_start, array| {
                const SLICE_INCREMENT: u32 = 255;
                debug_assert!(SLICE_INCREMENT >= TAG_ARRAY_SLICE_MIN);
                if array.len() - slice_start > SLICE_INCREMENT {
                    let new_start = slice_start + SLICE_INCREMENT;
                    // Remember to visit the array suffix later, store the next visit offset in the tag.
                    (array as *mut Obj).initialize_tag(new_start);
                    push_mark_stack(gc.mem, array as usize);
                    new_start
                } else {
                    // No further visits of this array. Restore the tag.
                    (array as *mut Obj).initialize_tag(TAG_ARRAY);
                    array.len()
                }
            },
        );
    }

    unsafe fn barrier_coverage_check(&self, field_address: *mut Value) {
        if self.strategy == Strategy::Full
            && (field_address as usize) >= self.mem.heap_base() as usize
            && (field_address as usize) < self.mem.last_heap_pointer() as usize
            && (*field_address).points_to_or_beyond(self.mem.last_heap_pointer() as usize)
        {
            assert!(REMEMBERED_SET
                .as_ref()
                .unwrap()
                .contains(Value::from_raw(field_address as u32)));
        }
    }

    unsafe fn mark_root_mutbox_fields(&mut self, mutbox: *mut MutBox) {
        let field_address = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field_address, self.generation_base()) {
            self.mark_object(*field_address);
        }
    }

    unsafe fn compact_phase(&mut self) {
        if self.is_compaction_beneficial() {
            self.thread_initial_phase();
            self.move_phase();
        }
    }

    unsafe fn is_compaction_beneficial(&self) -> bool {
        // Returns false if the survival rate is f64::INF for an empty generation.
        const SURVIVAL_THRESHOLD: f64 = 0.95;
        self.survival_rate() < SURVIVAL_THRESHOLD
    }

    unsafe fn generation_base(&self) -> usize {
        match self.strategy {
            Strategy::Young => self.mem.last_heap_pointer() as usize,
            Strategy::Full => self.mem.heap_base() as usize,
        }
    }

    unsafe fn generation_size(&self) -> usize {
        self.mem.heap_pointer() as usize - self.generation_base()
    }

    unsafe fn survival_rate(&self) -> f64 {
        // Returns f64::INF if the generation size is zero, e.g. on forced GC.
        self.marked_space as f64 / self.generation_size() as f64
    }

    unsafe fn thread_initial_phase(&mut self) {
        self.thread_all_backward_pointers();

        // For static roots, also forward pointers are threaded.
        // Therefore, this must happen after the heap traversal for backwards pointer threading.
        self.thread_static_roots();

        self.thread_continuation_table();

        // For the young generation GC run, the forward pointers from the old generation must be threaded too.
        if self.strategy == Strategy::Young {
            self.thread_old_generation_pointers();
        }
    }

    unsafe fn thread_static_roots(&self) {
        let root_array = self.mem.roots().static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            assert_eq!(object.tag(), TAG_MUTBOX);
            assert!((object as usize) < self.mem.heap_base() as usize);
            self.thread_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn thread_continuation_table(&self) {
        let continuation_table = *self.mem.roots().continuation_table_address;
        if continuation_table.is_ptr() && continuation_table.get_ptr() >= self.generation_base() {
            self.thread(self.mem.roots().continuation_table_address);
        }
    }

    unsafe fn thread_root_mutbox_fields(&self, mutbox: *mut MutBox) {
        let field_addr = &mut (*mutbox).field;
        if pointer_to_dynamic_heap(field_addr, self.generation_base()) {
            self.thread(field_addr);
        }
    }

    unsafe fn thread_all_backward_pointers(&mut self) {
        let mut bitmap_iter = iter_bits();
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
            self.generation_base(),
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
            assert!(
                (location as usize) >= self.mem.heap_base() as usize
                    && (location as usize) < self.mem.last_heap_pointer() as usize
            );
            let value = *location;
            // value in the location may have changed since recording by the write barrer
            if value.points_to_or_beyond(self.mem.last_heap_pointer() as usize) {
                self.thread(location);
            }
            iterator.next();
        }
    }

    unsafe fn move_phase(&mut self) {
        REMEMBERED_SET = None; // no longer valid when the moving phase starts
        let mut free = self.mem.heap_base() as usize;

        let mut bitmap_iter = iter_bits();
        if self.strategy == Strategy::Young {
            free = self.mem.last_heap_pointer() as usize;
        }
        let mut bit = bitmap_iter.next();
        while bit != BITMAP_ITER_END {
            let old_pointer = (bit * WORD_SIZE) as *mut Obj;
            let new_pointer = free;

            // Unthread backwards pointers as well as forward pointers of static objects.
            // In the case of a young collection, also unthread forward pointers of old objects.
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

        self.mem.set_heap_pointer(free as u32);
    }

    /// Thread forward pointers in object
    unsafe fn thread_forward_pointers(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            &mut (),
            object,
            object.tag(),
            self.generation_base(),
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
        assert!(self.should_be_threaded(pointed));
        let pointed_header = (*pointed).raw_tag;
        *field = Value::from_raw(pointed_header);
        (*pointed).raw_tag = field as u32;
    }

    unsafe fn unthread(&self, object: *mut Obj, new_location: usize) {
        assert!(self.should_be_threaded(object));
        let mut header = (*object).raw_tag;
        while header & 0b1 == 0 {
            let tmp = (*(header as *const Obj)).raw_tag;
            (*(header as *mut Value)) = Value::from_ptr(new_location);
            header = tmp;
        }
        assert!(header >= TAG_OBJECT && header <= TAG_NULL);
        (*object).raw_tag = header;
    }

    unsafe fn should_be_threaded(&self, object: *mut Obj) -> bool {
        object as usize >= self.generation_base()
    }
}
