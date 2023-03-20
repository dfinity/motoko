//! Generational compacting GC.
//! Two generations: young and old.
//! Frequent collection of young generation, sporadic full collection (old + young).
//! Young generation collection requires an extra root set of old-to-young pointers.
//! A write barrier catches all pointers leading from old to young generation.
//! Compaction is based on the existing Motoko RTS threaded mark & compact GC.

pub mod mark_stack;
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
use crate::visitor::{points_to_or_beyond, visit_pointer_fields};

use motoko_rts_macros::ic_mem_fn;

use self::mark_stack::{free_mark_stack, pop_mark_stack};
use self::write_barrier::REMEMBERED_SET;

use super::common::Strategy;

#[ic_mem_fn(ic_only)]
unsafe fn initialize_generational_gc<M: Memory>(mem: &mut M, heap_base: u32) {
    crate::memory::ic::initialize_memory(heap_base);
    write_barrier::init_generational_write_barrier(mem);
}

#[ic_mem_fn(ic_only)]
unsafe fn schedule_generational_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::decide_strategy;

    if decide_strategy().is_some() {
        generational_gc(mem);
    }
}

#[ic_mem_fn(ic_only)]
unsafe fn generational_gc<M: Memory>(mem: &mut M) {
    use crate::gc::common::{decide_strategy, update_statistics, update_strategy};
    let old_heap_size = mem.get_heap_pointer();
    let strategy = decide_strategy();

    #[cfg(debug_assertions)]
    let forced_gc = strategy.is_none();

    #[cfg(debug_assertions)]
    sanity_checks::verify_snapshot(mem, false);

    let strategy = strategy.unwrap_or(Strategy::Young);
    let mut gc = GenerationalGC::new(mem, strategy);
    gc.run();

    update_statistics(old_heap_size);
    update_strategy(strategy);

    #[cfg(debug_assertions)]
    if !forced_gc {
        sanity_checks::check_memory(mem);
        sanity_checks::take_snapshot(mem);
    }

    write_barrier::init_generational_write_barrier(mem);
}

pub struct GenerationalGC<'a, M: Memory> {
    mem: &'a mut M,
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
        const BITMAP_ALIGNMENT: usize = 8 * WORD_SIZE as usize;
        let heap_prefix = match self.strategy {
            Strategy::Young => {
                self.mem.get_last_heap_pointer() / BITMAP_ALIGNMENT * BITMAP_ALIGNMENT
            }
            Strategy::Full => self.mem.get_heap_base(),
        };
        let heap_size = Bytes((self.mem.get_heap_pointer() - heap_prefix) as u32);
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
        let roots = self.mem.get_roots();
        self.mark_static_roots(roots.static_roots);

        let continuation_table = *roots.continuation_table_location;
        if continuation_table.is_object_id()
            && continuation_table.get_object_address() >= self.generation_base()
        {
            self.mark_object(continuation_table);
        }

        if self.strategy == Strategy::Young {
            self.mark_additional_young_root_set();
        }
    }

    unsafe fn mark_static_roots(&mut self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            assert_eq!(object.tag(), TAG_MUTBOX);
            assert!((object as usize) < self.mem.get_heap_base());
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
            if value.points_to_or_beyond(self.mem.get_last_heap_pointer()) {
                self.mark_object(value);
            }
            iterator.next();
        }
    }

    unsafe fn mark_object(&mut self, object: Value) {
        let pointer = object.get_object_address() as u32;
        assert!(pointer >= self.generation_base() as u32);
        assert_eq!(pointer % WORD_SIZE, 0);

        let obj_idx = pointer / WORD_SIZE;
        if get_bit(obj_idx) {
            return;
        }
        set_bit(obj_idx);

        push_mark_stack(self.mem, pointer as usize);
        self.marked_space += block_size(pointer as usize).to_bytes().as_usize();
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
                    (*array).header.tag = new_start;
                    push_mark_stack(gc.mem, array as usize);
                    new_start
                } else {
                    // No further visits of this array. Restore the tag.
                    (*array).header.tag = TAG_ARRAY;
                    array.len()
                }
            },
        );
    }

    unsafe fn barrier_coverage_check(&self, field_address: *mut Value) {
        if self.strategy == Strategy::Full
            && (field_address as usize) >= self.mem.get_heap_base()
            && (field_address as usize) < self.mem.get_last_heap_pointer()
            && (*field_address).points_to_or_beyond(self.mem.get_last_heap_pointer())
        {
            assert!(REMEMBERED_SET
                .as_ref()
                .unwrap()
                .contains(Value::from_raw(field_address as u32)));
        }
    }

    unsafe fn mark_root_mutbox_fields(&mut self, mutbox: *mut MutBox) {
        let field_address = &mut (*mutbox).field;
        if points_to_or_beyond(field_address, self.generation_base()) {
            self.mark_object(*field_address);
        }
    }

    unsafe fn compact_phase(&mut self) {
        if self.is_compaction_beneficial() {
            self.thread_initial_phase();
            self.move_phase();
        }
    }

    fn is_compaction_beneficial(&self) -> bool {
        // Returns false if the survival rate is f64::INF for an empty generation.
        const SURVIVAL_THRESHOLD: f64 = 0.95;
        self.survival_rate() < SURVIVAL_THRESHOLD
    }

    fn generation_base(&self) -> usize {
        match self.strategy {
            Strategy::Young => self.mem.get_last_heap_pointer(),
            Strategy::Full => self.mem.get_heap_base(),
        }
    }

    fn generation_size(&self) -> usize {
        self.mem.get_heap_pointer() - self.generation_base()
    }

    fn survival_rate(&self) -> f64 {
        // Returns f64::INF if the generation size is zero, e.g. on forced GC.
        self.marked_space as f64 / self.generation_size() as f64
    }

    unsafe fn thread_initial_phase(&mut self) {
        self.thread_all_backward_pointers();

        let roots = self.mem.get_roots();
        // For static roots, also forward pointers are threaded.
        // Therefore, this must happen after the heap traversal for backwards pointer threading.
        self.thread_static_roots(roots.static_roots);

        let continuation_table = *roots.continuation_table_location;
        if continuation_table.is_object_id()
            && continuation_table.get_object_address() >= self.generation_base()
        {
            self.thread(roots.continuation_table_location);
        }

        // For the young generation GC run, the forward pointers from the old generation must be threaded too.
        if self.strategy == Strategy::Young {
            self.thread_old_generation_pointers();
        }
    }

    unsafe fn thread_static_roots(&self, static_roots: Value) {
        let root_array = static_roots.as_array();
        for i in 0..root_array.len() {
            let object = root_array.get(i).as_obj();
            assert_eq!(object.tag(), TAG_MUTBOX);
            assert!((object as usize) < self.mem.get_heap_base());
            self.thread_root_mutbox_fields(object as *mut MutBox);
        }
    }

    unsafe fn thread_root_mutbox_fields(&self, mutbox: *mut MutBox) {
        let field_addr = &mut (*mutbox).field;
        if points_to_or_beyond(field_addr, self.generation_base()) {
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
                if field_value.get_object_address() <= object as usize {
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
                (location as usize) >= self.mem.get_heap_base()
                    && (location as usize) < self.mem.get_last_heap_pointer()
            );
            let value = *location;
            // value in the location may have changed since recording by the write barrer
            if value.points_to_or_beyond(self.mem.get_last_heap_pointer()) {
                self.thread(location);
            }
            iterator.next();
        }
    }

    unsafe fn move_phase(&mut self) {
        REMEMBERED_SET = None; // no longer valid when the moving phase starts
        let mut free = self.mem.get_heap_base();

        let mut bitmap_iter = iter_bits();
        if self.strategy == Strategy::Young {
            free = self.mem.get_last_heap_pointer();
        }
        let mut bit = bitmap_iter.next();
        while bit != BITMAP_ITER_END {
            let old_pointer = (bit * WORD_SIZE) as *mut Obj;
            let new_pointer = free;

            let new_id = Value::new_object_id(new_pointer);

            // Unthread backwards pointers as well as forward pointers of static objects.
            // In the case of a young collection, also unthread forward pointers of old objects.
            self.unthread(old_pointer, new_id);

            // Move the object
            let object_size = block_size(old_pointer as usize);
            if new_pointer as usize != old_pointer as usize {
                memcpy_words(new_pointer as usize, old_pointer as usize, object_size);
                debug_assert!(object_size.as_usize() > size_of::<Obj>().as_usize());
            }

            free += object_size.to_bytes().as_usize();

            // Thread forward pointers of the object, even if not moved
            self.thread_forward_pointers(new_pointer as *mut Obj);

            bit = bitmap_iter.next();
        }

        self.mem.shrink_heap(free);
    }

    /// Thread forward pointers in object
    unsafe fn thread_forward_pointers(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            &mut (),
            object,
            object.tag(),
            self.generation_base(),
            |_, field_address| {
                if (*field_address).get_object_address() > object as usize {
                    (&self).thread(field_address)
                }
            },
            |_, _, array| array.len(),
        );
    }

    unsafe fn thread(&self, field: *mut Value) {
        let pointed = (*field).get_object_address() as *mut Obj;
        assert!(self.should_be_threaded(pointed));
        let pointed_header = (*pointed).tag;
        *field = Value::from_raw(pointed_header);
        (*pointed).tag = field as u32;
    }

    unsafe fn unthread(&self, object: *mut Obj, new_id: Value) {
        assert!(self.should_be_threaded(object));
        let mut header = (*object).tag;
        while header & 0b1 == 0 {
            let tmp = (*(header as *const Obj)).tag;
            (*(header as *mut Value)) = new_id;
            header = tmp;
        }
        assert!(header >= TAG_OBJECT && header <= TAG_NULL);
        (*object).tag = header;
    }

    unsafe fn should_be_threaded(&self, object: *mut Obj) -> bool {
        object as usize >= self.generation_base()
    }
}
