//! Garbage collector for young and old generation used by the incremental GC.
//!
//! Young generation collection (blocking):
//! --------------------------------------
//!
//! The young generation collection runs non-incrementally, blocking the mutator, by using `Time::unlimited()`.
//!
//! It is scheduled:
//! * Before each GC increment of the old generation, for simplifying incremental collection.
//! * After a certain amount of new allocations, i.e. when young generation has exceeded a threshold.
//!
//! Young generation collection aims at fast reclamation of short-lived objects, to reduce GC latency,
//! which is particularly relevant in an incremental GC. Young generation collection blocks the mutator,
//! i.e. does not run incrementally.
//!
//! The young generation collection requires an extra root set of old-to-young pointers. Those pointers
//! are caught by a write barrier and recorded in a remembered set. The remembered set lives in the young
//! generation and is freed during the young generation collection.
//!
//! The compaction phase can use the simple object movement enabled by the central object table provided
//! for the incremental GC.
//!
//! Old generation collection (incremental):
//! ---------------------------------------
//!
//! Old generation collection runs incrementally in multiple GC increments, using `Time::limited()`.
//! It is scheduled to run after young generation collection, such that it can ignore generational aspects.
//!
//! New objects are always first allocated in the young generation and become only visible to the incremental
//! GC after they have been promoted to the old generation (i.e. survived the preceding young generation collection).
//!
//! The incremental collection of the old generation is based on two phases:
//! 1. Mark: Incremental snapshot-at-the-beginning marking. The GC marks at least all objects that have been
//!   reachable at the start time of the incremental GC run. A pre-update write-barrier detects relevant
//!   concurrent pointer overwrites by the mutator between GC increments and marks the corresponding objects
//!   to guarantee snapshot-at-the-beginning consistency. Concurrent allocations to the old generation (being
//!   promotions from the young generation) are conservatively marked.
//! 2. Compact: Incremental compaction supported by a central object table. After the mark phase, the marked
//!   objects of the old generation are compacted towards the bottom of the generation. Objects can be easily moved
//!   one-by-one since all incoming references to a moved object can be atomically updated in the central
//!   object table. Concurrent allocations to the old generation during this phase (promotions from the young
//!   generations) are marked because they have to be retained and compacted during this incremental GC run.
//!  
//! Since the call stack cannot be accessed to collect roots on it, the following restrictions apply:
//! * During mark phase, new allocations to the old generations (promotions from young generation) need to be marked.
//! * The mark phase must only start on an empty call stack.
//! Anyway, GC increments are currently only scheduled on empty call stack.
//!
//! New allocation marking policy:
//! ------------------------------
//! * New objects are allocated in the young generation and not marked (to allow fast reclamation).
//! * When young objects are promoted to the old generation, they are marked if and only if the
//!   incremental GC of the old generation is active (i.e. is in mark or compact phase).
//!
//! Mark bit:
//! ---------
//!
//! The GC uses a mark bit in the object header instead of mark bitmaps since there is no performance advantage
//! by using the mark bitmap for skipping garbage objects. This is because the object ids of garbage objects need
//! to be freed in the object table and therefore, the compaction phase must visit all objects (marked and unmarked).
//!
//! Mark stack:
//! -----------
//!
//! The mark stack is allocated in the generation where it is used, i.e. young generation collection allocates
//! the mark stack tables inside the young generation, while mark stack tables of the old generation are added to
//! the old generation. Therefore, a generation may grow during the mark phase. As for the old generation, the
//! allocated mark stack tables need to be retained for subsequent mark increments. Mark stack tables remain
//! unmarked, such that they will be collected as garbage during the compaction of the corresponding generation.

use core::cmp::max;

use crate::{
    constants::WORD_SIZE,
    gc::incremental::state::incremental_gc_phase,
    mem_utils::memcpy_words,
    memory::Memory,
    remembered_set::RememberedSet,
    types::{
        block_size, has_object_header, Obj, Tag, Value, Words, NULL_OBJECT_ID, TAG_ARRAY_SLICE_MIN,
    },
    visitor::visit_pointer_fields,
};

use super::{
    array_slicing::slice_array, mark_old_object, roots::visit_roots, state::Phase, state::State,
    time::Time,
};

#[cfg(debug_assertions)]
use super::sanity_checks::{check_heap, check_mark_completion};

pub struct Generation {
    is_young: bool,
    remembered_set: Option<RememberedSet>,
    promote_surviving: bool,
}

impl Generation {
    pub fn new(
        is_young: bool,
        remembered_set: Option<RememberedSet>,
        promote_surviving: bool,
    ) -> Generation {
        Generation {
            is_young,
            remembered_set,
            promote_surviving,
        }
    }

    pub fn young<M: Memory>(
        mem: &mut M,
        remembered_set: RememberedSet,
        mark_promoted: bool,
    ) -> Generation {
        debug_assert!(mem.get_heap_base() <= mem.get_last_heap_pointer());
        debug_assert!(mem.get_last_heap_pointer() <= mem.get_heap_pointer());
        Self::new(true, Some(remembered_set), mark_promoted)
    }

    pub fn old<M: Memory>(mem: &mut M) -> Generation {
        debug_assert_eq!(mem.get_last_heap_pointer(), mem.get_heap_pointer());
        debug_assert!(mem.get_heap_base() <= mem.get_heap_pointer());
        Self::new(false, None, false)
    }
}

pub struct GarbageCollector<'a, M: Memory> {
    mem: &'a mut M,
    generation: Generation,
    state: &'a mut State,
    time: Time,
}

impl<'a, M: Memory> GarbageCollector<'a, M> {
    /// Each GC schedule point can get a new GC instance that shares the common GC state.
    /// This is because the memory implementation cannot be stored as a global variable.
    /// The young generation collection must have run before the an incremental GC increment.
    pub unsafe fn instance(
        mem: &'a mut M,
        generation: Generation,
        state: &'a mut State,
        time: Time,
    ) -> GarbageCollector<'a, M> {
        GarbageCollector {
            mem,
            generation,
            state,
            time,
        }
    }

    /// The start address of the currently collected generation may increase if the
    /// object table is extended during garbage collection. This can happen because
    /// of concurrent mutator allocations during an incremental collection
    /// or during the mark phase when the object table is full on the allocation of
    /// mark stack tables.
    fn generation_start(&self) -> usize {
        if self.generation.is_young {
            self.mem.get_last_heap_pointer()
        } else {
            self.mem.get_heap_base()
        }
    }

    /// GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase can only be started on an empty call stack.
    /// * New allocations being promoted to the old generation are marked when the
    ///   incremental collection of the old generation is active.
    pub unsafe fn run(&mut self) {
        if self.state.phase == Phase::Pause {
            self.start_marking();
        }
        if self.state.phase == Phase::Mark {
            self.mark_increment();
        }
        if self.marking_completed() {
            self.start_compacting();
        }
        if self.state.phase == Phase::Compact {
            self.compact_increment();
        }
    }

    /// Only to be called when the call stack is empty as pointers on stack are not collected as roots.
    unsafe fn start_marking(&mut self) {
        debug_assert!(self.state.phase == Phase::Pause);

        self.state.phase = Phase::Mark;
        self.state.mark_stack.allocate(self.mem);
        self.state.mark_complete = false;
        self.mark_roots();
    }

    unsafe fn mark_roots(&mut self) {
        // For the young generation, the remembered set is only used for marking.
        // It will also be collected during the compacting phase.
        let general_roots = self.mem.get_roots();
        let remembered_set = self.generation.remembered_set.take();
        visit_roots(
            general_roots,
            self.generation_start(),
            remembered_set.as_ref(),
            self,
            |gc, value| {
                gc.mark_object(value);
            },
        );
    }

    /// Mark the corresponding object if not yet marked before.
    /// The reachable object traversal is performed separatedly by `run` during
    /// the mark phase.
    pub unsafe fn mark_object(&mut self, value: Value) {
        debug_assert!(self.state.phase == Phase::Mark);
        debug_assert!(!self.state.mark_complete);
        let object = value.get_object_address() as *mut Obj;
        self.time.tick();
        debug_assert!(object as usize >= self.generation_start());
        debug_assert!((object as usize) < self.mem.get_heap_pointer());
        debug_assert_eq!(object as u32 % WORD_SIZE, 0);
        if object.is_marked() {
            return;
        }
        object.mark();
        self.state.mark_stack.push(self.mem, value);
        // The generation may have been extended because of additional mark stack tables.
    }

    unsafe fn mark_increment(&mut self) {
        debug_assert!(self.state.phase == Phase::Mark);
        debug_assert!(!self.state.mark_complete);
        loop {
            let value = self.state.mark_stack.pop();
            if value == NULL_OBJECT_ID {
                break;
            }
            let object = value.get_object_address() as *mut Obj;
            self.mark_fields(object);

            self.time.tick();
            if self.time.is_over() {
                return;
            }
        }
        self.complete_marking();
    }

    unsafe fn mark_fields(&mut self, object: *mut Obj) {
        visit_pointer_fields(
            self,
            object,
            object.tag(),
            self.generation_start(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                let length = slice_array(array);
                if (*array).header.tag >= TAG_ARRAY_SLICE_MIN {
                    let value = (array as *mut Obj).object_id();
                    gc.state.mark_stack.push(gc.mem, value);
                    // The generation may have been extended because of additional mark stack tables.
                }
                gc.time.advance((length - slice_start) as usize);
                length
            },
        );
    }

    unsafe fn complete_marking(&mut self) {
        debug_assert!(self.state.phase == Phase::Mark);
        debug_assert!(!self.state.mark_complete);
        debug_assert!(self.state.mark_stack.is_empty());
        self.state.mark_complete = true;
        self.state.mark_stack.free();

        #[cfg(debug_assertions)]
        check_mark_completion(self.mem, self.generation_start());

        #[cfg(debug_assertions)]
        check_heap(self.mem, true, self.generation_start());
    }

    unsafe fn marking_completed(&self) -> bool {
        self.state.phase == Phase::Mark && self.state.mark_complete
    }

    unsafe fn start_compacting(&mut self) {
        debug_assert!(self.state.phase == Phase::Mark);
        debug_assert!(self.marking_completed());
        debug_assert!(!self.state.mark_stack.is_allocated());

        self.state.phase = Phase::Compact;
        self.state.compact_from = self.generation_start();
        self.state.compact_to = self.state.compact_from;
    }

    unsafe fn compact_increment(&mut self) {
        debug_assert!(self.state.phase == Phase::Compact);
        // The remembered set is no longer valid as it will be freed during compaction.
        debug_assert!(self.generation.remembered_set.is_none());
        // Object table extension during incremental compaction may have shifted
        // the heap base and thus the generation start.
        self.state.compact_from = max(self.state.compact_from, self.generation_start());
        self.state.compact_to = max(self.state.compact_from, self.generation_start());
        // Need to visit all objects in the generation, since mark bits may need to be
        // cleared and/or garbage object ids must be freed.
        while self.state.compact_from < self.mem.get_heap_pointer() {
            let block = self.state.compact_from as *mut Tag;
            let size = block_size(block as usize);
            if has_object_header(*block) {
                self.compact_object(block as *mut Obj, size);
            }
            self.state.compact_from += size.to_bytes().as_usize();
            debug_assert_eq!(self.state.compact_from % WORD_SIZE as usize, 0);
            debug_assert!(self.state.compact_from <= self.mem.get_heap_pointer());
            self.time.tick();
            if self.time.is_over() {
                return;
            }
        }
        self.complete_compacting();
    }

    unsafe fn compact_object(&mut self, object: *mut Obj, size: Words<u32>) {
        let object_id = object.object_id();
        if object.is_marked() {
            // Clear the mark bit.
            object.unmark();
            if self.generation.promote_surviving {
                // If the object needs to be promoted, it may be marked again but possibly
                // also added to the mark stack depending on the incremental GC phase.
                // Therefore, the mark bit has been first cleared before promotion.
                self.promote_object(object_id);
            }
            let old_address = object as usize;
            let new_address = self.state.compact_to;
            debug_assert!(new_address <= old_address);
            if new_address != old_address {
                memcpy_words(new_address, old_address, size);
                object_id.set_new_address(new_address);

                // Determined by measurements in comparison to the mark and compact phases.
                const TIME_FRACTION_PER_WORD: f64 = 2.7;
                self.time
                    .advance((size.as_usize() as f64 / TIME_FRACTION_PER_WORD) as usize);
            }
            self.state.compact_to += size.to_bytes().as_usize();
            debug_assert_eq!(self.state.compact_to % WORD_SIZE as usize, 0);
        } else {
            // Free the id of a garbage object in the object table.
            object_id.free_object_id();
        }
    }

    unsafe fn promote_object(&mut self, object_id: Value) {
        if incremental_gc_phase() == Phase::Mark {
            mark_old_object(self.mem, object_id);
        } else {
            object_id.as_obj().mark();
        }
    }

    unsafe fn complete_compacting(&mut self) {
        debug_assert_eq!(self.state.compact_from, self.mem.get_heap_pointer());
        debug_assert!(self.state.compact_to <= self.state.compact_from);
        debug_assert!(self.generation_start() <= self.state.compact_to);
        self.mem.shrink_heap(self.state.compact_to);
        self.state.phase = Phase::Pause;

        #[cfg(debug_assertions)]
        check_heap(self.mem, false, self.generation_start());
    }
}
