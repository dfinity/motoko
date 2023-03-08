//! Incremental old generation garbage collection.
//!
//! This always runs after young generation collection, such that it can ignore generational aspects.
//!
//! New objects are always first allocated in the young generation (to support fast reclamation) and
//! become only visible to the incremental GC after they have been promoted to the old generation (i.e.
//! survived the preceding young generation collection).
//!
//! The incremental GC is based on two phases:
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
//! The GC uses a mark bit in the object header instead of mark bitmaps since there is no performance advantage
//! by using the mark bitmap for skipping garbage objects. This is because the object ids of garbage objects need
//! to be freed in the object table and therefore, the compaction phase must visit all objects (marked and unmarked).

use core::{cell::RefCell, ptr::null_mut};

use crate::{
    constants::WORD_SIZE,
    gc::{
        common::{Limits, Roots},
        incremental::{mark_stack::MarkStack, write_barrier::YOUNG_REMEMBERED_SET},
    },
    mem_utils::memcpy_words,
    memory::Memory,
    types::{block_size, has_object_header, Obj, Tag, Value, Words, TAG_ARRAY_SLICE_MIN},
    visitor::visit_pointer_fields,
};

use super::{array_slicing::slice_array, roots::visit_roots, time::BoundedTime};

#[derive(PartialEq, Clone, Copy)]
pub enum Phase {
    Pause,
    Mark,
    Compact,
    Stop,
}

impl Phase {
    pub fn is_running(self) -> bool {
        self != Self::Pause && self != Self::Stop
    }
}

pub struct State {
    phase: Phase,
    mark_stack: MarkStack,
    mark_complete: bool,
    compact_from: usize,
    compact_to: usize,
}

/// GC state retained over multiple GC increments.
static mut STATE: RefCell<State> = RefCell::new(State {
    phase: Phase::Pause,
    mark_stack: MarkStack::new(),
    mark_complete: false,
    compact_from: 0,
    compact_to: 0,
});

pub struct OldCollection<'a, M: Memory> {
    mem: &'a mut M,
    limits: Limits,
    state: &'a mut State,
    time: BoundedTime,
}

impl<'a, M: Memory> OldCollection<'a, M> {
    /// Each GC schedule point can get a new GC instance that shares the common GC state.
    /// This is because the memory implementation cannot be stored as a global variable.
    /// The young generation collection must have run before the an incremental GC increment.
    pub unsafe fn instance(
        mem: &'a mut M,
        limits: Limits,
        state: &'a mut State,
        time: BoundedTime,
    ) -> OldCollection<'a, M> {
        assert_eq!(limits.young_generation_size(), 0);
        OldCollection {
            mem,
            limits,
            state,
            time,
        }
    }

    /// Special GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase can only be started on an empty call stack.
    /// * New allocations being promoted to the old generation are marked when the GC is running.
    pub unsafe fn empty_call_stack_increment(&mut self, roots: Roots) {
        if self.pausing() {
            self.start_marking(roots);
        }
        self.increment();
        if self.marking_completed() {
            self.start_compacting();
            self.increment();
        }
    }

    unsafe fn pausing(&mut self) -> bool {
        self.state.phase == Phase::Pause
    }

    unsafe fn increment(&mut self) {
        match self.state.phase {
            Phase::Pause | Phase::Stop => {}
            Phase::Mark => self.mark_increment(),
            Phase::Compact => self.compact_increment(),
        }
    }

    /// Only to be called when the call stack is empty as pointers on stack are not collected as roots.
    unsafe fn start_marking(&mut self, roots: Roots) {
        assert!(self.pausing());

        self.state.phase = Phase::Mark;
        self.state.mark_stack.allocate(self.mem);
        self.state.mark_complete = false;
        self.mark_roots(roots);
    }

    fn generation_base(&self) -> usize {
        self.limits.base
    }

    fn generation_end(&self) -> usize {
        assert_eq!(self.limits.young_generation_size(), 0);
        self.limits.free
    }

    fn mark_surviving_objects(&self) -> bool {
        // TODO: Adjust when unified with young generation collection
        false
    }

    unsafe fn mark_roots(&mut self, roots: Roots) {
        visit_roots(roots, self.generation_base(), None, self, |gc, value| {
            gc.mark_object(value);
        });
    }

    pub unsafe fn mark_object(&mut self, value: Value) {
        self.time.tick();
        assert!(!self.state.mark_complete);
        let object = value.get_object_address() as *mut Obj;
        assert!(object as usize >= self.generation_base());
        assert_eq!(object as u32 % WORD_SIZE, 0);
        if object.is_marked() {
            return;
        }
        object.mark();
        self.state.mark_stack.push(self.mem, object);
    }

    unsafe fn mark_increment(&mut self) {
        assert!(!self.state.mark_complete);
        loop {
            let object = self.state.mark_stack.pop();
            if object == null_mut() {
                break;
            }
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
            self.generation_base(),
            |gc, field_address| {
                let field_value = *field_address;
                gc.mark_object(field_value);
            },
            |gc, slice_start, array| {
                let length = slice_array(array);
                if (*array).header.tag >= TAG_ARRAY_SLICE_MIN {
                    gc.state.mark_stack.push(gc.mem, array as *mut Obj);
                }
                gc.time.advance((length - slice_start) as usize);
                length
            },
        );
    }

    unsafe fn complete_marking(&mut self) {
        assert!(!self.state.mark_complete);
        self.state.mark_complete = true;
        self.state.mark_stack.free();
    }

    pub unsafe fn marking_completed(&self) -> bool {
        assert!(!self.state.mark_complete || self.state.mark_stack.is_empty());
        self.state.mark_complete
    }

    pub unsafe fn start_compacting(&mut self) {
        // TODO: Sanity check to test mark completeness against classical blocking marking.
        assert!(self.marking_completed());
        self.state.phase = Phase::Compact;
        self.state.compact_from = self.generation_base();
        self.state.compact_to = self.generation_base();
    }

    unsafe fn compact_increment(&mut self) {
        // Need to visit all objects in the generation, since mark bits may need to be
        // cleared and/or garbage object ids must be freed.
        assert!(YOUNG_REMEMBERED_SET.is_none()); // No longer valid as it will be collected.
        while self.state.compact_from < self.generation_end() && !self.time.is_over() {
            self.time.tick();
            let block = self.state.compact_from as *mut Tag;
            let size = block_size(block as usize);
            if has_object_header(*block) {
                self.compact_object(block as *mut Obj, size);
            }
            self.state.compact_from += size.to_bytes().as_usize();
        }
        self.complete_compacting();
    }

    unsafe fn compact_object(&mut self, object: *mut Obj, size: Words<u32>) {
        let object_id = object.object_id();
        if object.is_marked() {
            if !self.mark_surviving_objects() {
                // If the incremental GC is not active, the object mark must be cleared.
                // Otherwise, the mark bit must stay according to the allocation policy
                // during an active incremental marking or compaction phase.
                object.unmark();
            }
            let old_address = object as usize;
            let new_address = self.state.compact_to;
            if new_address != old_address {
                memcpy_words(new_address, old_address, size);
                object_id.set_new_address(new_address);

                // Determined by measurements in comparison to the mark and compact phases.
                const TIME_FRACTION_PER_WORD: f64 = 2.7;
                self.time
                    .advance((size.as_usize() as f64 / TIME_FRACTION_PER_WORD) as usize);
            }
            self.state.compact_to += size.to_bytes().as_usize();
        } else {
            // Free the id of a garbage object in the object table.
            object_id.free_object_id();
        }
    }

    fn complete_compacting(&mut self) {
        assert_eq!(self.state.compact_from, self.generation_end());
        self.limits.free = self.state.compact_to;
        self.limits.last_free = self.limits.free;
        self.state.phase = Phase::Pause;
    }

    pub fn get_new_limits(&self) -> Limits {
        self.limits
    }
}

pub unsafe fn incremental_gc_state() -> &'static mut State {
    STATE.get_mut()
}

pub unsafe fn incremental_gc_phase() -> Phase {
    incremental_gc_state().phase
}

pub unsafe fn is_incremental_gc_running() -> bool {
    incremental_gc_phase().is_running()
}

#[no_mangle]
pub unsafe extern "C" fn stop_gc_on_upgrade() {
    incremental_gc_state().phase = Phase::Stop;
}
