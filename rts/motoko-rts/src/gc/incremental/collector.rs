use core::ptr::null_mut;

use crate::{
    constants::WORD_SIZE,
    gc::{
        common::{Limits, Roots},
        incremental::state::Phase,
    },
    mem_utils::memcpy_words,
    memory::Memory,
    remembered_set::RememberedSet,
    types::{block_size, has_object_header, Obj, Tag, Value, Words, TAG_ARRAY_SLICE_MIN},
    visitor::visit_pointer_fields,
};

use super::{array_slicing::slice_array, roots::visit_roots, state::State, time::Time};

pub struct Generation {
    start: usize,
    end: usize,
    remembered_set: Option<RememberedSet>,
    mark_surviving: bool,
}

impl Generation {
    pub fn new(
        start: usize,
        end: usize,
        remembered_set: Option<RememberedSet>,
        mark_surviving: bool,
    ) -> Generation {
        Generation {
            start,
            end,
            remembered_set,
            mark_surviving,
        }
    }

    pub fn young(limits: Limits, remembered_set: RememberedSet, mark_promoted: bool) -> Generation {
        assert!(limits.last_free <= limits.free);
        Self::new(
            limits.last_free,
            limits.free,
            Some(remembered_set),
            mark_promoted,
        )
    }

    pub fn old(limits: Limits) -> Generation {
        assert_eq!(limits.last_free, limits.free);
        assert!(limits.base <= limits.free);
        Self::new(limits.base, limits.free, None, false)
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

    /// GC increment invoked when the call stack is guaranteed to be empty.
    /// As the GC cannot scan or use write barriers on the call stack, we need to ensure:
    /// * The mark phase can only be started on an empty call stack.
    /// * New allocations being promoted to the old generation are marked when the
    ///   incremental collection of the old generation is active.
    pub unsafe fn run(&mut self, roots: Roots) {
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

    unsafe fn mark_roots(&mut self, roots: Roots) {
        // For the young generation, the remembered set is only used for marking.
        // It will also be collected during the compacting phase.
        let remembered_set = self.generation.remembered_set.take();
        visit_roots(
            roots,
            self.generation.start,
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
        assert!(self.state.phase == Phase::Mark);
        assert!(!self.state.mark_complete);
        let object = value.get_object_address() as *mut Obj;
        self.time.tick();
        assert!(object as usize >= self.generation.start);
        assert!((object as usize) < self.generation.end);
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
            self.generation.start,
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
        assert!(self.state.phase == Phase::Mark);
        assert!(!self.state.mark_complete);
        self.state.mark_complete = true;
        self.state.mark_stack.free();
    }

    unsafe fn marking_completed(&self) -> bool {
        assert!(!self.state.mark_complete || self.state.mark_stack.is_empty());
        self.state.mark_complete
    }

    unsafe fn start_compacting(&mut self) {
        // TODO: Sanity check to test mark completeness against classical blocking marking.
        assert!(self.state.phase == Phase::Mark);
        assert!(self.marking_completed());
        self.state.phase = Phase::Compact;
        self.state.compact_from = self.generation.start;
        self.state.compact_to = self.state.compact_from;
    }

    unsafe fn compact_increment(&mut self) {
        assert!(self.state.phase == Phase::Compact);
        assert!(self.generation.remembered_set.is_none()); // No longer valid as it will be collected.
                                                           // Need to visit all objects in the generation, since mark bits may need to be
                                                           // cleared and/or garbage object ids must be freed.
        while self.state.compact_from < self.generation.end && !self.time.is_over() {
            let block = self.state.compact_from as *mut Tag;
            let size = block_size(block as usize);
            if has_object_header(*block) {
                self.compact_object(block as *mut Obj, size);
            }
            self.state.compact_from += size.to_bytes().as_usize();
            assert_eq!(self.state.compact_from % WORD_SIZE as usize, 0);
            assert!(self.state.compact_from <= self.generation.end);
            self.time.tick();
        }
        self.complete_compacting();
    }

    unsafe fn compact_object(&mut self, object: *mut Obj, size: Words<u32>) {
        let object_id = object.object_id();
        if object.is_marked() {
            // If objects are promoted from the young generation to the old generation
            // and the incremental old generation collection is active (not pausing),
            // the object remains marked. Otherwise, the mark bit must be cleared.
            if !self.generation.mark_surviving {
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
            assert!(self.state.compact_to <= self.state.compact_from);
            assert_eq!(self.state.compact_to % WORD_SIZE as usize, 0);
        } else {
            // Free the id of a garbage object in the object table.
            object_id.free_object_id();
        }
    }

    fn complete_compacting(&mut self) {
        assert_eq!(self.state.compact_from, self.generation.end);
        assert!(self.state.compact_from <= self.state.compact_to);
        assert!(self.generation.start <= self.state.compact_to);
        self.generation.end = self.state.compact_to;
        self.state.phase = Phase::Pause;
    }

    /// Get the new heap end (free pointer) after a GC increment or a full GC run.
    pub fn get_heap_end(&self) -> usize {
        self.generation.end
    }
}
