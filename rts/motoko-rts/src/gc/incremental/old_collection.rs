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
//! Since the call stack cannot be scanned for object ids (root set), the following restrictions apply:
//! * During mark phase, new allocations to the old generations (promotions from young generation) need to be marked.
//! * The mark phase must only start on an empty call stack.
//! Anyway, GC increments are currently only scheduled on empty call stack.

use crate::{
    gc::common::{Limits, Roots},
    memory::Memory,
    types::Value,
};

#[derive(PartialEq, Clone, Copy)]
pub enum Phase {
    Pause,
    Mark,
    Compact,
    Stop,
}

static mut PHASE: Phase = Phase::Pause;

pub unsafe fn is_incremental_gc_running() -> bool {
    PHASE != Phase::Pause && PHASE != Phase::Stop
}

pub unsafe fn get_phase() -> Phase {
    PHASE
}

pub struct OldCollection<'a, M: Memory> {
    mem: &'a mut M,
    limits: Limits,
    roots: Roots,
}

impl<'a, M: Memory> OldCollection<'a, M> {
    pub fn new(mem: &'a mut M, limits: Limits, roots: Roots) -> OldCollection<'a, M> {
        OldCollection { mem, limits, roots }
    }

    pub fn run(&mut self) {}

    pub fn get_new_limits(&self) -> Limits {
        self.limits
    }

    pub fn mark_object(_value: Value) {}
}

#[no_mangle]
pub unsafe extern "C" fn stop_gc_on_upgrade() {
    PHASE = Phase::Stop;
}
