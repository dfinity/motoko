use crate::{
    gc::common::{Limits, Roots},
    memory::Memory,
};

#[derive(PartialEq)]
enum Phase {
    Pause,
    Marking,
    Compacting,
    Stop,
}

static mut PHASE: Phase = Phase::Pause;

pub unsafe fn is_incremental_gc_running() -> bool {
    PHASE != Phase::Pause && PHASE != Phase::Stop
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
}
