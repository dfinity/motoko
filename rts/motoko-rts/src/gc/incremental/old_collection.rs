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
