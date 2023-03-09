use core::cell::RefCell;

use super::mark_stack::MarkStack;

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
    pub phase: Phase,
    pub mark_stack: MarkStack,
    pub mark_complete: bool,
    pub compact_from: usize,
    pub compact_to: usize,
}

impl State {
    pub const fn new() -> State {
        State {
            phase: Phase::Pause,
            mark_stack: MarkStack::new(),
            mark_complete: false,
            compact_from: 0,
            compact_to: 0,
        }
    }
}

/// Old generation GC state retained over multiple GC increments.
static mut INCREMENTAL_COLLECTION_STATE: RefCell<State> = RefCell::new(State {
    phase: Phase::Pause,
    mark_stack: MarkStack::new(),
    mark_complete: false,
    compact_from: 0,
    compact_to: 0,
});

pub unsafe fn incremental_gc_state() -> &'static mut State {
    INCREMENTAL_COLLECTION_STATE.get_mut()
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
