use motoko_rts::gc::incremental::state::{
    incremental_gc_state, is_incremental_gc_running, stop_gc_on_upgrade, Phase, State,
};

pub unsafe fn test() {
    println!("  Testing state...");

    test_phases();
    test_state_changes();
}

fn test_phases() {
    assert!(!Phase::Pause.is_running());
    assert!(Phase::Mark.is_running());
    assert!(Phase::Compact.is_running());
    assert!(!Phase::Stop.is_running());
}

unsafe fn test_state_changes() {
    assert!(!is_incremental_gc_running());
    let state = incremental_gc_state();
    check_initial_state(&state);

    stop_gc_on_upgrade();
    assert!(state.phase == Phase::Stop);
    assert!(!is_incremental_gc_running());

    state.phase = Phase::Mark;
    assert!(is_incremental_gc_running());

    state.phase = Phase::Compact;
    assert!(is_incremental_gc_running());

    *state = State::new();
    check_initial_state(&state);
    assert!(!is_incremental_gc_running());
}

fn check_initial_state(state: &State) {
    assert!(state.phase == Phase::Pause);
    assert!(!state.mark_stack.is_allocated());
    assert!(!state.mark_complete);
    assert_eq!(state.compact_from, 0);
    assert_eq!(state.compact_to, 0);
}
