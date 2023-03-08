//! Write barrier, used by the incremental GC

use crate::gc::common::Limits;
use crate::memory::Memory;
use crate::remembered_set::RememberedSet;
use crate::types::{is_skewed, Value};
use motoko_rts_macros::ic_mem_fn;

use super::old_collection::{incremental_gc_phase, incremental_gc_state, OldCollection, Phase};
use super::time::BoundedTime;

pub static mut YOUNG_REMEMBERED_SET: Option<RememberedSet> = None;

// TODO: Adjust also this `HEAP_BASE` when object table grows/shrinks! Better incorporate into mem.
// TODO: Remove these redundancies by offering this information via `mem`
pub static mut HEAP_BASE: u32 = 0;
pub static mut LAST_HP: u32 = 0;

/// Activate the write barrier for the incremental GC.
#[cfg(feature = "ic")]
pub(super) unsafe fn init_incremental_write_barrier<M: Memory>(mem: &mut M) {
    use crate::memory::ic;
    HEAP_BASE = ic::HEAP_BASE;
    create_young_remembered_set(mem, ic::LAST_HP as usize);
}

/// Create a new young remembered set after a young generation collection.
pub(super) unsafe fn create_young_remembered_set<M: Memory>(mem: &mut M, last_hp: usize) {
    assert!(YOUNG_REMEMBERED_SET.is_none());
    YOUNG_REMEMBERED_SET = Some(RememberedSet::new(mem));
    LAST_HP = last_hp as u32;
}

/// Write a potential pointer value with with a pre- and post-update barrier used by the incremental GC.
/// `location` (unskewed) denotes the field or array element where the value is to be written to.
/// `value` (skewed if an object id) denotes the value that is to be written to the location.
/// The barrier can be conservatively called even if the location does not store an object id or
/// the new value is not an object id.
///
/// Barrier effects:
/// * Pre update: Used during the GC mark phase to guarantee incremental snapshot-at-the-beginning marking.
/// * Post update: Used for the generational collection to record the old-to-young remembered set.
#[ic_mem_fn]
pub unsafe fn write_with_barrier<M: Memory>(mem: &mut M, location: *mut Value, new_value: Value) {
    debug_assert!(!is_skewed(location as u32));

    let old_value = *location;
    pre_update_barrier(mem, old_value);
    *location = new_value;
    post_update_barrier(mem, location);
}

/// Ensure snapshot-at-the-beginning consistency during the incremental mark phase.
/// Catch overwritten object ids and mark the corresponding objects when the GC is in the mark phase.
unsafe fn pre_update_barrier<M: Memory>(mem: &mut M, value: Value) {
    if incremental_gc_phase() == Phase::Mark && value.points_to_or_beyond(HEAP_BASE as usize) {
        let limits = Limits {
            base: HEAP_BASE as usize,
            last_free: LAST_HP as usize,
            free: LAST_HP as usize, // TODO: Remove this provisional solution by offering this information via `mem`.
        };
        let state = incremental_gc_state();
        let time = BoundedTime::new(0);
        let mut increment = OldCollection::instance(mem, limits, state, time);
        increment.mark_object(value);
    }
}

/// Catch object id writes that lead from the old generation to the young generation and store the corresponding
/// write location in the remembered set as additional root set for the young generation collection.
unsafe fn post_update_barrier<M: Memory>(mem: &mut M, location: *mut Value) {
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if (location as u32) < LAST_HP {
        let value = *location;
        if value.points_to_or_beyond(LAST_HP as usize) {
            if location as u32 >= HEAP_BASE {
                // Catch object ids that point from old generation (or static roots) to young generation.
                // Note: We could also only record the target value, as no threading is performed.
                // However, the location can be overwritten, such that the target object may still become garbage.
                // Therefore, this is an optimization that allows objects to be collected even if they have only
                // been temporarily referenced from the old generation.
                YOUNG_REMEMBERED_SET
                    .as_mut()
                    .unwrap()
                    .insert(mem, Value::from_raw(location as u32));
            }
        }
    }
}
