//! Write barrier, used by the incremental GC

use crate::memory::Memory;
use crate::remembered_set::RememberedSet;
use crate::types::{is_skewed, Value, OBJECT_TABLE};
use motoko_rts_macros::ic_mem_fn;

use super::mark_old_object;
use super::state::{incremental_gc_phase, Phase};

/// Records the ids of objects that serve as additional root set for the young
/// generation collection. It contains ids of objects living in the young generation
/// with the following properties:
/// * The object is or has been referenced from the old generation as detected by
///   the post-update write barrier.
/// * The object has been moved from the old generation to the young generation due
///   to object table growth.
static mut YOUNG_REMEMBERED_SET: Option<RememberedSet> = None;

/// Activate the write barrier for the incremental GC.
#[cfg(feature = "ic")]
pub(super) unsafe fn init_incremental_write_barrier<M: Memory>(mem: &mut M) {
    create_young_remembered_set(mem);
}

/// Take the young remembered set for young generation collection.
/// A new young remembered set needs to be created after completed GC work.
pub unsafe fn take_young_remembered_set() -> RememberedSet {
    YOUNG_REMEMBERED_SET.take().unwrap()
}

/// Object table extension may insert additional objects if they are moved from the old
/// generation to the young generation. No GC increment is running at that time, such that
/// the remembered set is present.
/// The insertion requires that the object table has sufficient free object ids, as the
/// table is not allowed to grow during this call.
pub(super) unsafe fn add_to_young_remembered_set<M: Memory>(mem: &mut M, value: Value) {
    YOUNG_REMEMBERED_SET.as_mut().unwrap().insert(mem, value);
}

/// Create a new young remembered set after any of these events:
/// * A young-only generation collection (without a subsequent old generation collection).
/// * An old generation GC increment (that was run after a young generation collection).
pub unsafe fn create_young_remembered_set<M: Memory>(mem: &mut M) {
    // Set the last heap pointer to current heap pointer when reinitializing the remembererd set.
    mem.shrink_heap(mem.get_heap_pointer());
    debug_assert!(YOUNG_REMEMBERED_SET.is_none());
    YOUNG_REMEMBERED_SET = Some(RememberedSet::new(mem));
    debug_assert!(mem.get_last_heap_pointer() < mem.get_heap_pointer());
}

pub unsafe fn using_incremental_barrier() -> bool {
    debug_assert!(YOUNG_REMEMBERED_SET.is_some() || incremental_gc_phase() == Phase::Pause);
    YOUNG_REMEMBERED_SET.is_some()
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
/// Catch overwritten object ids and mark the corresponding objects if the two conditions are met:
/// * The GC is in the mark phase.
/// * The corresponding object resides in the old generation.
unsafe fn pre_update_barrier<M: Memory>(mem: &mut M, value: Value) {
    if incremental_gc_phase() == Phase::Mark
        && value.points_to_or_beyond(mem.get_heap_base())
        && value.get_object_address() < mem.get_last_heap_pointer()
    {
        mark_old_object(mem, value);
    }
}

/// Catch object id writes that lead from the old generation to the young generation and store the corresponding
/// write location in the remembered set as additional root set for the young generation collection.
unsafe fn post_update_barrier<M: Memory>(mem: &mut M, location: *mut Value) {
    // Checks have been optimized according to the frequency of occurrence.
    // Only record locations inside old generation. Static roots are anyway marked by GC.
    if (location as usize) < mem.get_last_heap_pointer() {
        let value = *location;
        if value.points_to_or_beyond(mem.get_last_heap_pointer()) {
            if location as usize >= mem.get_heap_base() {
                let remembered_set = YOUNG_REMEMBERED_SET.as_mut().unwrap();
                // Conservatively reserve sufficient free ids in the object table for a potential hash table growth
                // of the remembered set.
                OBJECT_TABLE
                    .as_mut()
                    .unwrap()
                    .reserve(mem, remembered_set.count() as usize + 1);
                // Catch object ids that point from old generation (or static roots) to young generation.
                remembered_set.insert(mem, value);
            }
        }
    }
}
