use crate::types::Value;

/// Check whether the existing persistent stable type is compatible to the new stable type.
/// Traps if the check fails.
/// Both arguments point to blobs encoding a stable actor type.
pub fn check_memory_compatibility(_old_type: Value, _new_type: Value) {
    // TODO: Implement
}
