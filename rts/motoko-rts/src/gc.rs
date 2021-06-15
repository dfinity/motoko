mod copying;
mod mark_compact;

use crate::closure_table::closure_table_loc;
use crate::types::*;

extern "C" {
    /// Get __heap_base. Provided by the code generator (src/codegen/compile.ml).
    pub(crate) fn get_heap_base() -> u32;

    /// Get pointer to the static memory with an array to the static roots. Provided by the
    /// generated code.
    pub(crate) fn get_static_roots() -> SkewedPtr;
}

/// Maximum live data retained in a GC.
static mut MAX_LIVE: Bytes<u32> = Bytes(0);

/// Amount of garbage collected so far.
static mut RECLAIMED: Bytes<u64> = Bytes(0);

/// Counter for total allocations
pub(crate) static mut ALLOCATED: Bytes<u64> = Bytes(0);

/// Heap pointer
pub(crate) static mut HP: u32 = 0;

#[no_mangle]
unsafe extern "C" fn init() {
    HP = get_heap_base() as u32;
}

unsafe fn note_live_size(live: Bytes<u32>) {
    MAX_LIVE = ::core::cmp::max(MAX_LIVE, live);
}

#[no_mangle]
unsafe extern "C" fn get_max_live_size() -> Bytes<u32> {
    MAX_LIVE
}

unsafe fn note_reclaimed(reclaimed: Bytes<u32>) {
    RECLAIMED += Bytes(reclaimed.0 as u64);
}

#[no_mangle]
unsafe extern "C" fn get_reclaimed() -> Bytes<u64> {
    RECLAIMED
}

#[no_mangle]
unsafe extern "C" fn get_total_allocations() -> Bytes<u64> {
    ALLOCATED
}

#[no_mangle]
unsafe extern "C" fn get_heap_size() -> Bytes<u32> {
    Bytes(HP - get_heap_base())
}
