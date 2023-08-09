//! Orthogonal persistence support
//!
//! Persistent metadata table, located at 4MB, in the static partition space.

use core::mem::size_of;

use motoko_rts_macros::ic_mem_fn;

use crate::{barriers::write_with_barrier, gc::incremental::State, memory::Memory, types::Value};

const FINGERPRINT: [char; 32] = [
    'M', 'O', 'T', 'O', 'K', 'O', ' ', 'O', 'R', 'T', 'H', 'O', 'G', 'O', 'N', 'A', 'L', ' ', 'P',
    'E', 'R', 'S', 'I', 'S', 'T', 'E', 'N', 'C', 'E', ' ', '3', '2',
];
const VERSION: usize = 1;
const NO_ACTOR: Value = Value::from_scalar(0);

// Use a long-term representation by relying on C layout.
#[repr(C)]
struct PersistentMetadata {
    fingerprint: [char; 32],
    version: usize,
    actor: Value, // Must be added to the root set, use forwarding
    incremental_gc_state: State,
}

const METATDATA_ADDRESS: usize = 4 * 1024 * 1024;
const METADATA_RESERVE: usize = 128 * 1024;

// TODO: Include partition table in reserved space.
#[cfg(feature = "ic")]
pub const HEAP_START: usize = METATDATA_ADDRESS + METADATA_RESERVE;

const _: () = assert!(size_of::<PersistentMetadata>() <= METADATA_RESERVE);

impl PersistentMetadata {
    fn get() -> *mut Self {
        METATDATA_ADDRESS as *mut Self
    }

    unsafe fn is_initialized(self: *mut Self) -> bool {
        // Wasm memory is zero-initialized according to the Wasm specification.
        let initialized = (*self).version != 0;
        assert!(initialized || (*self).fingerprint == ['\0'; 32] && (*self).actor == NO_ACTOR);
        initialized
    }

    unsafe fn check_version(self: *const Self) {
        if (*self).version != VERSION {
            panic!(
                "Incompatible persistent memory version: {} instead of {}.",
                (*self).version,
                VERSION
            );
        }
    }

    unsafe fn initialize(self: *mut Self) {
        debug_assert!(!self.is_initialized());
        (*self).fingerprint = FINGERPRINT;
        (*self).version = VERSION;
        (*self).actor = NO_ACTOR;
    }
}

pub unsafe fn initialize_memory<M: Memory>(mem: &mut M) {
    mem.grow_memory(HEAP_START as u64);
    let metadata = PersistentMetadata::get();
    if metadata.is_initialized() {
        println!(100, "MEMORY REUSED");
        metadata.check_version();
    } else {
        println!(100, "MEMORY INITIALIZED");
        metadata.initialize();
    }
}

/// Returns scalar 0 if no actor is stored.
#[no_mangle]
pub unsafe extern "C" fn load_actor() -> Value {
    let metadata = PersistentMetadata::get();
    (*metadata).actor.forward_if_possible()
}

#[ic_mem_fn]
pub unsafe fn save_actor<M: Memory>(mem: &mut M, actor: Value) {
    assert!(actor != NO_ACTOR);
    let metadata = PersistentMetadata::get();
    let location = &mut (*metadata).actor as *mut Value;
    write_with_barrier(mem, location, actor);
}

pub(crate) unsafe fn get_incremenmtal_gc_state() -> &'static mut State {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).incremental_gc_state
}
