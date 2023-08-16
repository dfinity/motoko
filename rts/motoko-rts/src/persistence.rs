//! Orthogonal persistence support
//!
//! Persistent metadata table, located at 4MB, in the static partition space.

use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::{allocation_barrier, write_with_barrier},
    gc::incremental::{State, UNINITIALIZED_STATE},
    memory::Memory,
    types::{size_of, Null, Value, TAG_NULL},
};

const FINGERPRINT: [char; 32] = [
    'M', 'O', 'T', 'O', 'K', 'O', ' ', 'O', 'R', 'T', 'H', 'O', 'G', 'O', 'N', 'A', 'L', ' ', 'P',
    'E', 'R', 'S', 'I', 'S', 'T', 'E', 'N', 'C', 'E', ' ', '3', '2',
];
const VERSION: usize = 1;
// The `Value` representation in the default-initialized Wasm memory.
// The GC ignores this value since it is a scalar representation.
const DEFAULT_VALUE: Value = Value::from_scalar(0);

// Use a long-term representation by relying on C layout.
// The `Value` references belong to the GC root set and require forwarding pointer resolution.
#[repr(C)]
struct PersistentMetadata {
    // Predefined character sequence in the memory to double check the orthogonal persistence mode.
    fingerprint: [char; 32],
    // Version of the orthogonal persistence. To be increased on every persistent memory layout modification.
    version: usize,
    // Reference to the stable sub-record of the actor, comprising all stable actor fields. Set before upgrade.
    // Constitutes a GC root and requires pointer forwarding.
    stable_actor: Value,
    // Singleton of the top-level null value. To be retained across upgrades.
    // Constitutes a GC root and requires pointer forwarding.
    null_singleton: Value,
    // The state of the incremental GC including the partitioned heap description.
    // The GC continues work after upgrades.
    incremental_gc_state: State,
}

const METATDATA_ADDRESS: usize = 4 * 1024 * 1024;
const METADATA_RESERVE: usize = 128 * 1024;

// TODO: Include partition table in reserved space.
#[cfg(feature = "ic")]
pub const HEAP_START: usize = METATDATA_ADDRESS + METADATA_RESERVE;

const _: () = assert!(core::mem::size_of::<PersistentMetadata>() <= METADATA_RESERVE);

impl PersistentMetadata {
    fn get() -> *mut Self {
        METATDATA_ADDRESS as *mut Self
    }

    unsafe fn is_initialized(self: *mut Self) -> bool {
        // Wasm memory is zero-initialized according to the Wasm specification.
        let initialized = (*self).version != 0;
        assert!(
            initialized
                || (*self).fingerprint == ['\0'; 32]
                    && (*self).stable_actor == DEFAULT_VALUE
                    && (*self).null_singleton == DEFAULT_VALUE
        );
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

    unsafe fn initialize<M: Memory>(self: *mut Self, mem: &mut M) {
        debug_assert!(!self.is_initialized());
        (*self).fingerprint = FINGERPRINT;
        (*self).version = VERSION;
        (*self).stable_actor = DEFAULT_VALUE;
        (*self).null_singleton = alloc_null(mem);
        (*self).incremental_gc_state = UNINITIALIZED_STATE;
    }
}

/// Initialize fresh peristent memory after the canister installation or
/// reuse the persistent memory on a canister upgrade.
#[cfg(feature = "ic")]
pub unsafe fn initialize_memory<M: Memory>(mem: &mut M) {
    mem.grow_memory(HEAP_START as u64);
    let metadata = PersistentMetadata::get();
    if metadata.is_initialized() {
        metadata.check_version();
    } else {
        metadata.initialize(mem);
    }
}

/// Returns the stable sub-record of the actor of the upgraded canister version.
/// Returns scalar 0 if no actor is stored after on a fresh memory.
#[no_mangle]
pub unsafe extern "C" fn load_stable_actor() -> Value {
    let metadata = PersistentMetadata::get();
    (*metadata).stable_actor.forward_if_possible()
}

/// Save the stable sub-record, the stable variables, of a canister before an upgrade.
#[ic_mem_fn]
pub unsafe fn save_stable_actor<M: Memory>(mem: &mut M, actor: Value) {
    assert!(actor != DEFAULT_VALUE);
    let metadata = PersistentMetadata::get();
    let location = &mut (*metadata).stable_actor as *mut Value;
    write_with_barrier(mem, location, actor);
}

// GC root pointer required for GC marking and updating.
pub(crate) unsafe fn stable_actor_location() -> *mut Value {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).stable_actor as *mut Value
}

unsafe fn alloc_null<M: Memory>(mem: &mut M) -> Value {
    let value = mem.alloc_words(size_of::<Null>());
    let null = value.get_ptr() as *mut Null;
    (*null).header.tag = TAG_NULL;
    (*null).header.init_forward(value);
    allocation_barrier(value);
    value
}

/// Get the null singleton used for top-level optional types.
/// Serves for optimized null checks by pointer comparison.
/// The forwarding pointer of this object is already resolved.
/// NOTE: The forwarding pointer of the other comparison argument needs
/// to be resolved too.
#[no_mangle]
pub unsafe extern "C" fn null_singleton() -> Value {
    let metadata = PersistentMetadata::get();
    (*metadata).null_singleton.forward_if_possible()
}

// GC root pointer required for GC marking and updating.
pub(crate) unsafe fn null_singleton_location() -> *mut Value {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).null_singleton as *mut Value
}

// GC root pointer required for GC marking and updating.
#[cfg(feature = "ic")]
pub(crate) unsafe fn get_incremental_gc_state() -> &'static mut State {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).incremental_gc_state
}
