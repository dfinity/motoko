//! Orthogonal persistence support
//!
//! Persistent metadata table, located at 6MB, in the static partition space.

pub mod compatibility;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::{allocation_barrier, write_with_barrier},
    gc::incremental::State,
    memory::Memory,
    persistence::compatibility::memory_compatible,
    rts_trap_with,
    types::{size_of, Null, Value, TAG_BLOB, TAG_NULL},
};

use self::compatibility::TypeDescriptor;

#[cfg(feature = "ic")]
const FINGERPRINT: [char; 32] = [
    'M', 'O', 'T', 'O', 'K', 'O', ' ', 'O', 'R', 'T', 'H', 'O', 'G', 'O', 'N', 'A', 'L', ' ', 'P',
    'E', 'R', 'S', 'I', 'S', 'T', 'E', 'N', 'C', 'E', ' ', '6', '4',
];
#[cfg(feature = "ic")]
const VERSION: usize = 1;
/// The `Value` representation in the default-initialized Wasm memory.
/// The GC ignores this value since it is a scalar representation.
const DEFAULT_VALUE: Value = Value::from_scalar(0);

/// The persistent metadata stored at the defined location `METADATA_ADDRESS` in memory.
/// Use a long-term representation by relying on C layout.
/// The `Value` references belong to the GC root set and require forwarding pointer resolution.
#[repr(C)]
struct PersistentMetadata {
    /// Predefined character sequence in the memory to double check the orthogonal persistence mode.
    fingerprint: [char; 32],
    /// Version of the orthogonal persistence. To be increased on every persistent memory layout modification.
    version: usize,
    /// Reference to the stable sub-record of the actor, comprising all stable actor fields. Set before upgrade.
    /// Constitutes a GC root and requires pointer forwarding.
    stable_actor: Value,
    /// Reference to the stable type descriptor, serving for heap compatibility checks on upgrades.
    /// The blob in the descriptor serves as a GC root and requires pointer forwarding.
    stable_type: TypeDescriptor,
    /// The state of the incremental GC including the partitioned heap description.
    /// The GC continues work after upgrades.
    incremental_gc_state: State,
    /// Singleton of the top-level null value. To be retained across upgrades.
    /// Constitutes a GC root and requires pointer forwarding.
    null_singleton: Value,
}

/// Location of the persistent metadata. Prereseved and fixed forever.
const METADATA_ADDRESS: usize = 8 * 1024 * 1024;
/// The reserved maximum size of the metadata, contains a reserve for future extension of the metadata.
const METADATA_RESERVE: usize = 256 * 1024;

pub const HEAP_START: usize = METADATA_ADDRESS + METADATA_RESERVE;

const _: () = assert!(core::mem::size_of::<PersistentMetadata>() <= METADATA_RESERVE);

impl PersistentMetadata {
    fn get() -> *mut Self {
        METADATA_ADDRESS as *mut Self
    }

    #[cfg(feature = "ic")]
    unsafe fn is_initialized(self: *mut Self) -> bool {
        // Wasm memory is zero-initialized according to the Wasm specification.
        let initialized = (*self).version != 0;
        assert!(
            initialized
                || (*self).fingerprint == ['\0'; 32]
                    && (*self).stable_actor == DEFAULT_VALUE
                    && (*self).stable_type.is_default()
                    && (*self).null_singleton == DEFAULT_VALUE
        );
        initialized
    }

    #[cfg(feature = "ic")]
    unsafe fn check_version(self: *const Self) {
        if (*self).version != VERSION {
            panic!(
                "Incompatible persistent memory version: {} instead of {}.",
                (*self).version,
                VERSION
            );
        }
    }

    #[cfg(feature = "ic")]
    unsafe fn initialize<M: Memory>(self: *mut Self, mem: &mut M) {
        use crate::gc::incremental::IncrementalGC;
        debug_assert!(!self.is_initialized());
        (*self).fingerprint = FINGERPRINT;
        (*self).version = VERSION;
        (*self).stable_actor = DEFAULT_VALUE;
        (*self).stable_type = TypeDescriptor::default();
        (*self).incremental_gc_state = IncrementalGC::initial_gc_state(mem, HEAP_START);
        (*self).null_singleton = DEFAULT_VALUE;
    }
}

/// Initialize fresh persistent memory after the canister installation or
/// reuse the persistent memory on a canister upgrade.
#[cfg(feature = "ic")]
pub unsafe fn initialize_memory<M: Memory>(mem: &mut M) {
    mem.grow_memory(HEAP_START);
    let metadata = PersistentMetadata::get();
    if metadata.is_initialized() {
        metadata.check_version();
    } else {
        metadata.initialize(mem);
        allocate_initial_objects(mem);
    }
}

/// Allocate initial objects only after the partitioned heap has been initialized.
#[cfg(feature = "ic")]
unsafe fn allocate_initial_objects<M: Memory>(mem: &mut M) {
    let metadata = PersistentMetadata::get();
    debug_assert!((*metadata).null_singleton == DEFAULT_VALUE);
    (*metadata).null_singleton = allocate_null(mem);
}

/// Returns the stable sub-record of the actor of the upgraded canister version.
/// Returns scalar 0 if no actor is stored after on a fresh memory.
#[no_mangle]
pub unsafe extern "C" fn load_stable_actor() -> Value {
    let metadata = PersistentMetadata::get();
    (*metadata).stable_type.assert_initialized();
    (*metadata).stable_actor.forward_if_possible()
}

/// Save the stable sub-record, the stable variables, of a canister before an upgrade.
#[ic_mem_fn]
pub unsafe fn save_stable_actor<M: Memory>(mem: &mut M, actor: Value) {
    assert!(actor != DEFAULT_VALUE);
    let metadata: *mut PersistentMetadata = PersistentMetadata::get();
    (*metadata).stable_type.assert_initialized();
    let location = &mut (*metadata).stable_actor as *mut Value;
    write_with_barrier(mem, location, actor);
}

/// Free the stable actor sub-record after a completed upgrade.
/// Allow garbage collection for unused stable variables that are no longer declared
/// in the new program version.
#[ic_mem_fn]
pub unsafe fn free_stable_actor<M: Memory>(mem: &mut M) {
    let metadata: *mut PersistentMetadata = PersistentMetadata::get();
    let location = &mut (*metadata).stable_actor as *mut Value;
    write_with_barrier(mem, location, DEFAULT_VALUE);
}

#[cfg(feature = "ic")]
/// GC root pointer required for GC marking and updating.
pub(crate) unsafe fn stable_actor_location() -> *mut Value {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).stable_actor as *mut Value
}

/// Determine whether an object contains a specific field.
/// Used for upgrading to an actor with additional stable fields.
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe extern "C" fn contains_field(actor: Value, field_hash: usize) -> bool {
    use crate::constants::WORD_SIZE;

    let object = actor.as_object();
    let hash_blob = (*object).hash_blob.as_blob();
    assert_eq!(hash_blob.len().as_usize() % WORD_SIZE, 0);
    let number_of_fields = hash_blob.len().as_usize() / WORD_SIZE;
    let mut current_address = hash_blob.payload_const() as usize;
    for _ in 0..number_of_fields {
        let hash_address = current_address as *mut usize;
        if *hash_address == field_hash {
            return true;
        }
        current_address += WORD_SIZE;
    }
    false
}

pub unsafe fn allocate_null<M: Memory>(mem: &mut M) -> Value {
    let value = mem.alloc_words(size_of::<Null>());
    let null = value.get_ptr() as *mut Null;
    (*null).header.tag = TAG_NULL;
    (*null).header.init_forward(value);
    allocation_barrier(value);
    value
}

/// Register the stable actor type on canister installation and upgrade.
/// The type is stored in the persistent metadata memory for later retrieval on canister upgrades.
/// On an upgrade, the memory compatibility between the new and existing stable type is checked.
/// The `new_type` value points to a blob encoding the new stable actor type.
#[ic_mem_fn]
pub unsafe fn register_stable_type<M: Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
    new_actor_index: i32,
) {
    assert_eq!(new_candid_data.tag(), TAG_BLOB);
    let mut new_type = TypeDescriptor::new(new_candid_data, new_type_offsets, new_actor_index);
    let metadata = PersistentMetadata::get();
    let old_type = &mut (*metadata).stable_type;
    if !old_type.is_default() && !memory_compatible(mem, old_type, &mut new_type) {
        rts_trap_with("Memory-incompatible program upgrade");
    }
    (*metadata).stable_type.assign(mem, &new_type);
}

#[cfg(feature = "ic")]
pub(crate) unsafe fn stable_type_descriptor() -> &'static mut TypeDescriptor {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).stable_type
}

/// Get the null singleton used for top-level optional types.
/// Serves for optimized null checks by pointer comparison.
/// The forwarding pointer of this object is already resolved.
/// NOTE: The forwarding pointer of the other comparison argument needs
/// to be resolved too.
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe extern "C" fn null_singleton() -> Value {
    let metadata = PersistentMetadata::get();
    debug_assert!((*metadata).null_singleton != DEFAULT_VALUE);
    (*metadata).null_singleton.forward_if_possible()
}

// GC root pointer required for GC marking and updating.
#[cfg(feature = "ic")]
pub(crate) unsafe fn null_singleton_location() -> *mut Value {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).null_singleton as *mut Value
}

#[cfg(feature = "ic")]
pub(crate) unsafe fn get_incremental_gc_state() -> &'static mut State {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).incremental_gc_state
}
