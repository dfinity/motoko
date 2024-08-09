//! Orthogonal persistence support
//!
//! Persistent metadata table, located at 6MB, in the static partition space.

pub mod compatibility;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::write_with_barrier,
    constants::{KB, MB},
    gc::incremental::{partitioned_heap::allocate_initial_memory, State},
    memory::Memory,
    persistence::compatibility::memory_compatible,
    region::{
        LEGACY_VERSION_NO_STABLE_MEMORY, LEGACY_VERSION_REGIONS, LEGACY_VERSION_SOME_STABLE_MEMORY,
        VERSION_GRAPH_COPY_NO_REGIONS, VERSION_GRAPH_COPY_REGIONS, VERSION_STABLE_HEAP_NO_REGIONS,
        VERSION_STABLE_HEAP_REGIONS,
    },
    rts_trap_with,
    stable_mem::read_persistence_version,
    types::{Bytes, Value, TAG_BLOB_B},
};

use self::compatibility::TypeDescriptor;

const FINGERPRINT: [char; 32] = [
    'M', 'O', 'T', 'O', 'K', 'O', ' ', 'O', 'R', 'T', 'H', 'O', 'G', 'O', 'N', 'A', 'L', ' ', 'P',
    'E', 'R', 'S', 'I', 'S', 'T', 'E', 'N', 'C', 'E', ' ', '6', '4',
];
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
    /// Upgrade performance statistics: Total number of instructions consumed by the last upgrade.
    upgrade_instructions: u64,
}

/// Location of the persistent metadata. Prereserved and fixed forever.
const METADATA_ADDRESS: usize = 4 * MB + 512 * KB;
/// The reserved maximum size of the metadata, contains a reserve for future extension of the metadata.
const METADATA_RESERVE: usize = 512 * KB;

pub const HEAP_START: usize = METADATA_ADDRESS + METADATA_RESERVE;

const _: () = assert!(core::mem::size_of::<PersistentMetadata>() <= METADATA_RESERVE);

impl PersistentMetadata {
    fn get() -> *mut Self {
        METADATA_ADDRESS as *mut Self
    }

    unsafe fn is_initialized(self: *mut Self) -> bool {
        // Wasm memory is zero-initialized according to the Wasm specification.
        let initialized = (*self).version != 0;
        assert!(
            initialized
                || (*self).fingerprint == ['\0'; 32]
                    && (*self).stable_actor == DEFAULT_VALUE
                    && (*self).stable_type.is_default()
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

    unsafe fn initialize<M: Memory>(self: *mut Self) {
        use crate::gc::incremental::IncrementalGC;
        (*self).fingerprint = FINGERPRINT;
        (*self).version = VERSION;
        (*self).stable_actor = DEFAULT_VALUE;
        (*self).stable_type = TypeDescriptor::default();
        (*self).incremental_gc_state = IncrementalGC::<M>::initial_gc_state(HEAP_START);
        (*self).upgrade_instructions = 0;
    }
}

/// Initialize fresh persistent memory after the canister installation or reuse
/// the persistent memory on a canister upgrade if enhanced orthogonal persistence
/// is active. For graph-copy-based destabilization, the memory is reinitialized.
pub unsafe fn initialize_memory<M: Memory>() {
    allocate_initial_memory(Bytes(HEAP_START));
    let metadata = PersistentMetadata::get();
    if use_enhanced_orthogonal_persistence() && metadata.is_initialized() {
        metadata.check_version();
    } else {
        metadata.initialize::<M>();
    }
}

unsafe fn use_enhanced_orthogonal_persistence() -> bool {
    match read_persistence_version() {
        VERSION_STABLE_HEAP_NO_REGIONS | VERSION_STABLE_HEAP_REGIONS => true,
        VERSION_GRAPH_COPY_NO_REGIONS
        | VERSION_GRAPH_COPY_REGIONS
        | LEGACY_VERSION_NO_STABLE_MEMORY
        | LEGACY_VERSION_SOME_STABLE_MEMORY
        | LEGACY_VERSION_REGIONS => false,
        _ => rts_trap_with("Unsupported persistence version"),
    }
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

/// GC root pointer required for GC marking and updating.
pub(crate) unsafe fn stable_actor_location() -> *mut Value {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).stable_actor as *mut Value
}

/// Determine whether an object contains a specific field.
/// Used for upgrading to an actor with additional stable fields.
#[no_mangle]
pub unsafe extern "C" fn contains_field(actor: Value, field_hash: usize) -> bool {
    use crate::constants::WORD_SIZE;

    let object = actor.as_object();
    let hash_blob = (*object).hash_blob.as_blob();
    assert_eq!(hash_blob.len().as_usize() % WORD_SIZE, 0);
    let number_of_fields = hash_blob.len().as_usize() / WORD_SIZE;
    let mut current_address = hash_blob.payload_const() as usize;
    for _ in 0..number_of_fields {
        let hash_address = current_address as *mut usize;
        let hash_value = *hash_address;
        // The hash sequence is sorted: Stop when the hash matches or cannot exist.
        if hash_value >= field_hash {
            return hash_value == field_hash;
        }
        current_address += WORD_SIZE;
    }
    false
}

/// Register the stable actor type on canister initialization and upgrade.
/// The type is stored in the persistent metadata memory for later retrieval on canister upgrades.
/// On an upgrade, the memory compatibility between the new and existing stable type is checked.
/// The `new_type` value points to a blob encoding the new stable actor type.
#[ic_mem_fn]
pub unsafe fn register_stable_type<M: Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) {
    assert_eq!(new_candid_data.tag(), TAG_BLOB_B);
    assert_eq!(new_type_offsets.tag(), TAG_BLOB_B);
    let mut new_type = TypeDescriptor::new(new_candid_data, new_type_offsets);
    let metadata = PersistentMetadata::get();
    let old_type = &mut (*metadata).stable_type;
    if !old_type.is_default() && !memory_compatible(mem, old_type, &mut new_type) {
        rts_trap_with("Memory-incompatible program upgrade");
    }
    (*metadata).stable_type.assign(mem, &new_type);
}

pub(crate) unsafe fn stable_type_descriptor() -> &'static mut TypeDescriptor {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).stable_type
}

pub(crate) unsafe fn get_incremental_gc_state() -> &'static mut State {
    let metadata = PersistentMetadata::get();
    &mut (*metadata).incremental_gc_state
}

#[no_mangle]
pub unsafe extern "C" fn get_upgrade_instructions() -> u64 {
    let metadata = PersistentMetadata::get();
    (*metadata).upgrade_instructions
}

#[no_mangle]
pub unsafe extern "C" fn set_upgrade_instructions(instructions: u64) {
    let metadata = PersistentMetadata::get();
    (*metadata).upgrade_instructions = instructions;
}

/// Only used in WASI mode: Get a static temporary print buffer that resides in 32-bit address range.
/// This buffer has a fix length of 512 bytes, and resides at the end of the metadata reserve.
#[no_mangle]
pub unsafe extern "C" fn buffer_in_32_bit_range() -> usize {
    use crate::types::size_of;

    const BUFFER_SIZE: usize = 512;
    assert!(size_of::<PersistentMetadata>().to_bytes().as_usize() + BUFFER_SIZE < METADATA_RESERVE);
    METADATA_ADDRESS + METADATA_RESERVE - BUFFER_SIZE
}
