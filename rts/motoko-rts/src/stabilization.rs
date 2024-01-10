//! Graph-copy-based stabilzation on upgrades, serializing the entire stable object graph into
//! stable memory by using a defined long-term stable storage format.
//!
//! Use cases:
//! - Classical model of volatile main memory: Preserve the stable variables and their reachable
//!   objects across upgrades.
//! - New model of enhanced orthogonal persistence: Support upgrades in the presence of complex
//!   main memory layout changes.
//!
//! A memory compatibility check has to be performed before allowing upgrade: This checks whether
//! the stored stable object graph is compatible with the new program version. For this purpose, the
//! type tables are compared, similar to the IDL-subtype check but customized to the allowed implicit
//! conversion with regard to the stable storage format.
//!
//! Versioned stable storage format to permit future evolutions of the format.
//!  
//! See `GraphCopyStabilization.md` for the stable format specification and the employed algorithm.

pub mod deserialization;
pub mod graph_copy;
mod layout;
pub mod serialization;

#[cfg(feature = "ic")]
mod compatibility;
#[cfg(feature = "ic")]
mod metadata;
#[cfg(feature = "ic")]
mod performance;

use motoko_rts_macros::ic_mem_fn;

use core::cmp::min;

use crate::{
    constants::WASM_PAGE_SIZE,
    rts_trap_with,
    stable_mem::{self, ic0_stable64_write, PAGE_SIZE},
    types::Value,
};

use self::layout::StableValue;

extern "C" {
    pub fn moc_null_singleton() -> Value;
}

// Dummy value used for non-stable objects that are potentially reachable from
// stable variable because of structural subtyping or `Any`-subtyping.
// Must be a non-skewed value such that the GC also ignores this value.
const DUMMY_VALUE: StableValue = StableValue::from_raw(0);

const COPY_TIME_LIMIT: u64 = 1_000_000_000;

fn clear_stable_memory(start: u64, length: u64) {
    const CHUNK_SIZE: usize = WASM_PAGE_SIZE.as_usize();
    let empty_chunk = [0u8; CHUNK_SIZE];
    let mut position = start;
    let end = start + length;
    while position < end {
        let size = min(end - position, CHUNK_SIZE as u64);
        unsafe {
            ic0_stable64_write(position, &empty_chunk as *const u8 as u64, size);
        }
        position += size;
    }
}

fn grant_stable_space(byte_size: u64) {
    debug_assert!(byte_size < u64::MAX - PAGE_SIZE - 1);
    let required_pages = (byte_size + PAGE_SIZE - 1) / PAGE_SIZE;
    let available_pages = stable_mem::size();
    if required_pages > available_pages {
        let additional_pages = required_pages - available_pages;
        debug_assert_ne!(additional_pages, u64::MAX);
        let result = stable_mem::grow(additional_pages);
        if result == u64::MAX {
            unsafe {
                rts_trap_with("Insufficient stable memory");
            }
        }
    }
}

#[cfg(feature = "ic")]
extern "C" {
    fn ic0_performance_counter(number: u32) -> u64;
    fn set_upgrade_instructions(instructions: u64);
}

/// Temporary counter for upgrade instructions during the incremental stabilization process
/// or incremental destabilization process.
#[cfg(feature = "ic")]
static mut UPGRADE_INSTRUCTIONS: u64 = 0;

#[cfg(feature = "ic")]
static mut SERIALIZATION: Option<serialization::Serialization> = None;

/// Start the incremental stabilization.
/// This operations is needed before a series of stabilization increments can be run.
/// `stable_actor`: Root object for stabilization containing all stable variables of the actor.
/// Note:
/// - Once started, the heap is invalidated. Therefore, all application messages must be blocked once this has been started.
#[ic_mem_fn(ic_only)]
pub unsafe fn start_stabilization<M: crate::memory::Memory>(mem: &mut M, stable_actor: Value) {
    use self::performance::Measurement;
    use self::serialization::Serialization;

    let measurement = Measurement::start();
    assert!(SERIALIZATION.is_none());
    let stable_memory_pages = stable_mem::size();
    let serialized_data_start = stable_memory_pages * PAGE_SIZE;
    SERIALIZATION = Some(Serialization::start(
        mem,
        stable_actor,
        serialized_data_start,
    ));
    debug_assert_eq!(UPGRADE_INSTRUCTIONS, 0);
    UPGRADE_INSTRUCTIONS = measurement.elapsed_instructions();
}

/// Incremental stabilization, serializing a limit amount of heap objects reachable from stable variables into stable memory.
/// This function can be called multiple times before the upgrade of a large heap.
/// The incrementality serves to support the graph-copy-based serialization and deserialization of large heaps that do
/// not fit into the upgrade message instruction limit.
/// Returns true if the stabilization has been completed.
/// Notes:
/// - During stabilization, the heap is invalidated. Therefore, all application messages must be blocked once this has been started.
/// - Add the instruction costs of additionally called stabilization increments to the upgrade costs.
/// Implementation:
/// * Algorithm: Cheney's algorithm using main memory as from-space and stable memory as to-space.
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in
///   the stable object graph layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn stabilization_increment<M: crate::memory::Memory>(mem: &mut M) -> bool {
    use self::graph_copy::GraphCopy;
    use self::performance::Measurement;

    let measuremnt = Measurement::start();
    let serialization = SERIALIZATION.as_mut().unwrap();
    serialization.copy_increment(mem);
    let is_completed = serialization.is_completed();
    if is_completed {
        serialization.complete();
    }
    UPGRADE_INSTRUCTIONS += measuremnt.elapsed_instructions();
    is_completed
}

/// Completes the graph-copy-based stabilization process.
/// This requires that the serialization has been started and completed in advance.
/// The parameters encode the type table of the current program version:
/// `old_candid_data`: A blob encoding the Candid type as a table.
/// `old_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Usage:
/// ```
/// start_stabilization(mem, stable_actor);
/// while !stabilization_increment(mem) {}
/// complete_stabilization(mem, old_candid_data, old_type_offsets);
/// ```
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn complete_stabilization(old_candid_data: Value, old_type_offsets: Value) {
    use self::compatibility::TypeDescriptor;
    use self::graph_copy::GraphCopy;
    use self::metadata::StabilizationMetadata;

    let serialization = SERIALIZATION.as_ref().unwrap();
    assert!(serialization.is_completed());
    let serialized_data_start = serialization.serialized_data_start();
    let serialized_data_length = serialization.serialized_data_length();

    let type_descriptor = TypeDescriptor::new(old_candid_data, old_type_offsets, 0);
    let metadata = StabilizationMetadata {
        serialized_data_start,
        serialized_data_length,
        type_descriptor,
    };
    metadata.store(&mut UPGRADE_INSTRUCTIONS);
}

#[cfg(feature = "ic")]
static mut DESERIALIZATION: Option<deserialization::Deserialization> = None;

/// Starts the graph-copy-based destabilization process.
/// This requires that the deserialized is run and completed subsequently.
/// Checks whether the new program version is compatible to the stored state by comparing the type tables of both
/// the old and the new program version.
/// The parameters encode the type table of the new program version to which that data is to be upgraded.
/// `new_candid_data`: A blob encoding the Candid type as a table.
/// `new_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Traps if the stable state is incompatible with the new program version and the upgrade is not
/// possible.
#[ic_mem_fn(ic_only)]
pub unsafe fn start_destabilization<M: crate::memory::Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) {
    use self::compatibility::{memory_compatible, TypeDescriptor};
    use self::deserialization::Deserialization;
    use self::metadata::StabilizationMetadata;
    use self::performance::Measurement;
    use crate::{rts_trap_with, stable_mem::moc_stable_mem_set_size};

    let measurement = Measurement::start();
    let mut new_type_descriptor = TypeDescriptor::new(new_candid_data, new_type_offsets, 0);
    let (metadata, statistics) = StabilizationMetadata::load(mem);
    let mut old_type_descriptor = metadata.type_descriptor;
    if !memory_compatible(mem, &mut old_type_descriptor, &mut new_type_descriptor) {
        rts_trap_with("Memory-incompatible program upgrade");
    }
    moc_stable_mem_set_size(metadata.serialized_data_start / PAGE_SIZE);

    assert!(DESERIALIZATION.is_none());
    DESERIALIZATION = Some(Deserialization::start(
        mem,
        metadata.serialized_data_start,
        metadata.serialized_data_length,
    ));
    assert_eq!(UPGRADE_INSTRUCTIONS, 0);
    UPGRADE_INSTRUCTIONS =
        statistics.stabilization_instructions + measurement.elapsed_instructions();
}

/// Incremental destabilization, deserializing a limit amount of serialized data from stable memory to the heap.
/// This function can be called multiple times after the upgrade of a large heap.
/// The incrementality serves to support the graph-copy-based serialization and deserialization of large heaps that do
/// not fit into the upgrade message instruction limit.
/// Returns true if the destabilization has been completed.
/// Notes:
/// - The heap is only valid after completed destabilization. Therefore, all application messages must be blocked until this is completed.
/// - The post upgrade operation only runs a few increments that may not yet complete the upgrade.
/// The compiler needs to trigger more messages that run additional destabilzation increments, before the upgrade is completed and the
/// application code can resume its operation.
/// - Add the instruction costs of additionally called destabilization increments to the upgrade costs.
/// Implementation:
/// * Algorithm: Cheney's algorithm using stable memory as from-space and main memory as to-space.
/// * Encoding: The from-space uses the stable memory layout, while the to-space is to be encoded in
///   main memory layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn destabilization_increment<M: crate::memory::Memory>(mem: &mut M) -> bool {
    use self::graph_copy::GraphCopy;
    use self::performance::Measurement;

    let measurement = Measurement::start();
    let deserialization = DESERIALIZATION.as_mut().unwrap();
    deserialization.copy_increment(mem);
    let is_completed = deserialization.is_completed();
    if is_completed {
        deserialization.complete();
    }
    UPGRADE_INSTRUCTIONS += measurement.elapsed_instructions();
    is_completed
}

/// Returns the deserialized stable actor root after the completed destabilization.
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn complete_destabilization() -> Value {
    use crate::stabilization::graph_copy::GraphCopy;

    let deserialization = DESERIALIZATION.as_ref().unwrap();
    assert!(deserialization.is_completed());
    set_upgrade_instructions(UPGRADE_INSTRUCTIONS);
    UPGRADE_INSTRUCTIONS = 0;

    deserialization.get_stable_root()
}

#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn use_new_destabilization() -> bool {
    metadata::StabilizationMetadata::matching_version()
}
