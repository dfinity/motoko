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

const COPY_TIME_LIMIT: usize = 10_000;

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

/// Pre-upgrade operation for graph-copy-based program upgrades:
/// All objects inside main memory that are transitively reachable from stable variables are
/// serialized into stable memory by using a graph copy algorithm.
/// `stable_actor`: Root object for stabilization containing all stable variables of the actor.
/// The remaining parameters encode the type table of the current program version:
/// `old_candid_data`: A blob encoding the Candid type as a table.
/// `old_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Implementation:
/// * Algorithm: Cheney's algorithm using main memory as from-space and stable memory as to-space.
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in
///   the stable object graph layout (see `GraphCopyStabilization.md`).
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn stabilize(stable_actor: Value, old_candid_data: Value, old_type_offsets: Value) {
    use crate::stabilization::{
        graph_copy::GraphCopy, metadata::StabilizationMetadata, serialization::Serialization,
    };
    use compatibility::TypeDescriptor;

    let stable_memory_pages = stable_mem::size();
    let serialized_data_start = stable_memory_pages * PAGE_SIZE;

    let mut serialization = Serialization::start(stable_actor, serialized_data_start);
    while !serialization.is_completed() {
        serialization.copy_increment();
    }
    let serialized_data_length = serialization.complete();

    let type_descriptor = TypeDescriptor::new(old_candid_data, old_type_offsets, 0);
    let metadata = StabilizationMetadata {
        stable_memory_pages,
        serialized_data_start,
        serialized_data_length,
        type_descriptor,
    };
    metadata.store();
}

/// Post-upgrade operation for graph-copy-based program upgrades:
/// Deserialize the object graph stored in stable memory back into main memory by using a graph
/// copy algorithm. Checks whether the new program version is compatible to the stored state by
/// comparing the type tables of both the old and the new program version.
/// The parameters encode the type table of the new program version to which that data is to be upgraded.
/// `new_candid_data`: A blob encoding the Candid type as a table.
/// `new_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Returns the root object containing all restored stable variables of the actor.
/// Traps if the stable state is incompatible with the new program version and the upgrade is not
/// possible.
/// Implementation:
/// * Algorithm: Cheney's algorithm using stable memory as from-space and main memory as to-space.
/// * Encoding: The from-space uses the stable memory layout, while the to-space is to be encoded in
///   main memory layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn destabilize<M: crate::memory::Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) -> Value {
    use crate::stabilization::{deserialization::Deserialization, graph_copy::GraphCopy};
    use crate::{rts_trap_with, stable_mem::moc_stable_mem_set_size};
    use compatibility::{memory_compatible, TypeDescriptor};
    use metadata::StabilizationMetadata;

    let mut new_type_descriptor = TypeDescriptor::new(new_candid_data, new_type_offsets, 0);
    let (metadata, statistics) = StabilizationMetadata::load(mem);
    let mut old_type_descriptor = metadata.type_descriptor;
    if !memory_compatible(mem, &mut old_type_descriptor, &mut new_type_descriptor) {
        rts_trap_with("Memory-incompatible program upgrade");
    }

    let mut deserialization = Deserialization::start(
        mem,
        metadata.serialized_data_start,
        metadata.serialized_data_length,
    );
    while !deserialization.is_completed() {
        deserialization.copy_increment();
    }
    let stable_root = deserialization.complete();

    moc_stable_mem_set_size(metadata.stable_memory_pages);
    set_upgrade_instructions(statistics.stabilization_instructions + ic0_performance_counter(0));
    stable_root
}

#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn use_new_destabilization() -> bool {
    metadata::StabilizationMetadata::matching_version()
}
