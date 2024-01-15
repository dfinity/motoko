mod compatibility;
mod metadata;
mod performance;

use motoko_rts_macros::ic_mem_fn;

use crate::{
    gc::{start_gc_after_upgrade, stop_gc_before_upgrade},
    memory::Memory,
    rts_trap_with,
    stabilization::ic::{compatibility::TypeDescriptor, metadata::StabilizationMetadata},
    stable_mem::{self, moc_stable_mem_set_size, PAGE_SIZE},
    types::Value,
};

use self::{metadata::UpgradeStatistics, performance::InstructionMeter};

use super::graph_copy::GraphCopy;
use super::{deserialization::Deserialization, serialization::Serialization};

extern "C" {
    fn set_upgrade_instructions(instructions: u64);
}

struct StabilizationState {
    old_candid_data: Value,
    old_type_offsets: Value,
    completed: bool,
    serialization: Serialization,
    instruction_meter: InstructionMeter,
}

impl StabilizationState {
    fn new(
        serialization: Serialization,
        old_candid_data: Value,
        old_type_offsets: Value,
    ) -> StabilizationState {
        StabilizationState {
            old_candid_data,
            old_type_offsets,
            completed: false,
            serialization,
            instruction_meter: InstructionMeter::new(),
        }
    }
}

static mut STABILIZATION_STATE: Option<StabilizationState> = None;

#[no_mangle]
pub unsafe extern "C" fn is_stabilization_started() -> bool {
    STABILIZATION_STATE.is_some()
}

/// Start the incremental stabilization.
/// This operations is needed before a series of stabilization increments can be run.
/// `stable_actor`: Root object for stabilization containing all stable variables of the actor.
/// `old_candid_data`: A blob encoding the Candid type as a table.
/// `old_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Note:
/// - Once started, the heap is invalidated. Therefore, all application messages must be blocked once this has been started.
#[ic_mem_fn(ic_only)]
pub unsafe fn start_stabilization<M: Memory>(
    mem: &mut M,
    stable_actor: Value,
    old_candid_data: Value,
    old_type_offsets: Value,
) {
    println!(100, "START STABILIZATION");
    stop_gc_before_upgrade();
    assert!(STABILIZATION_STATE.is_none());
    let stable_memory_pages = stable_mem::size();
    let serialized_data_start = stable_memory_pages * PAGE_SIZE;
    let serialization = Serialization::start(mem, stable_actor, serialized_data_start);
    STABILIZATION_STATE = Some(StabilizationState::new(
        serialization,
        old_candid_data,
        old_type_offsets,
    ));
}

/// Incremental stabilization, serializing a limited amount of heap objects reachable from stable variables into stable memory.
/// This function can be called multiple times before the upgrade of a large heap.
/// The incrementality serves to support the graph-copy-based serialization large heaps that do not fit into the upgrade message instruction limit.
/// Returns true if the stabilization has been completed.
/// Notes:
/// - During stabilization, the heap is invalidated. Therefore, all application messages must be blocked once this has been started.
/// - The pre upgrade operation starts and/or completes the serialization if necessary. Manual incremental stabilization ahead of the pre-upgrade
/// is needed for long-running serialization.
/// Implementation:
/// * Algorithm: Cheney's algorithm using main memory as from-space and stable memory as to-space.
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in
///   the stable object graph layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn stabilization_increment<M: Memory>(mem: &mut M) -> bool {
    println!(100, "STABILIZATION INCREMENT");
    let state = STABILIZATION_STATE.as_mut().unwrap();
    if !state.completed {
        state.instruction_meter.start();
        state.serialization.copy_increment(mem);
        state.instruction_meter.stop();
        if state.serialization.is_completed() {
            write_metadata();
            state.completed = true;
        }
    }
    println!(100, "INCREMENT FINISHED {}", state.completed);
    state.completed
}

unsafe fn write_metadata() {
    println!(100, "COMPLETE STABILIZATION");
    let state = STABILIZATION_STATE.as_mut().unwrap();
    assert!(state.serialization.is_completed());
    state.instruction_meter.start();
    let serialized_data_start = state.serialization.serialized_data_start();
    let serialized_data_length = state.serialization.serialized_data_length();

    let type_descriptor = TypeDescriptor::new(state.old_candid_data, state.old_type_offsets, 0);
    let metadata = StabilizationMetadata {
        serialized_data_start,
        serialized_data_length,
        type_descriptor,
    };
    state.instruction_meter.stop();
    metadata.store(&mut state.instruction_meter);
}

struct DestabilizationState {
    deserialization: Deserialization,
    stabilization_statistics: UpgradeStatistics,
    completed: bool,
    instruction_meter: InstructionMeter,
}

static mut DESTABILIZATION_STATE: Option<DestabilizationState> = None;

/// Starts the graph-copy-based destabilization process.
/// This requires that the deserialization is subsequently run and completed.
/// Also checks whether the new program version is compatible to the stored state by comparing the type
/// tables of both the old and the new program version.
/// The parameters encode the type table of the new program version to which that data is to be upgraded.
/// `new_candid_data`: A blob encoding the Candid type as a table.
/// `new_type_offsets`: A blob encoding the type offsets in the Candid type table.
///   Type index 0 represents the stable actor object to be serialized.
/// Traps if the stable state is incompatible with the new program version and the upgrade is not
/// possible.
#[ic_mem_fn(ic_only)]
pub unsafe fn start_destabilization<M: Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) {
    println!(100, "START DESTABILIZATION");
    assert!(DESTABILIZATION_STATE.is_none());

    let mut instruction_meter = InstructionMeter::new();
    instruction_meter.start();
    let mut new_type_descriptor = TypeDescriptor::new(new_candid_data, new_type_offsets, 0);
    let (metadata, statistics) = StabilizationMetadata::load(mem);
    let mut old_type_descriptor = metadata.type_descriptor;
    if !compatibility::memory_compatible(mem, &mut old_type_descriptor, &mut new_type_descriptor) {
        rts_trap_with("Memory-incompatible program upgrade");
    }
    moc_stable_mem_set_size(metadata.serialized_data_start / PAGE_SIZE);
    let deserialization = Deserialization::start(
        mem,
        metadata.serialized_data_start,
        metadata.serialized_data_length,
    );
    instruction_meter.stop();
    DESTABILIZATION_STATE = Some(DestabilizationState {
        deserialization,
        stabilization_statistics: statistics,
        completed: false,
        instruction_meter,
    });
}

/// Incremental destabilization, deserializing a limited amount of serialized data from stable memory to the heap.
/// This function can be called multiple times after the upgrade of a large heap.
/// The incrementality serves to support the graph-copy-based deserialization of large heaps that do not fit into
/// the upgrade message instruction limit.
/// Returns true if the destabilization has been completed.
/// Notes:
/// - The heap is only valid after completed destabilization. Therefore, all application messages must be blocked until this is completed.
/// - The post upgrade operation only runs a few increments that may not yet complete the upgrade.
/// The compiler needs to trigger more messages that run additional destabilzation increments, before the upgrade is completed and the
/// application code can resume its operation.
/// Implementation:
/// * Algorithm: Cheney's algorithm using stable memory as from-space and main memory as to-space.
/// * Encoding: The from-space uses the stable memory layout, while the to-space is to be encoded in
///   main memory layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn destabilization_increment<M: Memory>(mem: &mut M) -> bool {
    println!(100, "DESTABILIZATION INCREMENT");
    let state = DESTABILIZATION_STATE.as_mut().unwrap();
    if !state.completed {
        state.instruction_meter.start();
        state.deserialization.copy_increment(mem);
        state.instruction_meter.stop();
        if state.deserialization.is_completed() {
            record_upgrade_costs();
            state.completed = true;
            start_gc_after_upgrade();
        }
    }
    state.completed
}

unsafe fn record_upgrade_costs() {
    println!(100, "COMPLETE DESTABILIZATION");
    let state = DESTABILIZATION_STATE.as_ref().unwrap();
    let total_instructions = state.stabilization_statistics.stabilization_instructions
        + state.instruction_meter.total_elapsed();
    set_upgrade_instructions(total_instructions);
}

/// Returns the deserialized stable actor root after the completed destabilization.
#[no_mangle]
pub unsafe extern "C" fn get_destabilized_actor() -> Value {
    let state = DESTABILIZATION_STATE.as_ref().unwrap();
    assert!(state.completed);
    state.deserialization.get_stable_root()
}

#[no_mangle]
pub unsafe extern "C" fn use_new_destabilization() -> bool {
    metadata::StabilizationMetadata::matching_version()
}
