//! Support for stable functions during persistence.
//!
//! A stable function is a named local function,
//! either contained in the actor or another named scope, such as
//! * a module,
//! * a function in a named scope,
//! * a class in a named scope,
//! * a named object in a named scope.
//!
//! Stable functions correspond to equally named functions in the new program versions.
//!
//! Function references are encoded by a persistent function ids that stay invariant across upgrades.
//!
//! Each program version defines a set of functions that can be assigned as stable function references.
//! Each such function obtains a function id on program initialization and upgrade.
//! If the function was already declared in the previous version, its function id is reused on upgrade.
//! Otherwise, if it is a new stable function, it obtains a new function id, or in the future, a recycled id.
//!
//! The runtime system supports stable functions by two mechanisms:
//!
//! 1. **Persistent virtual table** for stable function calls:
//!     
//!    The persistent virtual table maps function ids to Wasm table indices,
//!    for supporting dynamic calls of stable functions.
//!    Each entry also stores the hashed name of the stable function to match
//!    and rebind the function ids to the corresponding functions of the new Wasm
//!    binary on a program upgrade.
//!    The table survives upgrades and is built and updated by the runtime system.
//!    To build and update the persistent virtual table, the compiler provides
//!    a **stable function map**, mapping the hashed name of a potential
//!    stable function to the corresponding Wasm table index. For performance,
//!    the stable function map is sorted by the hashed name.
//!    
//! 2. **Function literal table** for materializing stable function literals:
//!
//!    As the compiler does not yet know the function ids of stable function literals/constants,
//!    this table maps a Wasm table index of the current program version to a function id.   
//!    The dynamic function literal table is re-built on program initialization and upgrade.
//!    When a stable function literal is loaded, it serves for resolving the corresponding
//!    function id and thus the stable function reference.
//!    The table is discarded on upgrades and (re-)constructed by the runtime system, based on
//!    the information of the **stable function map**.
//!
//! Potential garbage collection in the future:
//! * The runtime system could allow discarding old stable functions that are unused,
//!   i.e. when it is no longer stored in a live object and and no longer part of the literal table.
//! * Free function ids can be recycled for new stable functions in persistent virtual table.
//! * Once freed, a new program version is liberated from providing a matching stable function.

use core::{marker::PhantomData, mem::size_of, ptr::null_mut};

use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::{allocation_barrier, write_with_barrier},
    memory::{alloc_blob, Memory},
    rts_trap_with,
    types::{Blob, Bytes, Value, NULL_POINTER, TAG_BLOB_B},
};

use super::stable_function_state;

// Use `usize` and not `u32` to avoid unwanted padding on Memory64. 
// E.g. struct sizes will be rounded to 64-bit.
type FunctionId = usize;
type WasmTableIndex = usize;
type NameHash = usize;

/// Part of the persistent metadata. Contains GC-managed references to blobs.
#[repr(C)]
pub struct StableFunctionState {
    /// Persistent virtual table.
    virtual_table: Value,
}

// Transient table. GC root.
static mut FUNCTION_LITERAL_TABLE: Value = NULL_POINTER;

impl StableFunctionState {
    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_virtual_table(&mut self) -> *mut PersistentVirtualTable {
        assert_ne!(self.virtual_table, NULL_POINTER);
        self.virtual_table.as_blob_mut() as *mut PersistentVirtualTable
    }

    // GC root if part of the persistent metadata.
    pub fn virtual_table_location(&mut self) -> *mut Value {
        &mut self.virtual_table
    }

    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_literal_table(&mut self) -> *mut DynamicLiteralTable {
        assert_ne!(FUNCTION_LITERAL_TABLE, NULL_POINTER);
        FUNCTION_LITERAL_TABLE.as_blob_mut() as *mut DynamicLiteralTable
    }

    // Transient GC root.
    pub unsafe fn literal_table_location(&mut self) -> *mut Value {
        &mut FUNCTION_LITERAL_TABLE
    }
}

#[repr(C)]
struct IndexedTable<T> {
    header: Blob,
    _phantom: PhantomData<T>, // not materialized, just to use generic type.
                              // Series of `T`
}

impl<T> IndexedTable<T> {
    unsafe fn length(self: *const Self) -> usize {
        let payload_length = (self as *const Blob).len();
        debug_assert_eq!(payload_length.as_usize() % Self::get_entry_size(), 0);
        payload_length.as_usize() / Self::get_entry_size()
    }

    unsafe fn get(self: *mut Self, index: usize) -> *mut T {
        assert!(index <= self.length());
        let base = (self as *mut Blob).payload_addr() as *mut T;
        base.add(index)
    }

    unsafe fn set(self: *mut Self, index: usize, new_entry: T) {
        let old_entry = self.get(index);
        *old_entry = new_entry;
    }

    const fn get_entry_size() -> usize {
        size_of::<T>()
    }
}

/// Indexed by function id.
type PersistentVirtualTable = IndexedTable<VirtualTableEntry>;

#[repr(C)]
#[derive(Clone)]
struct VirtualTableEntry {
    function_name_hash: NameHash,
    wasm_table_index: WasmTableIndex,
}

#[no_mangle]
pub unsafe fn resolve_stable_function_call(function_id: FunctionId) -> WasmTableIndex {
    debug_assert_ne!(function_id, NULL_FUNCTION_ID);
    let virtual_table = stable_function_state().get_virtual_table();
    let table_entry = virtual_table.get(function_id);
    (*table_entry).wasm_table_index
}

/// Indexed by Wasm table index.
type DynamicLiteralTable = IndexedTable<FunctionId>;

#[no_mangle]
pub unsafe fn resolve_stable_function_literal(wasm_table_index: WasmTableIndex) -> FunctionId {
    let literal_table = stable_function_state().get_literal_table();
    let function_id = *literal_table.get(wasm_table_index);
    assert_ne!(function_id, NULL_FUNCTION_ID); // must be a stable function.
    function_id
}

#[repr(C)]
struct StableFunctionEntry {
    function_name_hash: NameHash,
    wasm_table_index: WasmTableIndex,
    /// Cache for runtime optimization.
    /// This entry is uninitialized by the compiler and the runtime system
    /// uses this space to remember matched function ids for faster lookup.
    cached_function_id: FunctionId,
}

/// Sorted by hash name.
type StableFunctionMap = IndexedTable<StableFunctionEntry>;

impl StableFunctionMap {
    unsafe fn find(self: *mut Self, name: NameHash) -> *mut StableFunctionEntry {
        // Binary search
        let mut left = 0;
        let mut right = self.length();
        while left < right {
            let middle = (left + right) / 2;
            let entry = self.get(middle);
            let middle_name = (*entry).function_name_hash;
            debug_assert!(
                (*self.get(left)).function_name_hash <= middle_name
                    && middle_name <= (*self.get(right - 1)).function_name_hash
            );
            if name <= middle_name {
                right = middle;
            } else {
                left = middle + 1;
            }
        }
        if left < self.length() {
            return null_mut();
        } else {
            let entry = self.get(left);
            if (*entry).function_name_hash == name {
                return entry;
            } else {
                return null_mut();
            }
        }
    }
}

/// Called on program initialization and on upgrade, both EOP and graph copy.
#[ic_mem_fn]
pub unsafe fn register_stable_functions<M: Memory>(
    mem: &mut M,
    stable_functions: *mut StableFunctionMap,
) {
    // O(n*log(n)) runtime costs:
    // 1. Initialize all function ids in stable functions map to null sentinel.
    prepare_stable_function_map(stable_functions);
    // 2. Scan the persistent virtual table and match/update all entries against
    // `stable_functions`. Assign the function ids in stable function map.
    let virtual_table = stable_function_state().get_virtual_table();
    update_existing_functions(virtual_table, stable_functions);
    // 3. Scan stable functions map and determine number of new stable functions that are yet
    // not part of the persistent virtual table.
    let extension_size = count_new_functions(stable_functions);
    // 4. Extend the persistent virtual table by the new stable functions.
    // Assign the function ids in stable function map.
    let new_virtual_table = add_new_functions(mem, virtual_table, extension_size, stable_functions);
    // 5. Create the function literal table by scanning the stable functions map and
    // mapping Wasm table indices to their assigned function id.
    let new_literal_table = create_function_literal_table(mem, stable_functions);
    // 6. Store the new persistent virtual table and dynamic literal table.
    // Apply write barriers!
    let state = stable_function_state();
    write_with_barrier(mem, state.virtual_table_location(), new_virtual_table);
    write_with_barrier(mem, state.literal_table_location(), new_literal_table);
}

const NULL_FUNCTION_ID: FunctionId = FunctionId::MAX;

/// Step 1: Initialize all function ids in the stable function map to null.
unsafe fn prepare_stable_function_map(stable_functions: *mut StableFunctionMap) {
    for index in 0..stable_functions.length() {
        let entry = stable_functions.get(index);
        (*entry).cached_function_id = NULL_FUNCTION_ID;
    }
}

// Step 2: Scan the persistent virtual table and match/update all entries against
// `stable_functions`. Assign the function ids in stable function map.
unsafe fn update_existing_functions(
    virtual_table: *mut PersistentVirtualTable,
    stable_functions: *mut StableFunctionMap,
) {
    assert_ne!(virtual_table.length(), NULL_FUNCTION_ID);
    for function_id in 0..virtual_table.length() {
        let virtual_table_entry = virtual_table.get(function_id);
        let name_hash = (*virtual_table_entry).function_name_hash;
        let stable_function_entry = stable_functions.find(name_hash);
        if stable_function_entry == null_mut() {
            rts_trap_with(format!(200, "Incompatible upgrade: Stable function {name_hash} is missing in the new program version"));
        }
        (*virtual_table_entry).wasm_table_index = (*stable_function_entry).wasm_table_index;
        (*stable_function_entry).cached_function_id = function_id as FunctionId;
    }
}

// 3. Scan stable functions map and determine number of new stable functions that are yet
// not part of the persistent virtual table.
unsafe fn count_new_functions(stable_functions: *mut StableFunctionMap) -> usize {
    let mut count = 0;
    for index in 0..stable_functions.length() {
        let function_entry = stable_functions.get(index);
        if (*function_entry).cached_function_id == NULL_FUNCTION_ID {
            count += 1;
        }
    }
    count
}

// 4. Extend the persistent virtual table by the new stable functions.
// Assign the function ids in stable function map.
unsafe fn add_new_functions<M: Memory>(
    mem: &mut M,
    old_virtual_table: *mut PersistentVirtualTable,
    new_function_count: usize,
    stable_functions: *mut StableFunctionMap,
) -> Value {
    if new_function_count == 0 {
        return Value::from_ptr(old_virtual_table as usize);
    }
    let new_length = old_virtual_table.length() + new_function_count;
    let new_blob = extend_virtual_table(mem, old_virtual_table, new_length);
    let new_virtual_table = new_blob.as_blob_mut() as *mut PersistentVirtualTable;
    let mut function_id = old_virtual_table.length();
    for index in 0..stable_functions.length() {
        let stable_function_entry = stable_functions.get(index);
        assert_ne!(stable_function_entry, null_mut());
        if (*stable_function_entry).cached_function_id == NULL_FUNCTION_ID {
            let function_name_hash = (*stable_function_entry).function_name_hash;
            let wasm_table_index = (*stable_function_entry).wasm_table_index;
            let new_virtual_table_entry = VirtualTableEntry {
                function_name_hash,
                wasm_table_index,
            };
            debug_assert_ne!(function_id, NULL_FUNCTION_ID);
            new_virtual_table.set(function_id, new_virtual_table_entry);
            (*stable_function_entry).cached_function_id = function_id as FunctionId;
            function_id += 1;
        }
    }
    debug_assert_eq!(function_id, new_virtual_table.length());
    new_blob
}

unsafe fn extend_virtual_table<M: Memory>(
    mem: &mut M,
    old_virtual_table: *mut PersistentVirtualTable,
    new_length: usize,
) -> Value {
    debug_assert!(new_length > old_virtual_table.length());
    let new_blob = alloc_blob(
        mem,
        TAG_BLOB_B,
        Bytes(new_length * PersistentVirtualTable::get_entry_size()),
    );
    allocation_barrier(new_blob);
    let new_virtual_table = new_blob.as_blob_mut() as *mut PersistentVirtualTable;
    for index in 0..old_virtual_table.length() {
        let old_entry = old_virtual_table.get(index);
        new_virtual_table.set(index, (*old_entry).clone());
    }
    // New entries will be initialized by caller (`add_new_functions`).
    new_blob
}

// 5. Create the function literal table by scanning the stable functions map and
// mapping Wasm table indices to their assigned function id.
unsafe fn create_function_literal_table<M: Memory>(
    mem: &mut M,
    stable_functions: *mut StableFunctionMap,
) -> Value {
    let table_length = compute_literal_table_length(stable_functions);
    let byte_length = Bytes(table_length * DynamicLiteralTable::get_entry_size());
    let new_blob = alloc_blob(mem, TAG_BLOB_B, byte_length);
    allocation_barrier(new_blob);
    let dynamic_literal_table = new_blob.as_blob_mut() as *mut DynamicLiteralTable;
    for index in 0..stable_functions.length() {
        let entry = stable_functions.get(index);
        let wasm_table_index = (*entry).wasm_table_index;
        let function_id = (*entry).cached_function_id; // Can also be `NULL_FUNCTION_ID` if not stable.
        dynamic_literal_table.set(wasm_table_index, function_id);
    }
    new_blob
}

unsafe fn compute_literal_table_length(stable_functions: *mut StableFunctionMap) -> usize {
    let mut length = 0;
    for index in 0..stable_functions.length() {
        let entry = stable_functions.get(index);
        let wasm_table_index = (*entry).wasm_table_index;
        length = core::cmp::max(length, wasm_table_index + 1);
    }
    length
}
