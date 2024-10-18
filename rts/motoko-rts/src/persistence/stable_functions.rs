//! Support for stable functions during persistence.
//!
//! A stable function is a named local function, either contained
//! in the actor or another named scope, such as
//! * a module,
//! * a function in a named scope,
//! * a class in a named scope,
//! * a named object in a named scope.
//!
//! Stable functions correspond to equally named functions in new program versions.
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
//!    a **static function map**, mapping the hashed name of a potential
//!    stable function to the corresponding Wasm table index. For performance,
//!    the static function table is sorted by the hashed name.
//!    
//! 2. **Dynamic literal table** for materializing stable function literals:
//!
//!    As the compiler does not yet know the function ids of stable function literals,
//!    this table maps a compiler-generated function literal index to a function id.   
//!    The dynamic function literal table is re-built on program initialization and upgrade.
//!    When a stable function literal is loaded, it serves for resolving the corresponding
//!    function id and thus the stable function reference.
//!    The table is discarded on upgrades and (re-)constructed by the runtime system.
//!    To build the dynamic function literal table, the compiler provides a
//!    **static literal map**, mapping each literal index to the the hashed name of the
//!    stable function.
//!
//! Potential garbage collection in the future:
//! * The runtime system could allow discarding old stable functions that are unused,
//!   i.e. when it is no longer stored in a live object and and no longer part of the literal table.
//! * Free function ids can be recycled for new stable functions in persistent virtual table.
//! * Once freed, a new program version is liberated from providing a matching stable function.

use core::{marker::PhantomData, mem::size_of, ptr::{null, null_mut}};

use motoko_rts_macros::ic_mem_fn;

use crate::{
    barriers::{allocation_barrier, write_with_barrier}, memory::{alloc_blob, Memory}, rts_trap_with, types::{Blob, Bytes, Value, NULL_POINTER, TAG_BLOB_B}
};

use super::stable_function_state;

type FunctionId = u32;
type WasmTableIndex = u32;
type NameHash = u32;
type LiteralIndex = u32;

/// Part of the persistent metadata. Contains GC-managed references to blobs.
#[repr(C)]
pub struct StableFunctionState {
    /// Persistent virtual table.
    virtual_table: Value,
}

// Transient table. GC root.
static mut DYNAMIC_LITERAL_TABLE: Value = NULL_POINTER;

impl StableFunctionState {
    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_virtual_table(&mut self) -> *mut PersistentVirtualTable {
        self.virtual_table.as_blob_mut() as *mut PersistentVirtualTable
    }

    // GC root if part of the persistent metadata.
    pub fn virtual_table_location(&mut self) -> *mut Value {
        &mut self.virtual_table
    }

    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_literal_table(&mut self) -> *mut DynamicLiteralTable {
        DYNAMIC_LITERAL_TABLE.as_blob_mut() as *mut DynamicLiteralTable
    }

    // Transient GC root.
    pub unsafe fn literal_table_location(&mut self) -> *mut Value {
        &mut DYNAMIC_LITERAL_TABLE
    }
}

#[repr(C)]
struct IndexedTable<T> {
    header: Blob,
    _phantom: PhantomData<T>, // not materialized, just to use generic type.
                              // Series of `T`
}

impl <T> IndexedTable<T> {
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
type PersistentVirtualTable = IndexedTable<StableFunctionEntry>;

#[derive(Clone)]
struct StableFunctionEntry {
    wasm_table_index: WasmTableIndex,
    function_name_hash: NameHash,
}

#[no_mangle]
pub unsafe fn resolve_stable_function_call(function_id: FunctionId) -> WasmTableIndex {
    debug_assert_ne!(function_id, NULL_FUNCTION_ID);
    let virtual_table = stable_function_state().get_virtual_table();
    let table_entry = virtual_table.get(function_id as usize);
    (*table_entry).wasm_table_index
}

/// Indexed by literal index.
type DynamicLiteralTable = IndexedTable<FunctionId>;

#[no_mangle]
pub unsafe fn resolve_stable_function_literal(literal_index: LiteralIndex) -> FunctionId {
    let literal_table = stable_function_state().get_literal_table();
    let table_entry = literal_table.get(literal_index as usize);
    *table_entry
}

struct StaticFunctionEntry {
    function: StableFunctionEntry,
    /// Cache for runtime optimization.
    /// This entry is uninitialized by the compiler and the runtime system
    /// uses this space to remember matched function ids for faster lookup. 
    cached_function_id: FunctionId,
}

/// Sorted by hash name.
type StaticFunctionMap = IndexedTable<StaticFunctionEntry>;

impl StaticFunctionMap {
    unsafe fn find(self: *mut Self, name: NameHash) -> *mut StaticFunctionEntry {
        // Binary search
        let mut left = 0;
        let mut right = self.length();
        while left < right {
            let middle = (left + right) / 2;
            let entry = self.get(middle);
            if name <= (*entry).function.function_name_hash {
                right = middle;
            } else {
                left = middle + 1;
            }
        }
        if left < self.length() {
            return null_mut();
        } else {
            let entry = self.get(left);
            if (*entry).function.function_name_hash == name {
                return entry;
            } else {
                return null_mut();
            }
        }
    }
}

/// Indexed by literal index.
type StaticLiteralMap = IndexedTable<NameHash>;

#[ic_mem_fn]
pub unsafe fn update_stable_functions<M: Memory>(
    mem: &mut M,
    static_functions: *mut StaticFunctionMap,
    static_literals: *mut StaticLiteralMap,
) {
    // O(n*log(n)) runtime costs:
    // 1. Initialize all function ids in static functions to null sentinel.
    prepare_static_function_table(static_functions);
    // 2. Scan the persistent virtual table and match/update all entries against
    // `static_functions`. Assign the function ids in static function map.
    let virtual_table = stable_function_state().get_virtual_table();
    update_existing_functions(virtual_table, static_functions);
    // 3. Scan static literal app and determine number of new stable functions
    // not part of persistent virtual table.
    let extension_size = count_new_functions(static_functions, static_literals);
    // 4. Extend the persistent virtual table by the new stable functions.
    // Assign the function ids in static function map.
    let new_virtual_table = add_new_functions(mem, virtual_table, extension_size, static_functions, static_literals);
    // 5. Create the dynamic literal table by scanning the static literal
    // table and retrieving the corresponding function ids in the static function
    // table.
    let new_literal_table = create_dynamic_literal_table(mem, static_functions, static_literals);
    // 6. Store the new persistent virtual table and dynamic literal table.
    // Apply write barriers!
    let state = stable_function_state();
    write_with_barrier(mem, state.virtual_table_location(), new_virtual_table);
    write_with_barrier(mem, state.literal_table_location(), new_literal_table);
}

const NULL_FUNCTION_ID: FunctionId = FunctionId::MAX;

/// Step 1: Initialize all function ids in the static function table to null.
unsafe fn prepare_static_function_table(static_functions: *mut StaticFunctionMap) {
    for index in 0..static_functions.length() {
        let entry = static_functions.get(index);
        (*entry).cached_function_id = NULL_FUNCTION_ID;
    }
}

// Step 2: Scan the persistent virtual table and match/update all entries against
// `static_functions`. Assign the function ids in static function map.
unsafe fn update_existing_functions(
    virtual_table: *mut PersistentVirtualTable,
    static_functions: *mut StaticFunctionMap,
) {
    assert_ne!(virtual_table.length(), NULL_FUNCTION_ID as usize);
    assert!(virtual_table.length() < FunctionId::MAX as usize);
    for function_id in 0..virtual_table.length() {
        let virtual_table_entry = virtual_table.get(function_id);
        let name_hash = (*virtual_table_entry).function_name_hash;
        let static_function_entry = static_functions.find(name_hash);
        if static_function_entry == null_mut() {
            rts_trap_with(format!(200, "Incompatible upgrade: Stable function {name_hash} is missing in the new program version"));
        }
        (*virtual_table_entry).wasm_table_index = (*static_function_entry).function.wasm_table_index;
        (*static_function_entry).cached_function_id = function_id as FunctionId;
    }
}

// 3. Scan static literal app and determine number of new stable functions
// not part of persistent virtual table.    
unsafe fn count_new_functions(
    static_functions: *mut StaticFunctionMap,
    static_literals: *mut StaticLiteralMap,
) -> usize {
    let mut count = 0;
    for literal_index in 0..static_literals.length() {
        let name_hash = *static_literals.get(literal_index);
        let function_entry = static_functions.find(name_hash);
        if (*function_entry).cached_function_id == NULL_FUNCTION_ID {
            count += 1;
        }
    }
    count
}

// 4. Extend the persistent virtual table by the new stable functions.
// Assign the function ids in static function map.
unsafe fn add_new_functions<M: Memory>(
    mem: &mut M,
    old_virtual_table: *mut PersistentVirtualTable,
    new_function_count: usize,
    static_functions: *mut StaticFunctionMap,
    static_literals: *mut StaticLiteralMap,
) -> Value {
    if new_function_count == 0 {
        return Value::from_ptr(old_virtual_table as usize);
    }
    let new_length = old_virtual_table.length() + new_function_count;
    let new_blob = extend_virtual_table(mem, old_virtual_table, new_length);
    let new_virtual_table = new_blob.as_blob_mut() as *mut PersistentVirtualTable;
    let mut function_id = old_virtual_table.length();
    for literal_index in 0..static_literals.length() {
        let hash_name = *static_literals.get(literal_index);
        let static_function_entry = static_functions.find(hash_name);
        assert_ne!(static_function_entry, null_mut());
        if (*static_function_entry).cached_function_id == NULL_FUNCTION_ID {
            let new_virtual_table_entry = (*static_function_entry).function.clone();
            debug_assert_ne!(function_id, NULL_FUNCTION_ID as usize);
            new_virtual_table.set(function_id, new_virtual_table_entry);
            (*static_function_entry).cached_function_id = function_id as FunctionId;
            function_id += 1;
        }
    }
    debug_assert_eq!(function_id, new_virtual_table.length());
    new_blob
}

unsafe fn extend_virtual_table<M: Memory>(mem: &mut M, old_virtual_table: *mut PersistentVirtualTable, new_length: usize) -> Value {
    debug_assert!(new_length > old_virtual_table.length());
    let new_blob = alloc_blob(mem, TAG_BLOB_B, Bytes(new_length * PersistentVirtualTable::get_entry_size()));
    allocation_barrier(new_blob);
    let new_virtual_table = new_blob.as_blob_mut() as *mut PersistentVirtualTable;
    for index in 0..old_virtual_table.length() {
        let old_entry = old_virtual_table.get(index);
        new_virtual_table.set(index, (*old_entry).clone());
    }
    // New entries will be initialized by caller (`add_new_functions`).
    new_blob
}

// 5. Create the dynamic literal table by scanning the static literal
// table and retrieving the corresponding function ids in the static function
// table.
unsafe fn create_dynamic_literal_table<M: Memory>(mem: &mut M, static_functions: *mut StaticFunctionMap, static_literals: *mut StaticLiteralMap) -> Value {
    let new_length = static_literals.length() * DynamicLiteralTable::get_entry_size();
    let new_blob = alloc_blob(mem, TAG_BLOB_B, Bytes(new_length));
    allocation_barrier(new_blob);
    let dynamic_literal_table = new_blob.as_blob_mut() as *mut DynamicLiteralTable;
    for literal_index in 0..static_literals.length() {
        let name_hash = *static_literals.get(literal_index);
        let function_entry = static_functions.find(name_hash);
        let function_id = (*function_entry).cached_function_id;
        assert_ne!(function_id, NULL_FUNCTION_ID);
        dynamic_literal_table.set(literal_index, function_id);
    }
    new_blob
}
