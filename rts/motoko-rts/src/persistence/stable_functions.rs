//! Support for stable functions during persistence.
//!
//! A stable function is a named local function in a stable scope,
//! closing over variables of stable type.
//!
//! A stable scope is:
//! * the main actor
//! * an imported module,
//! * a named function in a stable scope,
//! * a class in a stable scope,
//! * a named object in a stable scope.
//!
//! A stable function is also a stable type.
//!
//! Syntactically, function types are prefixed by `stable` to denote a stable function, e.g.
//! `stable X -> Y`. Stable functions implicitly have a corresponding stable reference type.
//!
//! A stable functions are upgraded as follows:
//! * They map to stable functions of equal fully qualified name in the new program version.
//! * Their function type in the new version need to be compatible with the previous version (super-type).
//! * Their closure type in the new version must be compatible with the previous version (super-type).
//!
//! All other functions, such as lambdas, or named functions in a lambda, are flexible
//! functions. A stable function type is a sub-type of a flexible function type with
//! type-compatible signature, i.e. `stable X' -> Y <: X -> Y'` for `X' <: X` and `Y' :< Y`.
//!
//! Function references are encoded by a function ids in the following representation:
//! * Stable function id, encoded as non-negative number:
//!   A stable function reference that stays invariant across upgrades.
//! * Flexible functiion id, encoded as negative number:
//!   A flexible function reference that is invalidated on upgrade.
//!
//! Each program version defines a set of named local functions that can be used as
//! stable function references. Each such function obtains a stable function id on
//! program initialization and upgrade. If the stable function was already declared in
//! the previous version, its function id is reused on upgrade. Thereby, the compatibility
//! of the function type and closure type are checked. Otherwise, if it is a new
//! stable function, it obtains a new stable function id, or in the future, a recycled id.
//!
//! The runtime system supports stable functions by two mechanisms:
//!
//! 1. **Persistent virtual table** for stable function calls:
//!     
//!    The persistent virtual table maps stable function ids to Wasm table indices, for
//!    supporting dynamic calls of stable functions. Each entry also stores the hashed name
//!    of the stable function to match and rebind the stable function ids to the corresponding
//!    functions of the new Wasm binary on a program upgrade. Moreover, each entry also records
//!    the type of the closure, referring to the persistent type table. The table survives
//!    upgrades and is built and updated by the runtime system. To build and update the persistent
//!    virtual table, the compiler provides a **stable function map**, mapping the hashed name of a
//!    potentially stable function to the corresponding Wasm table index, plus its closure type
//!    pointing to the new type table. For performance, the stable function map is sorted by the hashed names.
//!    
//! 2. **Function literal table** for materializing stable function literals:
//!
//!    As the compiler does not yet know the function ids of stable function literals/constants,
//!    this table maps a Wasm table index of the current program version to a stable function id.   
//!    The function literal table is re-built on program initialization and upgrade. When a stable
//!    function literal is loaded, it serves for resolving the corresponding function id and thus
//!    the stable function reference. The table is discarded on upgrades and (re-)constructed by
//!    the runtime system, based on the information of the **stable function map**.
//!
//! The runtime system distinguishes between flexible and stable function references by using a
//! different encoding. This is to avoid complicated conversion logic been inserted by the compiler
//! when a stable function reference is assigned to flexible reference, in particular in the presence
//! of sharing (a function reference can be reached by both a stable and flexible function type) and
//! composed types (function references can be deeply nested in a composed value that is assigned).
//!
//! Stable function compatibility check is performed by the runtime system on upgrade.
//! * It checks for a matching function in the new version.
//! * The function type compatibility is implicitly covered by the upgrade memory compatibility
//!   check, since the stable function in use needs to be reachable by the stable actor type.
//! * The closure compatibility is additionally checked for each mapped stable function. This
//!   covers all captured variables of the stable function. This check is supported by the
//!   information of the persistent virtual table and the stable function map.
//
//! Flexible function references are represented as negative function ids determining the Wasm
//! table index, specifically `-wasm_table_index - 1`.
//!
//! Potential garbage collection in the future:
//! * The runtime system could allow discarding old stable functions that are unused, i.e. when it is
//!   no longer stored in a live object and no longer part of the stable function map.
//! * Free function ids can be recycled for new stable functions in persistent virtual table.
//! * Once freed, a new program version is liberated from providing a matching stable function.

use core::{marker::PhantomData, mem::size_of, ptr::null_mut, str::from_utf8};

use crate::{
    algorithms::SortedArray,
    barriers::{allocation_barrier, write_with_barrier},
    memory::{alloc_blob, Memory},
    rts_trap_with,
    types::{Blob, Bytes, Value, NULL_POINTER, TAG_BLOB_B},
};

use super::{compatibility::MemoryCompatibilityTest, stable_function_state};

// Use `usize` or `isize` instead of `u32` and `i32` to avoid unwanted padding on Memory64.
// E.g. struct sizes will be rounded to 64-bit.
type WasmTableIndex = usize;
type NameHash = usize;
type TypeIndex = isize;

type FunctionId = isize;

const NULL_FUNCTION_ID: FunctionId = FunctionId::MAX;

fn is_flexible_function_id(function_id: FunctionId) -> bool {
    function_id < 0
}

fn resolve_flexible_function_id(function_id: FunctionId) -> WasmTableIndex {
    debug_assert!(is_flexible_function_id(function_id));
    (-function_id - 1) as WasmTableIndex
}

fn resolve_stable_function_id(function_id: FunctionId) -> usize {
    debug_assert!(!is_flexible_function_id(function_id));
    debug_assert_ne!(function_id, NULL_FUNCTION_ID);
    function_id as usize
}

fn to_flexible_function_id(wasm_table_index: WasmTableIndex) -> FunctionId {
    debug_assert!(wasm_table_index < FunctionId::MAX as WasmTableIndex);
    -(wasm_table_index as FunctionId) - 1
}

/// Part of the persistent metadata. Contains GC-managed references to blobs.
#[repr(C)]
pub struct StableFunctionState {
    /// Persistent virtual table.
    virtual_table: Value,
}

// Transient table. GC root.
static mut FUNCTION_LITERAL_TABLE: Value = NULL_POINTER;

// Zero memory map, as seen in the initial persistent Wasm memory.
const DEFAULT_VALUE: Value = Value::from_scalar(0);

impl StableFunctionState {
    // No dynamic allocations allowed at this point (persistence startup).
    pub fn default() -> Self {
        Self {
            virtual_table: DEFAULT_VALUE,
        }
    }

    pub fn is_default(&self) -> bool {
        self.virtual_table == DEFAULT_VALUE
    }

    unsafe fn initialize_virtual_table<M: Memory>(&mut self, mem: &mut M) {
        assert_eq!(self.virtual_table, DEFAULT_VALUE);
        let initial_virtual_table = PersistentVirtualTable::new(mem);
        write_with_barrier(mem, self.virtual_table_location(), initial_virtual_table);
    }

    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_virtual_table(&mut self) -> *mut PersistentVirtualTable {
        assert_ne!(self.virtual_table, DEFAULT_VALUE);
        assert_ne!(self.virtual_table, NULL_POINTER);
        self.virtual_table.as_blob_mut() as *mut PersistentVirtualTable
    }

    // GC root if part of the persistent metadata.
    pub fn virtual_table_location(&mut self) -> *mut Value {
        &mut self.virtual_table
    }

    /// The returned low-level pointer can only be used within the same IC message.
    unsafe fn get_literal_table(&mut self) -> *mut FunctionLiteralTable {
        assert_ne!(FUNCTION_LITERAL_TABLE, NULL_POINTER);
        FUNCTION_LITERAL_TABLE.as_blob_mut() as *mut FunctionLiteralTable
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

impl PersistentVirtualTable {
    unsafe fn new<M: Memory>(mem: &mut M) -> Value {
        let blob = alloc_blob(mem, TAG_BLOB_B, Bytes(0));
        allocation_barrier(blob);
        blob
    }
}

#[repr(C)]
#[derive(Clone)]
struct VirtualTableEntry {
    function_name_hash: NameHash,
    closure_type_index: TypeIndex, // Referring to the persisted type table.
    wasm_table_index: WasmTableIndex,
}

/// Determine the Wasm table index for a function call (stable or flexible function).
#[no_mangle]
pub unsafe fn resolve_function_call(function_id: FunctionId) -> WasmTableIndex {
    if is_flexible_function_id(function_id) {
        return resolve_flexible_function_id(function_id);
    }
    debug_assert_ne!(function_id, NULL_FUNCTION_ID);
    let virtual_table = stable_function_state().get_virtual_table();
    let table_entry = virtual_table.get(resolve_stable_function_id(function_id));
    (*table_entry).wasm_table_index
}

/// Indexed by Wasm table index.
type FunctionLiteralTable = IndexedTable<FunctionId>;

/// Determine the function id for Wasm table index (stable or flexible function).
#[no_mangle]
pub unsafe fn resolve_function_literal(wasm_table_index: WasmTableIndex) -> FunctionId {
    let literal_table = stable_function_state().get_literal_table();
    let function_id = if wasm_table_index < literal_table.length() {
        *literal_table.get(wasm_table_index)
    } else {
        NULL_FUNCTION_ID
    };
    if function_id == NULL_FUNCTION_ID {
        return to_flexible_function_id(wasm_table_index);
    }
    function_id
}

#[repr(C)]
struct StableFunctionEntry {
    function_name_hash: NameHash,
    wasm_table_index: WasmTableIndex,
    // Referring to the type table of the new prorgram version.
    closure_type_index: TypeIndex,
    /// Cache for runtime optimization.
    /// This entry is uninitialized by the compiler and the runtime system
    /// uses this space to remember matched function ids for faster lookup.
    cached_function_id: FunctionId,
}

/// Sorted by hash name.
type StableFunctionMap = IndexedTable<StableFunctionEntry>;

impl SortedArray<NameHash> for *mut StableFunctionMap {
    fn get_length(&self) -> usize {
        unsafe { self.length() }
    }

    fn value_at(&self, index: usize) -> NameHash {
        unsafe {
            let entry = self.get(index);
            (*entry).function_name_hash
        }
    }
}

impl StableFunctionMap {
    unsafe fn find(self: *mut Self, name: NameHash) -> *mut StableFunctionEntry {
        match self.index_of(name) {
            None => null_mut(),
            Some(index) => self.get(index),
        }
    }
}

/// Called on program initialization and on upgrade, both during EOP and graph copy.
pub unsafe fn register_stable_functions<M: Memory>(
    mem: &mut M,
    stable_functions_map: Value,
    type_test: Option<&MemoryCompatibilityTest>,
) {
    let stable_functions = stable_functions_map.as_blob_mut() as *mut StableFunctionMap;
    // O(n*log(n)) runtime costs:
    // 1. Initialize all function ids in stable functions map to null sentinel.
    prepare_stable_function_map(stable_functions);
    // 2. Retrieve the persistent virtual, or, if not present, initialize an empty one.
    let virtual_table = prepare_virtual_table(mem);
    // 3. Scan the persistent virtual table and match/update all entries against
    // `stable_functions_map`. Check the compatibility of the closure types.
    // Assign the function ids in stable function map.
    update_existing_functions(virtual_table, stable_functions, type_test);
    // 4. Scan stable functions map and determine number of new stable functions that are yet
    // not part of the persistent virtual table.
    let extension_size = count_new_functions(stable_functions);
    // 5. Extend the persistent virtual table by the new stable functions.
    // Assign the function ids in stable function map.
    let new_virtual_table = add_new_functions(mem, virtual_table, extension_size, stable_functions);
    // 6. Create the function literal table by scanning the stable functions map and
    // mapping Wasm table indices to their assigned function id.
    let new_literal_table = create_function_literal_table(mem, stable_functions);
    // 7. Store the new persistent virtual table and function literal table.
    // Apply write barriers!
    let state = stable_function_state();
    write_with_barrier(mem, state.virtual_table_location(), new_virtual_table);
    write_with_barrier(mem, state.literal_table_location(), new_literal_table);
}

/// Step 1: Initialize all function ids in the stable function map to null.
unsafe fn prepare_stable_function_map(stable_functions: *mut StableFunctionMap) {
    for index in 0..stable_functions.length() {
        let entry = stable_functions.get(index);
        (*entry).cached_function_id = NULL_FUNCTION_ID;
    }
}

// Step 2. Retrieve the persistent virtual, or, if not present, initialize an empty one.
unsafe fn prepare_virtual_table<M: Memory>(mem: &mut M) -> *mut PersistentVirtualTable {
    let state = stable_function_state();
    if state.is_default() {
        state.initialize_virtual_table(mem);
    }
    state.get_virtual_table()
}

// Step 3: Scan the persistent virtual table and match/update all entries against
// `stable_functions_map`. Check the compatibility of the closure types.
// Assign the function ids in stable function map.
unsafe fn update_existing_functions(
    virtual_table: *mut PersistentVirtualTable,
    stable_functions: *mut StableFunctionMap,
    type_test: Option<&MemoryCompatibilityTest>,
) {
    assert_ne!(virtual_table.length(), NULL_FUNCTION_ID as usize);
    for function_id in 0..virtual_table.length() {
        let virtual_table_entry = virtual_table.get(function_id);
        let name_hash = (*virtual_table_entry).function_name_hash;
        let stable_function_entry = stable_functions.find(name_hash);
        if stable_function_entry == null_mut() {
            let buffer = format!(200, "Incompatible upgrade: Stable function {name_hash} is missing in the new program version");
            let message = from_utf8(&buffer).unwrap();
            rts_trap_with(message);
        }
        let old_closure = (*virtual_table_entry).closure_type_index;
        let new_closure = (*stable_function_entry).closure_type_index;
        assert!(old_closure >= i32::MIN as TypeIndex && old_closure <= i32::MAX as TypeIndex);
        assert!(new_closure >= i32::MIN as TypeIndex && new_closure <= i32::MAX as TypeIndex);
        if type_test.is_some_and(|test| !test.is_compatible(old_closure as i32, new_closure as i32))
        {
            let buffer = format!(
                200,
                "Memory-incompatible closure type of stable function {name_hash}"
            );
            let message = from_utf8(&buffer).unwrap();
            rts_trap_with(message);
        }
        (*virtual_table_entry).wasm_table_index = (*stable_function_entry).wasm_table_index;
        (*virtual_table_entry).closure_type_index = new_closure;
        (*stable_function_entry).cached_function_id = function_id as FunctionId;
    }
}

// Step 4. Scan stable functions map and determine number of new stable functions that are yet
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

// Step 5. Extend the persistent virtual table by the new stable functions.
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
    let mut function_id = old_virtual_table.length() as FunctionId;
    for index in 0..stable_functions.length() {
        let stable_function_entry = stable_functions.get(index);
        assert_ne!(stable_function_entry, null_mut());
        if (*stable_function_entry).cached_function_id == NULL_FUNCTION_ID {
            let function_name_hash = (*stable_function_entry).function_name_hash;
            let closure_type_index = (*stable_function_entry).closure_type_index;
            let wasm_table_index = (*stable_function_entry).wasm_table_index;
            let new_virtual_table_entry = VirtualTableEntry {
                function_name_hash,
                closure_type_index,
                wasm_table_index,
            };
            debug_assert!(!is_flexible_function_id(function_id));
            debug_assert_ne!(function_id, NULL_FUNCTION_ID);
            new_virtual_table.set(
                resolve_stable_function_id(function_id),
                new_virtual_table_entry,
            );
            (*stable_function_entry).cached_function_id = function_id;
            function_id += 1;
        }
    }
    debug_assert_eq!(function_id as usize, new_virtual_table.length());
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

// Step 6. Create the function literal table by scanning the stable functions map and
// mapping Wasm table indices to their assigned function id.
unsafe fn create_function_literal_table<M: Memory>(
    mem: &mut M,
    stable_functions: *mut StableFunctionMap,
) -> Value {
    let table_length = compute_literal_table_length(stable_functions);
    let function_literal_table = create_empty_literal_table(mem, table_length);
    for index in 0..stable_functions.length() {
        let entry = stable_functions.get(index);
        let wasm_table_index = (*entry).wasm_table_index;
        let function_id = (*entry).cached_function_id; // Can also be `NULL_FUNCTION_ID` if not stable.
        function_literal_table.set(wasm_table_index, function_id);
    }
    Value::from_ptr(function_literal_table as usize)
}

unsafe fn create_empty_literal_table<M: Memory>(
    mem: &mut M,
    table_length: usize,
) -> *mut FunctionLiteralTable {
    let byte_length = Bytes(table_length * FunctionLiteralTable::get_entry_size());
    let new_blob = alloc_blob(mem, TAG_BLOB_B, byte_length);
    allocation_barrier(new_blob);
    let function_literal_table = new_blob.as_blob_mut() as *mut FunctionLiteralTable;
    for index in 0..function_literal_table.length() {
        function_literal_table.set(index, NULL_FUNCTION_ID);
    }
    function_literal_table
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
