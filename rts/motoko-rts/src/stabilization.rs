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

use motoko_rts_macros::ic_mem_fn;

use core::cmp::min;

use crate::{
    memory::Memory,
    rts_trap_with,
    stabilization::layout::{deserialize, serialize},
    stable_mem::{self, ic0_stable64_write, PAGE_SIZE},
    types::{FwdPtr, Tag, Value, TAG_CLOSURE, TAG_FWD_PTR}, visitor::visit_pointer_fields,
};

use self::{
    layout::{scan_serialized, StableValue, STABLE_NULL_POINTER, StableToSpace},
    stable_memory_access::StableMemoryAccess,
    stable_memory_stream::{ScanStream, StableMemoryStream},
};

#[cfg(feature = "ic")]
mod compatibility;
#[cfg(feature = "ic")]
mod metadata;

mod layout;

pub mod stable_memory_access;
pub mod stable_memory_stream;

extern "C" {
    pub fn moc_null_singleton() -> Value;
}

/// Generic graph copy from main memory (from-space) to stable memory (to-space).
/// The direction of copying is fixed but the memory layout used in the from-space
/// and the to-space flips when switching between serialization and deserialization.
/// `S`: Source address type (from-space, main memory).
/// `T`: Target address type (to-space, stable memory).
/// `P`: Pointer encoding type (e.g. `u32` or `u64`).
/// During serialization:
/// * Main memory = main memory layout, S = Value.
/// * Stable memory = stable memory layout, T = StableMemoryAddress.
/// During derialization:
/// * Main memory = stable memory layout, S = StableMemoryAddress.
/// * Stable memory = main memory layout, T = Value.
trait GraphCopy<S: Copy, T: Copy, P: Copy + Default> {
    /// Run the entire graph copy algorithm: Copy the object graph reachable from the `root` pointer.
    fn run(&mut self, root: S) {
        self.evacuate(root);
        while !self.scan_completed() {
            self.scan();
        }
    }

    fn scan_completed(&self) -> bool;

    /// Lazy evacuation of a single object.
    /// Triggered for each pointer that is patched in the `scan()` function.
    /// Determines whether the object has already been copied before, and if not,
    /// copies it to the target space.
    /// Returns the new target address of the object.
    fn evacuate(&mut self, object: S) -> T {
        match self.get_forward_address(object) {
            Some(target) => target,
            None => {
                let target = self.copy(object);
                self.set_forward_address(object, target);
                target
            }
        }
    }

    /// Check if the object has been forwarded.
    /// Returns `None` if not forwarded, or otherwise, the new target address.
    fn get_forward_address(&self, object: S) -> Option<T>;

    /// Mark the object as forwarded and record its new target address.
    fn set_forward_address(&mut self, object: S, target: T);

    /// Allocate the object in the to-space by bumping the `free` pointer.
    /// Copy its content to that target location using the encoding of the target layout.
    /// Notes:
    /// * The pointer values in the field are retained as source addresses.
    /// * The source and target layout must use the same size for addresses, e.g. 32-bit.
    /// * The allocator must be contiguously growing. Free space must be inserted when the
    ///   allocator uses internal fragmentation, e.g. for the partitioned heap.
    fn copy(&mut self, object: S) -> T;

    /// Read an object at the `scan` position in the to-space, and patch all the pointer fields
    /// by translating the source pointer to the corresponding new target pointer by calling
    /// `evacuate()`.
    fn scan(&mut self);
}

const NON_STABLE_OBJECT_TAGS: [Tag; 1] = [TAG_CLOSURE];

// Dummy value used for non-stable objects that are potentially reachable from
// stable variable because of structural subtyping or `Any`-subtyping.
// Must be a non-skewed value such that the GC also ignores this value.
const DUMMY_VALUE: StableValue = StableValue::from_raw(0);

pub struct Serialization {
    to_space: StableMemoryStream,
}

impl Serialization {
    /// Notes:
    /// - Invalidates the heap by replacing reachable stable object by forwarding objects:
    /// The heap is finally no longer usable by mutator or GC.
    /// - `copy` and partially also `scan` depends on the heap layout. Adjust these functions
    /// whenever the heap layout is changed.
    pub fn run(root: Value, stable_start: u64) -> u64 {
        let to_space = StableMemoryStream::open(stable_start);
        let mut serialization = Serialization { to_space };
        serialization.run(root);
        serialization.to_space.close();
        serialization.to_space.written_length()
    }

    fn is_null(field_value: Value) -> bool {
        unsafe {
            debug_assert!(!moc_null_singleton().is_forwarded());
        }
        field_value.is_ptr() && field_value == unsafe { moc_null_singleton() }
    }

    fn encode_null() -> StableValue {
        STABLE_NULL_POINTER
    }

    /// Resolve the Brooks forwarding pointer of the incremental GC by considering potential
    /// forwarding objects (`FwdPtr`) used in Cheney's algorithm for stabilization.
    unsafe fn resolve_gc_forwarding(object: Value) -> Value {
        let tag = Self::read_object_tag(object);
        if tag == TAG_FWD_PTR {
            object
        } else {
            object.forward()
        }
    }
    /// Read the object tag by considering potential forwarding objects (`FwdPtr`).
    unsafe fn read_object_tag(object: Value) -> Tag {
        // Do not call `tag()` as it dereferences the Brooks forwarding pointer of the incremental GC,
        // which does not exist for the forwarding objects (`FwdPtr`) used by the Cheney's algorithm.
        *(object.get_ptr() as *const Tag)
    }

    fn has_non_stable_type(old_field: Value) -> bool {
        unsafe { old_field.is_ptr() && NON_STABLE_OBJECT_TAGS.contains(&old_field.tag()) }
    }

    fn written_length(&self) -> u64 {
        self.to_space.written_length()
    }
}

impl GraphCopy<Value, StableValue, u32> for Serialization {
    fn get_forward_address(&self, object: Value) -> Option<StableValue> {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            let tag = Self::read_object_tag(object);
            match tag {
                TAG_FWD_PTR => {
                    let new_location = (*(object.get_ptr() as *mut FwdPtr)).fwd;
                    Some(StableValue::serialize(new_location))
                }
                _ => None,
            }
        }
    }

    fn set_forward_address(&mut self, object: Value, target: StableValue) {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            debug_assert!(object.is_obj());
            let fwd = object.get_ptr() as *mut FwdPtr;
            (*fwd).tag = TAG_FWD_PTR;
            (*fwd).fwd = target.deserialize();
        }
    }

    fn copy(&mut self, object: Value) -> StableValue {
        unsafe {
            let object = Self::resolve_gc_forwarding(object);
            debug_assert!(object.is_obj());
            let address = self.to_space.written_length();
            serialize(&mut self.to_space, object);
            StableValue::from_address(address)
        }
    }

    fn scan(&mut self) {
        scan_serialized(self, &|context, original| {
            let old_value = original.deserialize();
            if Self::is_null(old_value) {
                Self::encode_null()
            } else if old_value.is_ptr() {
                // Due to structural subtyping or `Any`-subtyping, a non-stable object (such as a closure) may be
                // be dynamically reachable from a stable varibale. The value is not accessible in the new program version.
                // Therefore, the content of these fields can serialized with a dummy value that is also ignored by the GC.
                if Self::has_non_stable_type(old_value) {
                    DUMMY_VALUE
                } else {
                    context.evacuate(old_value)
                }
            } else {
                original
            }
        });
    }

    fn scan_completed(&self) -> bool {
        self.to_space.scan_completed()
    }
}

impl StableToSpace for Serialization {
    fn to_space(&mut self) -> &mut StableMemoryStream {
        &mut self.to_space
    }
}

pub struct Deserialization<'a, M: Memory> {
    mem: &'a mut M,
    from_space: StableMemoryAccess,
    scan_address: Option<usize>,
}

impl<'a, M: Memory> Deserialization<'a, M> {
    /// Notes:
    /// - CAUTION: Linearly writes the stable memory at the heap end. Does not work if the partitioned
    /// heap inlines metadata inside the partitions.
    /// - Invokes the heap allocator to compute the future object addresses in the heap.
    /// However, the allocator must not yet write to the heap.
    pub fn run(mem: &'a mut M, stable_start: u64, stable_size: u64, heap_start: usize) -> Value {
        let from_space = StableMemoryAccess::open(stable_start, stable_size);
        Deserialization {
            mem,
            from_space,
            scan_address: Some(heap_start),
        }
        .run(StableValue::serialize(Value::from_ptr(0)));
        clear_stable_memory(stable_start, stable_size);
        Value::from_ptr(heap_start)
    }

    fn is_null(value: StableValue) -> bool {
        value == STABLE_NULL_POINTER
    }

    fn encode_null() -> Value {
        unsafe { moc_null_singleton() }
    }

    fn next_object(address: usize) -> Option<usize> {
        todo!()
    }


    unsafe fn scan_deserialized<C, F: Fn(&mut C, Value) -> Value>(
        context: &mut C,
        target_object: Value,
        translate: &F,
    ) {
        assert!(target_object.is_obj());
        visit_pointer_fields(
            context,
            target_object.as_obj(),
            target_object.tag(),
            0,
            |context, field_address| {
                *field_address = translate(context, *field_address);
            },
            |_, _, array| array.len(),
        );
    }
}

impl<'a, M: Memory> GraphCopy<StableValue, Value, u32> for Deserialization<'a, M> {
    fn get_forward_address(&self, stable_object: StableValue) -> Option<Value> {
        let address = stable_object.to_address();
        let tag = self.from_space.read::<Tag>(address);
        match tag {
            TAG_FWD_PTR => {
                let forward_object = self.from_space.read::<FwdPtr>(address);
                Some(forward_object.fwd)
            }
            _ => None,
        }
    }

    fn set_forward_address(&mut self, stable_object: StableValue, target: Value) {
        let address = stable_object.to_address();
        let forward_object = FwdPtr {
            tag: TAG_FWD_PTR,
            fwd: target,
        };
        self.from_space.write(address, &forward_object);
    }

    fn copy(&mut self, stable_object: StableValue) -> Value {
        unsafe { deserialize(self.mem, &mut self.from_space, stable_object) }
    }

    /// Note:
    /// * The deserialized memory may contain free space at a partition end.
    fn scan(&mut self) {
        let address = self.scan_address.unwrap();
        let target_object = Value::from_ptr(address);
        unsafe {
            Self::scan_deserialized(self, target_object, &|context, original| {
                let old_value = StableValue::serialize(original);
                if Self::is_null(old_value) {
                    Self::encode_null()
                } else if original.is_ptr() {
                    context.evacuate(old_value)
                } else {
                    original
                }
            });
        }
        self.scan_address = Self::next_object(address);
    }

    fn scan_completed(&self) -> bool {
        self.scan_address.is_none()
    }
}

fn clear_stable_memory(start: u64, length: u64) {
    const CHUNK_SIZE: usize = 1024;
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
    assert!(byte_size < u64::MAX - PAGE_SIZE - 1);
    let required_pages = (byte_size + PAGE_SIZE - 1) / PAGE_SIZE;
    let available_pages = stable_mem::size();
    if required_pages > available_pages {
        let additional_pages = required_pages - available_pages;
        assert_ne!(additional_pages, u64::MAX);
        let result = stable_mem::grow(additional_pages);
        if result == u64::MAX {
            unsafe {
                rts_trap_with("Insufficient stable memory");
            }
        }
    }
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
    use crate::stabilization::metadata::StabilizationMetadata;
    use compatibility::TypeDescriptor;

    let stable_memory_pages = stable_mem::size();
    let serialized_data_start = stable_memory_pages * PAGE_SIZE;
    let serialized_data_length = Serialization::run(stable_actor, serialized_data_start);
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
/// /// Traps if the stable state is incompatible with the new program version and the upgrade is not
/// possible.
/// Implementation:
/// * Algorithm:
///     1. Copy the serialized image from stable memory into main memory.
///     2. Cheney's algorithm using main memory (encoded in stable layout) as from-space and
///        stable memory (encoded in main memory layout) as to-space.
///     3. Copy the deserialized image from stable memory back into main memory.
///   (This multi-step approach serves for avoiding random accesses to stable memory and thus
///    minimizing the expensive API calls on stable memory.)
/// * Encoding: The from-space uses the stable memory layout (although located in main memory),
///   while the to-space is to be encoded in main memory layout (although located in stable memory).
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in the
///   stable object graph layout (see `GraphCopyStabilization.md`).
#[ic_mem_fn(ic_only)]
pub unsafe fn destabilize<M: Memory>(
    mem: &mut M,
    new_candid_data: Value,
    new_type_offsets: Value,
) -> Value {
    use crate::{memory::ic::dynamic_heap_end, rts_trap_with, stable_mem::moc_stable_mem_set_size};
    use compatibility::{memory_compatible, TypeDescriptor};
    use metadata::StabilizationMetadata;

    let mut new_type_descriptor = TypeDescriptor::new(new_candid_data, new_type_offsets, 0);
    let metadata = StabilizationMetadata::load(mem);
    let mut old_type_descriptor = metadata.type_descriptor;
    if !memory_compatible(mem, &mut old_type_descriptor, &mut new_type_descriptor) {
        rts_trap_with("Memory-incompatible program upgrade");
    }
    // There may already exist some objects before destabilization.
    // We preserve them by continuing the heap at that location.
    // Note: This only works if the preceding allocations are all contiguous,
    // i.e. no occupied partition resides above this position and
    // no free partition resides below this position.
    let heap_deserialzation_start = dynamic_heap_end();
    let stable_root = Deserialization::run(
        mem,
        metadata.serialized_data_start,
        metadata.serialized_data_length,
        heap_deserialzation_start,
    );
    moc_stable_mem_set_size(metadata.stable_memory_pages);
    stable_root
}

#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn use_new_destabilization() -> bool {
    metadata::StabilizationMetadata::matching_version()
}
