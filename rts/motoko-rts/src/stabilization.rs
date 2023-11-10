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
//! See `GraphCopyStabilization.md` for the stable layout specification and the employed algorithm.

use core::fmt::Debug;

use crate::{
    constants::WORD_SIZE,
    stable_mem::ic0_stable64_read,
    types::{is_skewed, skew, unskew, FwdPtr, Tag, Value, TAG_ARRAY, TAG_FWD_PTR, TAG_MUTBOX},
};

use self::reader_writer::{ScanStream, StableMemorySpace, WriteStream};

pub mod reader_writer;

/// Address in stable memory.
#[derive(Clone, Copy, Debug, PartialEq)]
struct StableMemoryAddress(usize);

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
    fn to_space(&mut self) -> &mut StableMemorySpace;

    /// Run the entire graph copy algorithm: Copy the object graph reachable from the `root` pointer.
    /// Return the final to-space size.
    fn run(&mut self, root: S) -> u64 {
        self.evacuate(root);
        while !self.to_space().scan_completed() {
            self.scan(|context| context.patch_field());
        }
        self.to_space().close();
        self.to_space().written_length()
    }

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

    /// Allocate the object in the to-space by bumbing the `free` pointer.
    /// Copy its content to that target location using the encoding of the target layout.
    /// Note: The pointer values in the field are retained as source addresses.
    fn copy(&mut self, object: S) -> T;

    /// Read an object at the `scan` position in the to-space, process all its potential
    /// pointer fields by invoking `visit_field` for each of them.
    fn scan<F: Fn(&mut Self)>(&mut self, visit_field: F);

    fn patch_field(&mut self) {
        let old_field = self.to_space().read();
        if self.is_pointer(old_field) {
            let object = self.decode_pointer(old_field);
            let target = self.evacuate(object);
            let new_field = self.encode_pointer(target);
            self.to_space().update(&new_field);
        }
    }

    /// Determine whether field value denotes a pointer.
    fn is_pointer(&self, field_value: P) -> bool;
    /// Decode a source pointer from field value.
    fn decode_pointer(&self, field_value: P) -> S;
    /// Encode a target address as a field value.
    fn encode_pointer(&self, target: T) -> P;
}

pub struct Serialization {
    to_space: StableMemorySpace,
}

impl Serialization {
    pub fn run(root: Value, stable_start: u64) -> u64 {
        let to_space = StableMemorySpace::open(stable_start);
        Serialization { to_space }.run(root)
    }
}

impl GraphCopy<Value, StableMemoryAddress, u32> for Serialization {
    fn to_space(&mut self) -> &mut StableMemorySpace {
        &mut self.to_space
    }

    // TODO: Redesign `FwdPtr` to better fit the graph copying, e.g. use raw pointer than `Value` field.
    fn get_forward_address(&self, object: Value) -> Option<StableMemoryAddress> {
        unsafe {
            let object = object.forward();
            match object.tag() {
                TAG_FWD_PTR => {
                    let new_address = (*(object.get_ptr() as *mut FwdPtr)).fwd.get_raw();
                    Some(StableMemoryAddress(new_address as usize))
                }
                _ => None,
            }
        }
    }

    fn set_forward_address(&mut self, object: Value, target: StableMemoryAddress) {
        unsafe {
            let object = object.forward();
            debug_assert_ne!(object.tag(), TAG_FWD_PTR);
            debug_assert!(object.is_obj());
            let fwd = object.get_ptr() as *mut FwdPtr;
            (*fwd).tag = TAG_FWD_PTR;
            (*fwd).fwd = Value::from_raw(target.0 as u32);
        }
    }

    fn copy(&mut self, object: Value) -> StableMemoryAddress {
        unsafe {
            let address = self.to_space.written_length();
            match object.tag() {
                TAG_ARRAY => {
                    let array = object.as_array();
                    self.to_space.write(&TAG_ARRAY);
                    let array_length = (*array).len;
                    self.to_space.write(&array_length);
                    let payload_length = (array_length * WORD_SIZE) as usize;
                    self.to_space
                        .raw_write(array.payload_addr() as usize, payload_length);
                }
                TAG_MUTBOX => {
                    let mutbox = object.as_mutbox();
                    self.to_space.write(&TAG_MUTBOX);
                    self.to_space.write(&(*mutbox).field);
                }
                other_tag => unimplemented!("tag {other_tag}"),
            }
            StableMemoryAddress(address as usize)
        }
    }

    fn scan<F: Fn(&mut Self)>(&mut self, visit_pointer: F) {
        let tag = self.to_space.read::<Tag>();
        match tag {
            TAG_ARRAY => {
                let array_length = self.to_space.read::<u32>();
                // TOOD: Optimize in chunked visiting
                for _ in 0..array_length {
                    visit_pointer(self);
                }
            }
            TAG_MUTBOX => {
                visit_pointer(self);
            }
            other_tag => unimplemented!("tag {other_tag}"),
        }
    }

    fn is_pointer(&self, field_value: u32) -> bool {
        is_skewed(field_value)
    }

    fn decode_pointer(&self, field_value: u32) -> Value {
        Value::from_raw(field_value)
    }

    fn encode_pointer(&self, target: StableMemoryAddress) -> u32 {
        // Again skew the target pointer, such that deserialization can also identify it as pointer.
        skew(target.0) as u32
    }
}

struct Deserialization {
    to_space: StableMemorySpace,
    heap_base: usize,
}

impl Deserialization {
    fn run(stable_start: u64, stable_size: u64, heap_base: usize) -> u64 {
        Self::stable_memory_bulk_copy(stable_start, stable_size, heap_base);
        let to_space = StableMemorySpace::open(stable_start);
        let new_size = Deserialization {
            to_space,
            heap_base,
        }
        .run(StableMemoryAddress(0));
        Self::stable_memory_bulk_copy(stable_start, new_size, heap_base);
        new_size
    }

    fn stable_memory_bulk_copy(stable_start: u64, stable_size: u64, heap_base: usize) {
        unsafe {
            ic0_stable64_read(heap_base as u64, stable_start, stable_size);
        }
    }
}

impl GraphCopy<StableMemoryAddress, Value, u32> for Deserialization {
    fn to_space(&mut self) -> &mut StableMemorySpace {
        &mut self.to_space
    }

    fn get_forward_address(&self, object: StableMemoryAddress) -> Option<Value> {
        let source = object.0 + self.heap_base;
        unsafe {
            let tag = *(source as *mut Tag);
            match tag {
                TAG_FWD_PTR => {
                    let target = (*(source as *mut FwdPtr)).fwd;
                    Some(target)
                }
                _ => None,
            }
        }
    }

    fn set_forward_address(&mut self, object: StableMemoryAddress, target: Value) {
        let source = object.0 + self.heap_base;
        let fwd = source as *mut FwdPtr;
        unsafe {
            (*fwd).tag = TAG_FWD_PTR;
            (*fwd).fwd = target;
        }
    }

    fn copy(&mut self, _object: StableMemoryAddress) -> Value {
        todo!()
    }

    fn scan<F: Fn(&mut Self)>(&mut self, _visit_field: F) {
        todo!()
    }

    fn is_pointer(&self, field_value: u32) -> bool {
        is_skewed(field_value)
    }

    fn decode_pointer(&self, field_value: u32) -> StableMemoryAddress {
        StableMemoryAddress(unskew(field_value as usize))
    }

    fn encode_pointer(&self, target: Value) -> u32 {
        target.get_raw()
    }
}

struct StabilizationMetadata {
    type_table: Value,
    data_start: u64,
    data_size: u64,
}

fn load_metadata() -> StabilizationMetadata {
    todo!()
}

fn store_metadata(_metadata: StabilizationMetadata) {
    todo!()
}

fn is_upgrade_compatible(_old_type_table: Value, _new_type_table: Value) -> bool {
    todo!()
}

/// Pre-upgrade operation for graph-copy-based program upgrades:
/// All objects inside main memory that are transitively reachable from stable variables are
/// serialized into stable memory by using a graph copy algorithm.
/// `stable_actor`: Root object for stabilization containing all stable variables of the actor.
/// `old_type_table`: Type table of the current program version that is to be upgraded.
/// Implementation:
/// * Algorithm: Cheney's algorithm using main memory as from-space and stable memory as to-space.
/// * Encoding: The from-space uses the main memory heap layout, while the to-space is encoded in
///   the stable object graph layout (see `GraphCopyStabilization.md`).
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn stabilize(stable_actor: Value, old_type_table: Value) {
    use crate::stable_mem::{self, PAGE_SIZE};

    let stable_start = stable_mem::size() * PAGE_SIZE;
    let stable_size = Serialization::run(stable_actor, stable_start);
    let metadata = StabilizationMetadata {
        type_table: old_type_table,
        data_start: stable_start,
        data_size: stable_size,
    };
    store_metadata(metadata);
}

/// Post-upgrade operation for graph-copy-based program upgrades:
/// Deserialize the object graph stored in stable memory back into main memory by using a graph
/// copy algorithm. Checks whether the new program version is compatible to the stored state by
/// comparing the type tables of both the old and the new program version.
/// `new_type_table`: Type table of the new program version to which that data is to be upgraded.
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
#[no_mangle]
#[cfg(feature = "ic")]
pub unsafe fn destabilize(new_type_table: Value) -> Value {
    use crate::{
        memory::ic::{get_aligned_heap_base, resize_heap},
        rts_trap_with,
    };

    let metadata = load_metadata();
    let old_type_table = metadata.type_table;
    if !is_upgrade_compatible(old_type_table, new_type_table) {
        rts_trap_with("Incompatible program versions: Upgrade not possible")
    }
    let heap_base = get_aligned_heap_base();
    let heap_size = Deserialization::run(metadata.data_start, metadata.data_size, heap_base);
    assert!(heap_size <= usize::MAX as u64);
    unsafe {
        resize_heap(heap_size as usize);
    }
    Value::from_ptr(heap_base)
}
