mod buffer_hash_map;
mod buffered_stable_memory;
mod reader_writer;
mod stable_memory;

use crate::{
    gc::{
        check_dynamic_heap,
        heap::MotokoHeap,
        random::generate,
        utils::{GC, GC_IMPLS, WORD_SIZE},
        CheckMode, TestHeap,
    },
    stabilization::stable_memory::clear_stable_memory,
};
use motoko_rts::{
    memory::alloc_array,
    stabilization::{Deserialization, Serialization},
    types::{Array, Null, Obj, Value, TAG_ARRAY, TAG_FWD_PTR, TAG_NULL},
};
use motoko_rts_macros::{incremental_gc, non_incremental_gc};
use oorandom::Rand32;

pub unsafe fn test() {
    println!("Testing stabilization ...");
    buffer_hash_map::test();
    buffered_stable_memory::test();
    reader_writer::test();
    initialize_null_singleton();
    test_stabilization();
    reset_memory();
}

static mut DUMMY_NULL_SINGLETON: Null = Null {
    header: Obj {
        tag: TAG_NULL,
        #[cfg(feature = "incremental_gc")]
        forward: Value::from_scalar(0), // Temporary value, will be replaced.
    },
};

unsafe fn initialize_null_singleton() {
    let null_singleton = Value::from_ptr(&mut DUMMY_NULL_SINGLETON as *mut Null as usize);
    DUMMY_NULL_SINGLETON.header.tag = TAG_NULL;
    DUMMY_NULL_SINGLETON.header.init_forward(null_singleton);
}

#[no_mangle]
pub fn moc_null_singleton() -> Value {
    unsafe { Value::from_ptr(&mut DUMMY_NULL_SINGLETON as *mut Null as usize) }
}

#[non_incremental_gc]
fn clear_heap(heap: &mut MotokoHeap) {
    heap.set_heap_ptr_address(heap.heap_base_address());
}

#[incremental_gc]
fn clear_heap(heap: &mut MotokoHeap) {
    use motoko_rts::gc::incremental::IncrementalGC;

    unsafe {
        IncrementalGC::initialize(heap, heap.heap_base_address());
    }
}

fn reset_memory() {
    clear_stable_memory();
    reset_main_memory();
}

#[non_incremental_gc]
fn reset_main_memory() {}

#[incremental_gc]
fn reset_main_memory() {
    use crate::memory::TestMemory;
    use motoko_rts::gc::incremental::{partitioned_heap::PARTITION_SIZE, IncrementalGC};
    use motoko_rts::types::Words;

    let mut memory = TestMemory::new(Words(PARTITION_SIZE as u32));
    unsafe {
        IncrementalGC::initialize(&mut memory, 0);
    }
}

struct RandomHeap {
    descriptor: TestHeap,
    memory: MotokoHeap,
}

impl RandomHeap {
    fn clear(&mut self) {
        clear_heap(&mut self.memory);
    }

    fn set_new_root(&mut self, stable_root: Value) {
        self.clear_continuation_table();
        self.reset_static_root(stable_root);
        // Set the stable root as sole static root pointer.
        self.reset_descriptor();
    }

    fn clear_continuation_table(&mut self) {
        let table_pointer = self.memory.continuation_table_ptr_address() as *mut Value;
        unsafe {
            *table_pointer = alloc_array(&mut self.memory, 0);
        }
    }

    fn reset_static_root(&mut self, stable_root: Value) {
        let old_roots = self.memory.static_root_array_address() as *mut Array;
        unsafe {
            // Serialization has replaced the static root array by a forwarding pointer object.
            assert_eq!((*old_roots).header.tag, TAG_FWD_PTR);
            (*old_roots).header.tag = TAG_ARRAY;
            (*old_roots)
                .header
                .init_forward(Value::from_ptr(old_roots as usize));
            let new_roots = stable_root.as_array();
            let length = (*new_roots).len;
            assert_eq!(length as usize, self.descriptor.roots.len());
            (*old_roots).len = length;
            for index in 0..length {
                old_roots.initialize(index, new_roots.get(index), &mut self.memory);
            }
            if length > 0 {
                // GC `check_heap()` expects the object id as scalar as the first array element.
                // Therefore, generate a new object id and assign this to the garbage stable root object.
                let new_object_id = self
                    .descriptor
                    .heap
                    .iter()
                    .map(|(key, _)| key)
                    .max()
                    .unwrap()
                    + 1;
                new_roots.set_scalar(0, Value::from_scalar(new_object_id));
                let mut outgoing = vec![];
                for index in 1..length {
                    // GC `check_heap` expects skewed addresses pointing beyond heap base for all array elements
                    // except the first one.
                    new_roots.initialize(index, stable_root, &mut self.memory);
                    outgoing.push(new_object_id);
                }
                self.descriptor.heap.push((new_object_id, outgoing));
            }
        }
    }

    fn reset_descriptor(&mut self) {
        self.descriptor.continuation_table.clear();
    }

    fn heap_base_address(&self) -> usize {
        self.memory.heap_base_address()
    }

    fn old_stable_root(&self) -> Value {
        Value::from_ptr(self.memory.static_root_array_address())
    }

    fn check_heap(&self) {
        check_dynamic_heap(
            CheckMode::Stabilzation,
            &self.descriptor.heap,
            &self.descriptor.roots,
            &self.descriptor.continuation_table,
            &self.memory.heap().as_ref(),
            self.memory.heap_base_offset(),
            self.memory.heap_ptr_offset(),
            self.memory.continuation_table_ptr_offset(),
            self.memory.region0_ptr_offset(),
        )
    }
}

fn random_heap(random: &mut Rand32, max_objects: u32, gc: GC) -> RandomHeap {
    let descriptor = generate(random.rand_u32() as u64, max_objects);
    let pointers: usize = descriptor
        .heap
        .iter()
        .map(|(_, references)| references.len() + 1)
        .sum();
    let memory = descriptor.build(gc, pointers * WORD_SIZE as usize);
    RandomHeap { descriptor, memory }
}

fn test_stabilization() {
    println!("  Testing serialization and deserialization ...");
    const RANDOM_SEED: u64 = 4711;
    let mut random = Rand32::new(RANDOM_SEED);
    test_serialization_deserialization(&mut random, 100, 0);
    test_serialization_deserialization(&mut random, 1000, 200);
    test_serialization_deserialization(&mut random, 10_000, 5_000);
    test_serialization_deserialization(&mut random, 20_000, 7_000);
}

fn test_serialization_deserialization(random: &mut Rand32, max_objects: u32, stable_start: u64) {
    println!("    Testing with {max_objects} objects");
    clear_stable_memory();
    let gc = GC_IMPLS[0];
    let mut heap = random_heap(random, max_objects, gc);
    let heap_base = heap.heap_base_address();
    let stable_size = Serialization::run(heap.old_stable_root(), stable_start);
    heap.clear();
    let stable_root = Deserialization::run(&mut heap.memory, stable_start, stable_size, heap_base);
    heap.set_new_root(stable_root);
    heap.check_heap();
}
