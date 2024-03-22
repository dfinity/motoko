mod layout;
mod reader_writer;
mod stable_bigints;
mod stable_memory;

use crate::{
    gc::{
        check_dynamic_heap, heap::MotokoHeap, random::generate, utils::WORD_SIZE, CheckMode,
        TestHeap,
    },
    memory::TestMemory,
    stabilization::stable_memory::clear_stable_memory,
};
use motoko_rts::{
    memory::{alloc_array, Memory},
    stabilization::{
        deserialization::Deserialization, graph_copy::GraphCopy, serialization::Serialization,
    },
    types::{Array, Value, Words, TAG_ARRAY, TAG_FWD_PTR},
};
use oorandom::Rand32;

pub unsafe fn test() {
    println!("Testing stabilization ...");
    layout::test();
    stable_bigints::test();
    reader_writer::test();
    test_stabilization();
    reset_memory();
}

#[no_mangle]
pub fn moc_stabilization_instruction_limit() -> u64 {
    u64::MAX
}

#[no_mangle]
pub fn ic0_performance_counter(_counter: u32) -> u64 {
    0
}

fn reset_gc<M: Memory>(memory: &mut M, heap_base_address: usize) {
    use motoko_rts::gc::incremental::{set_incremental_gc_state, IncrementalGC};

    unsafe {
        let state = IncrementalGC::initial_gc_state(memory, heap_base_address);
        set_incremental_gc_state(Some(state));
    }
}

fn clear_heap(heap: &mut MotokoHeap) {
    reset_gc(heap, heap.heap_base_address());
}

fn reset_memory() {
    clear_stable_memory();
    reset_main_memory();
}

fn reset_main_memory() {
    use motoko_rts::gc::incremental::partitioned_heap::PARTITION_SIZE;

    let mut memory = TestMemory::new(Words(PARTITION_SIZE));
    reset_gc(&mut memory, 0);
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
        let table_pointer = self.memory.continuation_table_variable_address() as *mut Value;
        unsafe {
            *table_pointer = alloc_array(&mut self.memory, 0);
        }
    }

    fn reset_static_root(&mut self, stable_root: Value) {
        let root_array_pointer = self.memory.static_root_array_variable_address() as *mut Value;
        unsafe {
            let old_roots = (*root_array_pointer).get_ptr() as *mut Array;
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

    fn old_stable_root(&self) -> Value {
        let root_array_pointer = self.memory.static_root_array_variable_address() as *mut Value;
        unsafe { *root_array_pointer }
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
            self.memory.static_root_array_variable_offset(),
            self.memory.continuation_table_variable_offset(),
            self.memory.region0_pointer_variable_offset(),
        )
    }
}

fn random_heap(random: &mut Rand32, max_objects: usize) -> RandomHeap {
    let descriptor = generate(random.rand_u32() as u64, max_objects);
    let pointers: usize = descriptor
        .heap
        .iter()
        .map(|(_, references)| references.len() + 1)
        .sum();
    let memory = descriptor.build(pointers * WORD_SIZE as usize);
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

fn test_serialization_deserialization(random: &mut Rand32, max_objects: usize, stable_start: u64) {
    println!("    Testing with {max_objects} objects");
    clear_stable_memory();
    let mut heap = random_heap(random, max_objects);
    let old_stable_root = heap.old_stable_root();

    let stable_size = serialize(old_stable_root, stable_start);

    heap.clear();

    let stable_root = deserialize(&mut heap.memory, stable_start, stable_size);

    heap.set_new_root(stable_root);
    heap.check_heap();
}

fn serialize(old_stable_root: Value, stable_start: u64) -> u64 {
    let mut memory = TestMemory::new(Words(0));
    let mut serialization = Serialization::start(&mut memory, old_stable_root, stable_start);
    serialization.copy_increment(&mut memory);
    assert!(serialization.is_completed());
    serialization.serialized_data_length()
}

fn deserialize<M: Memory>(mem: &mut M, stable_start: u64, stable_size: u64) -> Value {
    let mut deserialization = Deserialization::start(mem, stable_start, stable_size);
    deserialization.copy_increment(mem);
    assert!(deserialization.is_completed());
    deserialization.get_stable_root()
}
