mod reader_writer;
mod stable_memory;

use crate::gc::{
    check_dynamic_heap,
    heap::MotokoHeap,
    random::generate,
    utils::{GC, GC_IMPLS},
    CheckMode, TestHeap,
};
use motoko_rts::{
    stabilization::{Deserialization, Serialization},
    types::{Array, Value, TAG_ARRAY, TAG_FWD_PTR},
};
use motoko_rts_macros::{incremental_gc, non_incremental_gc};

pub unsafe fn test() {
    println!("Testing stabilization ...");
    reader_writer::test();
    test_serialization_deserialization()
}

#[non_incremental_gc]
fn clear_heap(heap: &mut MotokoHeap) {
    let new_hp = heap.heap_base_address();
    heap.set_heap_ptr_address(new_hp);
}

#[incremental_gc]
fn clear_heap(heap: &mut MotokoHeap) {
    todo!();
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

    fn clear_continuation_table(&self) {
        let table_pointer = self.memory.continuation_table_ptr_address() as *mut Value;
        unsafe {
            let table = *table_pointer;
            let array = table.as_array();
            for index in 0..(*array).len {
                array.set_scalar(index, Value::from_scalar(0));
            }
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

fn random_heap(gc: GC) -> RandomHeap {
    const RANDOM_SEED: u64 = 4711;
    const MAX_OBJECTS: u32 = 100;
    let descriptor = generate(RANDOM_SEED, MAX_OBJECTS);
    let memory = descriptor.build(gc);
    RandomHeap { descriptor, memory }
}

fn test_serialization_deserialization() {
    println!("  Testing serialization and deserialization ...");
    let gc = GC_IMPLS[0];
    let mut heap = random_heap(gc);
    let stable_start = 0;
    let heap_base = heap.heap_base_address();
    let stable_size = Serialization::run(heap.old_stable_root(), stable_start);
    heap.clear();
    let stable_root = Deserialization::run(&mut heap.memory, stable_start, stable_size, heap_base);
    heap.set_new_root(stable_root);
    heap.check_heap();
}
