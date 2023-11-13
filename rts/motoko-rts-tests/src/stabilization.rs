mod reader_writer;
mod stable_memory;

use crate::gc::{
    check_dynamic_heap,
    heap::MotokoHeap,
    random::generate,
    utils::{GC, GC_IMPLS},
    TestHeap,
};
use motoko_rts::{
    constants::WORD_SIZE,
    region::region0_get,
    stabilization::{Deserialization, Serialization},
    types::Value,
};
use motoko_rts_macros::{incremental_gc, non_incremental_gc};

pub unsafe fn test() {
    println!("Testing stabilization ...");
    reader_writer::test();
    test_serialization_deserialization()
}

fn clear_array(value: Value) {
    unsafe {
        let array = value.as_array();
        for index in 0..(*array).len {
            array.set_scalar(index, Value::from_scalar(0));
        }
    }
}

#[non_incremental_gc]
fn resize_heap(heap: &mut MotokoHeap, new_size: usize) {
    assert_eq!(new_size % WORD_SIZE as usize, 0);
    let new_hp = heap.heap_base_address() + new_size;
    heap.set_heap_ptr_address(new_hp);
}

#[incremental_gc]
fn resize_heap(heap: &mut MotokoHeap, new_size: usize) {
    todo!();
}

struct RandomHeap {
    descriptor: TestHeap,
    heap: MotokoHeap,
}

impl RandomHeap {
    fn reset(&mut self, new_heap_size: usize, stable_root: Value) {
        let continuation_table = Value::from_ptr(self.heap.continuation_table_ptr_address());
        clear_array(continuation_table);
        let static_roots = Value::from_ptr(self.heap.static_root_array_address());
        clear_array(static_roots);
        unsafe {
            resize_heap(&mut self.heap, new_heap_size);
            let static_root_array = static_roots.as_array();
            assert!((*static_root_array).len > 0);
            static_root_array.set_pointer(0, stable_root, &mut self.heap);
        }
        // Set the stable root as sole static root pointer.
        self.descriptor.roots.clear();
        let new_root_id = self
            .descriptor
            .heap
            .iter()
            .map(|(key, _)| *key)
            .max()
            .unwrap();
        self.descriptor
            .heap
            .push((new_root_id, self.descriptor.roots.clone()));
        self.descriptor.roots.clear();
        self.descriptor.roots.push(new_root_id);
        self.descriptor.continuation_table.clear();
        unsafe {
            assert!(!region0_get(&mut self.heap).is_ptr());
        }
    }

    fn heap_base_address(&self) -> usize {
        self.heap.heap_base_address()
    }

    fn old_stable_root(&self) -> Value {
        Value::from_ptr(self.heap.static_root_array_address())
    }

    fn check_heap(&self) {
        check_dynamic_heap(
            true,
            &self.descriptor.heap,
            &self.descriptor.roots,
            &self.descriptor.continuation_table,
            &self.heap.heap().as_ref(),
            self.heap.heap_base_offset(),
            self.heap.heap_ptr_offset(),
            self.heap.continuation_table_ptr_offset(),
            self.heap.region0_ptr_offset(),
        )
    }
}

fn random_heap(gc: GC) -> RandomHeap {
    const RANDOM_SEED: u64 = 4711;
    const MAX_OBJECTS: u32 = 1000;
    let descriptor = generate(RANDOM_SEED, MAX_OBJECTS);
    let heap = descriptor.build(gc);
    RandomHeap { descriptor, heap }
}

fn test_serialization_deserialization() {
    println!("  Testing serialization and deserialization ...");
    let gc = GC_IMPLS[0];
    let mut heap = random_heap(gc);
    let stable_start = 0;
    let stable_size = Serialization::run(heap.old_stable_root(), stable_start);
    let result = Deserialization::run(stable_start, stable_size, heap.heap_base_address());
    heap.reset(result.new_heap_size as usize, result.stable_root);
    heap.check_heap();
}
