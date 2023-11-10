mod reader_writer;
mod stable_memory;

use crate::gc::{random::generate, heap::MotokoHeap, utils::{GC_IMPLS, GC}};
use motoko_rts::{stabilization::Serialization, types::Value};

pub unsafe fn test() {
    println!("Testing stabilization ...");
    reader_writer::test();

    test_serialization()
}

fn random_heap(gc: GC) -> MotokoHeap {
    const RANDOM_SEED: u64 = 4711;
    const MAX_OBJECTS: u32 = 1000;
    let random_heap = generate(RANDOM_SEED, MAX_OBJECTS);
    random_heap.build(gc)
}

fn test_serialization() {
    println!("  Testing serialization ...");

    let gc = GC_IMPLS[0];
    let heap = random_heap(gc);
    let stable_root = Value::from_ptr(heap.static_root_array_address());
    let size = Serialization::run(stable_root, 0);
}
