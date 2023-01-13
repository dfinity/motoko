use std::{
    io::{stdout, Write},
    mem::size_of,
};

use motoko_rts::{
    gc::incremental::partitioned_heap::{
        HeapIteratorState, PartitionedHeap, PartitionedHeapIterator, PARTITION_SIZE,
    },
    memory::{alloc_array, Memory},
    types::{Array, Value, Words},
};

use crate::gc::utils::WORD_SIZE;

const HEAP_SIZE: usize = 2 * PARTITION_SIZE;
const NUMBER_OF_OBJECTS: usize = HEAP_SIZE / 16;

pub unsafe fn test() {
    println!("  Testing partitioned heap...");

    let mut heap = create_test_heap();
    let occupied_partitions = 1 + heap.heap_pointer as usize / PARTITION_SIZE;
    test_allocation_partitions(&heap.inner, occupied_partitions);
    test_iteration(&heap.inner, 1024, occupied_partitions);
    test_evacuation_plan(&mut heap.inner, occupied_partitions);
    test_freeing_partitions(&mut heap.inner, occupied_partitions);
}

fn test_allocation_partitions(heap: &PartitionedHeap, number_of_partitions: usize) {
    println!("    Test allocation partitions...");
    assert!(number_of_partitions >= 1);
    assert!(heap.is_allocation_partition(number_of_partitions - 1));
    for index in 0..number_of_partitions - 1 {
        assert!(!heap.is_allocation_partition(index));
    }
}

unsafe fn test_iteration(
    heap: &PartitionedHeap,
    break_step_size: usize,
    occupied_partitions: usize,
) {
    println!("    Test heap iteration...");
    let mut iterator_state = HeapIteratorState::new();
    let mut count = 0;
    loop {
        let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
        while iterator.current_object().is_some() {
            assert!(iterator.current_partition().is_some());
            let object = iterator.current_object().unwrap();
            let array = Value::from_ptr(object as usize).as_array();
            let content = array.get(0).get_scalar();
            assert_eq!(content as usize, count);
            count += 1;
            progress(count, NUMBER_OF_OBJECTS);
            let partition = iterator.current_partition().unwrap();
            assert!(!partition.is_free());
            assert!(!partition.to_be_evacuated());
            assert_eq!(partition.get_index(), object as usize / PARTITION_SIZE);
            assert!(partition.get_index() < occupied_partitions);
            iterator.next_object();
            if count % break_step_size == 0 {
                break;
            }
        }
        if iterator.current_object().is_none() {
            assert!(iterator.current_partition().is_none());
            break;
        }
    }
    assert_eq!(count, NUMBER_OF_OBJECTS);
    print!("\r");
}

unsafe fn test_evacuation_plan(heap: &mut PartitionedHeap, occupied_partitions: usize) {
    println!("    Test evacuation plan...");
    heap.plan_evacuations();
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    let mut allocation_partition_present = false;
    while iterator.current_partition().is_some() {
        assert!(iterator.current_object().is_some());
        let partition = iterator.current_partition().unwrap();
        assert!(partition.get_index() < occupied_partitions);
        assert!(!partition.is_free());
        if heap.is_allocation_partition(partition.get_index()) {
            allocation_partition_present = true;
            assert!(!partition.to_be_evacuated());
        } else {
            assert!(partition.to_be_evacuated());
        }
        iterator.next_partition();
    }
    assert!(allocation_partition_present);
}

unsafe fn test_freeing_partitions(heap: &mut PartitionedHeap, occupied_partitions: usize) {
    println!("    Test freeing partitions...");
    heap.free_evacuated_partitions();
    let mut iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::resume(heap, &mut iterator_state);
    let partition = iterator.current_partition().unwrap();
    assert!(partition.get_index() < occupied_partitions);
    assert!(!partition.is_free());
    assert!(!partition.to_be_evacuated());
    assert!(heap.is_allocation_partition(partition.get_index()));
    iterator.next_partition();
    assert!(iterator.current_partition().is_none());
    assert!(heap.occupied_size().as_usize() < PARTITION_SIZE);
}

fn create_test_heap() -> PartitionedTestHeap {
    println!("    Create test heap...");
    let mut heap = PartitionedTestHeap::new(HEAP_SIZE);
    allocate_objects(&mut heap);
    let heap_size = heap.inner.occupied_size().as_usize();
    assert_eq!(heap_size, heap.heap_pointer as usize);
    const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;
    assert_eq!(
        heap_size,
        heap.heap_base as usize + NUMBER_OF_OBJECTS * OBJECT_SIZE
    );
    heap
}

fn allocate_objects(heap: &mut PartitionedTestHeap) {
    for index in 0..NUMBER_OF_OBJECTS {
        progress(index + 1, NUMBER_OF_OBJECTS);
        let value = Value::from_scalar(index as u32);
        heap.allocate_array(&[value]);
    }
    print!("\r");
}

fn progress(count: usize, max: usize) {
    if count % (max / 100) == 0 || count == max {
        let percentage = count * 100 / max;
        print!("\r{percentage}/100");
        Write::flush(&mut stdout()).unwrap();
    }
}

pub struct PartitionedTestHeap {
    memory: Box<[u8]>,
    heap_base: u32,
    heap_pointer: u32,
    inner: PartitionedHeap,
}

impl PartitionedTestHeap {
    pub fn new(size: usize) -> PartitionedTestHeap {
        let memory = vec![0u8; size].into_boxed_slice();
        let heap_base = memory.as_ptr() as u32;
        PartitionedTestHeap {
            memory,
            heap_base,
            heap_pointer: heap_base,
            inner: PartitionedHeap::new(heap_base as usize),
        }
    }

    pub fn allocate_array(&mut self, elements: &[Value]) -> Value {
        unsafe {
            let array = alloc_array(self, elements.len() as u32);
            for index in 0..elements.len() {
                let raw_array = array.as_array();
                raw_array.set_scalar(index as u32, elements[index]);
            }
            array
        }
    }
}

impl Memory for PartitionedTestHeap {
    unsafe fn alloc_words(&mut self, size: Words<u32>) -> Value {
        let before = self.heap_pointer;
        self.inner
            .prepare_allocation_partition(&mut self.heap_pointer, size.to_bytes());
        assert_eq!(before, self.heap_pointer);
        let bytes = size.to_bytes().as_u32();
        if bytes > self.heap_base + self.memory.len() as u32 {
            panic!("Test heap is too small");
        }
        let result = Value::from_ptr(self.heap_pointer as usize);
        self.heap_pointer += bytes;
        result
    }
}
