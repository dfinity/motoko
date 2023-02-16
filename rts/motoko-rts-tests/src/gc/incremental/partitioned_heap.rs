use std::{
    collections::HashSet,
    io::{stdout, Write},
    mem::size_of,
};

use motoko_rts::{
    gc::incremental::{
        partitioned_heap::{
            HeapIteratorState, Partition, PartitionIterator, PartitionedHeap,
            PartitionedHeapIterator, PARTITION_SIZE, SURVIVAL_RATE_THRESHOLD,
        },
        time::BoundedTime,
    },
    memory::{alloc_array, alloc_blob, Memory},
    types::{unmark, Array, Blob, Bytes, Obj, Tag, Value, Words, TAG_ARRAY, TAG_BLOB},
};

use crate::{gc::utils::WORD_SIZE, memory::TestMemory};

const NUMBER_OF_OBJECTS: usize = 2 * PARTITION_SIZE / 16;
const HEAP_SIZE: usize = 4 * PARTITION_SIZE;

pub unsafe fn test() {
    println!("  Testing partitioned heap...");
    test_normal_size_scenario();
    test_large_size_scenario();
}

unsafe fn test_normal_size_scenario() {
    let mut heap = create_test_heap();
    let occupied_partitions = 2 + heap.heap_pointer() / PARTITION_SIZE;
    test_allocation_partitions(&heap.inner, occupied_partitions);
    test_iteration(&heap.inner, 1024);
    test_evacuation_plan(&mut heap, occupied_partitions);
    test_freeing_partitions(&mut heap.inner, occupied_partitions);
    test_reallocations(&mut heap);
    test_evacuation_plan(&mut heap, HEAP_SIZE / PARTITION_SIZE);
    test_survival_rate(&mut heap.inner);
    test_freeing_partitions(&mut heap.inner, HEAP_SIZE / PARTITION_SIZE);
    test_close_partition(&mut heap);
}

fn test_allocation_partitions(heap: &PartitionedHeap, number_of_partitions: usize) {
    println!("    Test allocation partitions...");
    assert!(number_of_partitions >= 1);
    let mut exists = false;
    for index in 0..number_of_partitions {
        if heap.is_allocation_partition(index) {
            assert!(!exists);
            exists = true;
        }
    }
    assert!(exists);
}

unsafe fn test_iteration(heap: &PartitionedHeap, break_step_size: usize) {
    println!("    Test heap iteration...");
    let mut iterator_state = HeapIteratorState::new();
    let mut set = HashSet::new();
    while set.len() < NUMBER_OF_OBJECTS {
        let mut time = BoundedTime::new(break_step_size);
        let count_before = set.len();
        iterate_heap(heap, &mut iterator_state, &mut set, &mut time);
        assert!(set.len() > count_before);
        assert!(time.is_over() || set.len() == NUMBER_OF_OBJECTS);
    }
    assert_eq!(set.len(), NUMBER_OF_OBJECTS);
    reset_progress();
}

unsafe fn iterate_heap(
    heap: &PartitionedHeap,
    state: &mut HeapIteratorState,
    set: &mut HashSet<usize>,
    time: &mut BoundedTime,
) {
    let mut iterator = PartitionedHeapIterator::load_from(heap, state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(heap);
        assert!(!partition.is_free());
        assert!(!partition.to_be_evacuated());
        assert!(!partition.has_large_content());
        assert!(!partition.is_temporary());
        iterate_partition(partition, state, set, time);
        if time.is_over() {
            break;
        }
        iterator.next_partition(heap);
    }
    iterator.save_to(state);
}

unsafe fn iterate_partition(
    partition: &Partition,
    state: &mut HeapIteratorState,
    set: &mut HashSet<usize>,
    time: &mut BoundedTime,
) {
    let mut iterator = PartitionIterator::load_from(partition, state);
    while iterator.has_object() {
        let object = iterator.current_object();
        //println!("FOUND {:#x} {}", object as usize, set.len());
        let array = Value::from_ptr(object as usize).as_array();
        let content = array.get(0).get_scalar();
        let inserted = set.insert(content as usize);
        assert!(inserted);
        progress(set.len(), NUMBER_OF_OBJECTS);
        assert_eq!(partition.get_index(), object as usize / PARTITION_SIZE);
        time.tick();
        iterator.next_object();
        if time.is_over() {
            break;
        }
    }
    iterator.save_to(state);
}

unsafe fn test_evacuation_plan(heap: &mut PartitionedTestHeap, occupied_partitions: usize) {
    println!("    Test evacuation plan...");
    unmark_all_objects(heap);
    heap.inner.plan_evacuations();
    let iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::load_from(&heap.inner, &iterator_state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(&heap.inner);
        assert!(partition.get_index() < occupied_partitions);
        assert!(!partition.is_free());
        assert!(
            partition.to_be_evacuated()
                || heap.inner.is_allocation_partition(partition.get_index())
        );
        iterator.next_partition(&heap.inner);
    }
}

unsafe fn test_freeing_partitions(heap: &mut PartitionedHeap, occupied_partitions: usize) {
    println!("    Test freeing partitions...");
    heap.complete_collection();
    let iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::load_from(heap, &iterator_state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(heap);
        assert!(partition.get_index() < occupied_partitions);
        assert!(!partition.is_free());
        assert!(!partition.to_be_evacuated());
        assert!(heap.is_allocation_partition(partition.get_index()));
        iterator.next_partition(heap);
    }
    assert!(heap.occupied_size().as_usize() < PARTITION_SIZE + heap.base_address());
}

const OBJECT_SIZE: usize = size_of::<Array>() + WORD_SIZE;

unsafe fn test_reallocations(heap: &mut PartitionedTestHeap) {
    println!("    Test reallocations...");
    heap.inner.start_collection(&mut heap.memory);
    let remaining_objects = count_objects(&heap.inner);
    allocate_objects(heap);
    assert!(
        heap.inner.occupied_size().as_usize() >= NUMBER_OF_OBJECTS * OBJECT_SIZE + heap.heap_base()
    );
    let final_objects = count_objects(&heap.inner);
    assert_eq!(final_objects, remaining_objects + NUMBER_OF_OBJECTS);
}

unsafe fn count_objects(heap: &PartitionedHeap) -> usize {
    let mut count = 0;
    let state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::load_from(heap, &state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(heap);
        assert!(!partition.is_free());
        assert!(!partition.to_be_evacuated());
        count += count_objects_in_partition(partition);
        iterator.next_partition(heap);
    }
    reset_progress();
    count
}

unsafe fn count_objects_in_partition(partition: &Partition) -> usize {
    let mut count = 0;
    let mut time = BoundedTime::new(0);
    let mut state = HeapIteratorState::new();
    let mut iterator = PartitionIterator::load_from(partition, &mut state);
    while iterator.has_object() {
        let object = iterator.current_object();
        assert_eq!(partition.get_index(), object as usize / PARTITION_SIZE);
        let array = Value::from_ptr(object as usize).as_array();
        let content = array.get(0).get_scalar() as usize;
        assert!(content < NUMBER_OF_OBJECTS);
        time.tick();
        count += 1;
        if count <= NUMBER_OF_OBJECTS {
            progress(count, NUMBER_OF_OBJECTS);
        }
        iterator.next_object();
    }
    count
}

fn test_close_partition(heap: &mut PartitionedTestHeap) {
    println!("    Test close partition...");
    test_close_partition_multi_word(heap);
    test_close_partition_single_word(heap);
}

fn test_close_partition_multi_word(heap: &mut PartitionedTestHeap) {
    let old_heap_pointer = heap.heap_pointer();
    let old_partition = old_heap_pointer / PARTITION_SIZE;
    let remainder = PARTITION_SIZE - old_heap_pointer % PARTITION_SIZE;
    let blob = heap.allocate_blob(remainder);
    assert_ne!(heap.heap_pointer() / PARTITION_SIZE, old_partition);
    assert_ne!(blob.get_ptr(), old_heap_pointer);
}

fn test_close_partition_single_word(heap: &mut PartitionedTestHeap) {
    let old_heap_pointer = heap.heap_pointer();
    let old_partition = old_heap_pointer / PARTITION_SIZE;
    let remainder = PARTITION_SIZE - old_heap_pointer % PARTITION_SIZE;
    assert!(remainder > size_of::<Blob>());
    let old_partition_blob = heap.allocate_blob(remainder - size_of::<Blob>() - WORD_SIZE);
    assert_eq!(old_partition_blob.get_ptr(), old_heap_pointer);
    let filler_address = heap.heap_pointer();
    let new_partition_blob = heap.allocate_blob(0);
    assert_ne!(heap.heap_pointer() / PARTITION_SIZE, old_partition);
    assert_ne!(new_partition_blob.get_ptr(), filler_address);
}

unsafe fn test_survival_rate(heap: &mut PartitionedHeap) {
    println!("    Test survival rate...");
    let iterator_state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::load_from(heap, &iterator_state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(heap);
        let dynamic_partition_size =
            PARTITION_SIZE - partition.dynamic_space_start() % PARTITION_SIZE;
        let expected_survival_rate =
            occupied_space(partition) as f64 / dynamic_partition_size as f64;
        assert!(f64::abs(partition.survival_rate() - expected_survival_rate) < 1e6);
        let expected_evacuation = !heap.is_allocation_partition(partition.get_index())
            && partition.survival_rate() <= SURVIVAL_RATE_THRESHOLD;
        assert_eq!(partition.to_be_evacuated(), expected_evacuation);
        iterator.next_partition(heap);
    }
}

unsafe fn test_large_size_scenario() {
    println!("    Test large allocations...");
    const LARGE: usize = PARTITION_SIZE + WORD_SIZE;
    const EXTRA_LARGE: usize = 2 * PARTITION_SIZE;
    test_allocation_sizes(&[32, PARTITION_SIZE, 16], 3);
    test_allocation_sizes(&[28, LARGE, 20], 3);
    test_allocation_sizes(&[24, LARGE, LARGE, 36], 5);
    test_allocation_sizes(&[24, EXTRA_LARGE, 16], 3);
    test_allocation_sizes(&[24, EXTRA_LARGE, LARGE, 16], 6);
    test_allocation_sizes(&[24, EXTRA_LARGE, 32, LARGE, 16], 6);
}

unsafe fn test_allocation_sizes(sizes: &[usize], number_of_partitions: usize) {
    let total_partitions = number_of_partitions + 1; // Plus temporary partition.
    let mut heap = PartitionedTestHeap::new(total_partitions * PARTITION_SIZE);
    assert!(heap.inner.occupied_size().as_usize() < PARTITION_SIZE + heap.heap_base());
    heap.inner.start_collection(&mut heap.memory);
    for size in sizes.iter() {
        assert_eq!(*size % WORD_SIZE, 0);
        assert!(*size >= size_of::<Blob>());
        let blob = heap.allocate_blob(*size - size_of::<Blob>());
        let object = blob.get_ptr() as *mut Obj;
        let unmarked_before = heap.inner.mark_object(object);
        assert!(unmarked_before);
    }
    assert!(
        heap.inner.occupied_size().as_usize() >= sizes.iter().sum::<usize>() + heap.heap_base()
    );
    iterate_large_objects(&heap.inner, sizes);
    unmark_all_objects(&mut heap);
    heap.inner.plan_evacuations();
    heap.inner.collect_large_objects();
    heap.inner.complete_collection();
    heap.inner.start_collection(&mut heap.memory);
    iterate_large_objects(&heap.inner, &[]);
    assert!(heap.inner.occupied_size().as_usize() < PARTITION_SIZE + heap.heap_base())
}

unsafe fn unmark_all_objects(heap: &mut PartitionedTestHeap) {
    heap.inner.complete_collection();
    heap.inner.start_collection(&mut heap.memory);
}

unsafe fn iterate_large_objects(heap: &PartitionedHeap, expected_sizes: &[usize]) {
    let mut detected_sizes = vec![];
    let state = HeapIteratorState::new();
    let mut iterator = PartitionedHeapIterator::load_from(heap, &state);
    while iterator.has_partition() {
        let partition = iterator.current_partition(heap);
        iterate_large_partition(partition, &mut detected_sizes);
        iterator.next_partition(heap);
    }
    detected_sizes.sort();
    let mut expected_sorted = expected_sizes.to_vec();
    expected_sorted.sort();
    assert_eq!(detected_sizes, expected_sorted);
}

unsafe fn iterate_large_partition(partition: &Partition, detected_sizes: &mut Vec<usize>) {
    let mut time = BoundedTime::new(0);
    let mut state = HeapIteratorState::new();
    let mut iterator = PartitionIterator::load_from(partition, &mut state);
    while iterator.has_object() {
        let object = iterator.current_object();
        assert_eq!(object.tag(), TAG_BLOB);
        let size = block_size(object as *const Tag);
        detected_sizes.push(size);
        time.tick();
        iterator.next_object();
    }
}

unsafe fn occupied_space(partition: &Partition) -> usize {
    let mut sweep_line = partition.dynamic_space_start();
    let mut occupied_space = 0;
    while sweep_line < partition.dynamic_space_end() {
        let block = sweep_line as *const Tag;
        let size = block_size(block);
        occupied_space += size;
        sweep_line += size;
        assert!(sweep_line <= partition.dynamic_space_end());
    }
    occupied_space
}

fn create_test_heap() -> PartitionedTestHeap {
    println!("    Create test heap...");
    let mut heap = PartitionedTestHeap::new(HEAP_SIZE);
    unsafe {
        heap.inner.start_collection(&mut heap.memory);
    }
    allocate_objects(&mut heap);
    let heap_size = heap.inner.occupied_size().as_usize();
    assert_eq!(
        heap_size,
        heap.heap_base() + NUMBER_OF_OBJECTS * OBJECT_SIZE
    );
    heap
}

fn allocate_objects(heap: &mut PartitionedTestHeap) {
    for index in 0..NUMBER_OF_OBJECTS {
        progress(index + 1, NUMBER_OF_OBJECTS);
        let value = Value::from_scalar(index as u32);
        let array = heap.allocate_array(&[value]);
        unsafe {
            let object = array.get_ptr() as *mut Obj;
            let unmarked_before = heap.inner.mark_object(object);
            assert!(unmarked_before);
            assert!(!heap.inner.mark_object(object));
        }
    }
    reset_progress();
}

fn progress(count: usize, max: usize) {
    if count % (max / 100) == 0 || count == max {
        let percentage = count * 100 / max;
        print!("{percentage}/100\r");
        Write::flush(&mut stdout()).unwrap();
    }
}

fn reset_progress() {
    print!("       \r");
    Write::flush(&mut stdout()).unwrap();
}

pub struct PartitionedTestHeap {
    memory: TestMemory,
    inner: PartitionedHeap,
}

impl PartitionedTestHeap {
    pub fn new(size: usize) -> PartitionedTestHeap {
        let mut memory = TestMemory::new(Bytes(size as u32).to_words());
        let heap_base = memory.heap_base();
        let inner = unsafe { PartitionedHeap::new(&mut memory, heap_base) };
        PartitionedTestHeap { memory, inner }
    }

    pub fn heap_base(&self) -> usize {
        self.memory.heap_base()
    }

    pub fn heap_pointer(&self) -> usize {
        self.memory.heap_pointer()
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

    pub fn allocate_blob(&mut self, size: usize) -> Value {
        unsafe { alloc_blob(self, Bytes(size as u32)) }
    }
}

unsafe fn block_size(block: *const Tag) -> usize {
    let tag = unmark(*block);
    match tag {
        TAG_ARRAY => {
            size_of::<Array>() + (block as *const Array).len() as usize * WORD_SIZE as usize
        }
        TAG_BLOB => size_of::<Blob>() + (block as *const Blob).len().as_usize(),
        _ => unimplemented!(),
    }
}

impl Memory for PartitionedTestHeap {
    unsafe fn alloc_words(&mut self, size: Words<u32>) -> Value {
        let result = self.inner.allocate(&mut self.memory, size);
        self.memory
            .set_heap_pointer(result.get_ptr() + size.to_bytes().as_usize());
        result
    }

    unsafe fn linear_alloc_words(&mut self, _n: Words<u32>) -> Value {
        unreachable!()
    }

    unsafe fn grow_memory(&mut self, _ptr: u64) {
        unreachable!();
    }
}
