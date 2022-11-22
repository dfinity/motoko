use std::collections::HashSet;

use motoko_rts::{
    gc::incremental::free_list::{FreeBlock, SegregatedFreeList},
    types::Bytes,
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing free list...");

    let allocation_sizes = vec![128];
    let memory_size = allocation_sizes.iter().sum();
    test_allocate_free(&allocation_sizes, 12, memory_size);

    let allocation_sizes = vec![12, 4096, 12, 16, 124, 128, 132, 16];
    let memory_size = allocation_sizes.iter().sum::<u32>() * 2; // due to fragmentation
    test_allocate_free(&allocation_sizes, 8, memory_size);

    const TOTAL_SIZE: u32 = 32 * 1024;
    const SMALL_SIZE: u32 = 48;
    test_split_blocks(TOTAL_SIZE, SMALL_SIZE, TOTAL_SIZE / SMALL_SIZE);
}

unsafe fn test_allocate_free(allocation_sizes: &Vec<u32>, rounds: usize, total_size: u32) {
    println!("    Testing allocate/free {} ...", allocation_sizes.len());
    let mut mem = TestMemory::new(Bytes(total_size).to_words());
    let mut list = SegregatedFreeList::new();
    list.sanity_check();
    for _ in 0..rounds {
        let mut allocations: HashSet<*mut FreeBlock> = HashSet::new();
        for size in allocation_sizes.iter() {
            let block = list.allocate(&mut mem, Bytes(*size));
            assert_eq!(block.size().as_u32(), *size);
            let new_insertion = allocations.insert(block);
            assert!(new_insertion);
            list.sanity_check();
        }
        for block in allocations.iter() {
            list.free(*block);
            list.sanity_check();
        }
    }
}

unsafe fn test_split_blocks(total_size: u32, small_size: u32, amount: u32) {
    println!("    Testing split blocks {amount} ...");
    let mut mem = TestMemory::new(Bytes(total_size).to_words());
    let mut list = SegregatedFreeList::new();
    list.sanity_check();
    let large = list.allocate(&mut mem, Bytes(total_size));
    assert_eq!(large.size().as_u32(), total_size);
    list.free(large);
    let mut allocations: HashSet<*mut FreeBlock> = HashSet::new();
    for _ in 0..amount {
        let block = list.allocate(&mut mem, Bytes(small_size));
        assert_eq!(block.size().as_u32(), small_size);
        let new_insertion = allocations.insert(block);
        assert!(new_insertion);
        list.sanity_check();
    }
    for block in allocations.iter() {
        list.free(*block);
        list.sanity_check();
    }
}
