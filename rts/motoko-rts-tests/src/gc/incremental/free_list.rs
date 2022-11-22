use std::collections::HashSet;

use motoko_rts::{
    gc::incremental::free_list::{FreeBlock, SegregatedFreeList},
    types::{Bytes, Words},
};

use crate::memory::TestMemory;

pub unsafe fn test() {
    println!("  Testing free list...");

    let allocation_sizes = vec![12, 4096, 12, 16, 124, 128, 132, 16];
    test_allocate_free(allocation_sizes, 8);

    let allocation_sizes = vec![256];
    test_allocate_free(allocation_sizes, 1);
}

unsafe fn test_allocate_free(allocation_sizes: Vec<u32>, rounds: usize) {
    println!("  Testing allocate/free {}...", allocation_sizes.len());
    let mut mem = TestMemory::new(Words(allocation_sizes.iter().sum()));
    let mut list = SegregatedFreeList::new();
    list.sanity_check();
    for _ in 0..rounds {
        let mut allocations: HashSet<*mut FreeBlock> = HashSet::new();
        for size in allocation_sizes.iter() {
            println!("Allocate {}", *size);
            let block = list.allocate(&mut mem, Bytes(*size));
            assert_eq!(block.size().as_u32(), *size);
            let new_insertion = allocations.insert(block);
            assert!(new_insertion);
            list.sanity_check();
        }
        for block in allocations.iter() {
            println!("Free {}", block.size().as_usize());
            list.free(*block);
            list.sanity_check();
        }
    }
}
