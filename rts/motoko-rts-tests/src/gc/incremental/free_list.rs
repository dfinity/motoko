use std::{collections::HashSet, mem::size_of, ptr::null_mut};

use motoko_rts::{
    gc::incremental::free_list::SegregatedFreeList,
    types::{unmark, Blob, Bytes, Obj, TAG_BLOB},
};

use crate::memory::TestMemory;

const SIZE_CLASSES: [usize; 4] = [8, 16, 20, 512];

pub unsafe fn test() {
    println!("  Testing free list...");
    test_allocate_free();
    test_split_merge();
}

unsafe fn test_allocate_free() {
    println!("    Testing allocate/free ...");

    println!("      Uniform sizes ...");
    // uniform sizes, no splitting, no fragmentation issues due to same sized blocks
    let allocation_sizes = vec![16];
    let memory_size = allocation_sizes.iter().sum();
    allocate_free(&allocation_sizes, memory_size);

    println!("      Mixed sizes ...");
    // different sizes, external and internal fragmentation
    let allocation_sizes = vec![8, 4096, 8, 12, 16, 32, 124, 128, 132, 16];
    let memory_size = allocation_sizes.iter().sum::<u32>() * 2;
    allocate_free(&allocation_sizes, memory_size);

    println!("      Overflow sizes ...");
    // overflow sizes, external fragmentation
    let allocation_sizes = vec![260, 512, 700, 516, 704, 600, 300, 520, 1024];
    let memory_size = allocation_sizes.iter().sum::<u32>() * 4;
    allocate_free(&allocation_sizes, memory_size);
}

unsafe fn test_split_merge() {
    println!("    Testing split/merge ...");

    println!("      Uniform sizes ...");
    // same sized free blocks, fits a size class to avoid fragmentation
    split_merge(1024, &[8]);

    println!("      Mixed sizes ...");
    // mixed free fillers and free blocks, of same size class to avoid fragmentation
    split_merge(1024, &[16, 32]);
}

const ROUNDS: usize = 8;

unsafe fn allocate_free(allocation_sizes: &Vec<u32>, total_size: u32) {
    let mut mem = TestMemory::new(Bytes(total_size).to_words());
    let mut list = SegregatedFreeList::new_specific(&SIZE_CLASSES);
    for _ in 0..ROUNDS {
        let mut allocations: HashSet<*mut Blob> = HashSet::new();
        for size in allocation_sizes.iter() {
            let blob = allocate(&mut list, &mut mem, *size);
            let new_insertion = allocations.insert(blob);
            assert!(new_insertion);
        }
        for blob in allocations.iter() {
            free(&mut list, *blob);
        }
    }
}

unsafe fn allocate(list: &mut SegregatedFreeList, mem: &mut TestMemory, size: u32) -> *mut Blob {
    assert!(size as usize >= size_of::<Blob>());
    let value = list.allocate(mem, Bytes(size));
    let blob = value.get_ptr() as *mut Blob;
    (*blob).header.raw_tag = unmark(TAG_BLOB);
    (*blob).len = Bytes(size - size_of::<Blob>() as u32);
    list.sanity_check();
    blob
}

unsafe fn free(list: &mut SegregatedFreeList, blob: *mut Blob) {
    assert!(!(blob as *mut Obj).is_marked());
    let address = blob as usize;
    let length = (*blob).len + Bytes(size_of::<Blob>() as u32);
    let block = SegregatedFreeList::create_free_space(address, length);
    assert_ne!(block, null_mut());
    list.add_block(block);
    list.sanity_check();
}

unsafe fn split_merge(amount: u32, small_sizes: &[u32]) {
    let total_size = amount * small_sizes.iter().sum::<u32>();
    let mut mem = TestMemory::new(Bytes(total_size).to_words());
    let mut list = SegregatedFreeList::new();
    let large = allocate(&mut list, &mut mem, total_size);
    free(&mut list, large);
    for _ in 0..ROUNDS {
        for _ in 0..amount {
            for size in small_sizes {
                allocate(&mut list, &mut mem, *size);
            }
        }
        assert_eq!(list.total_size(), Bytes(0));
        let block = SegregatedFreeList::create_free_space(large as usize, Bytes(total_size));
        assert_ne!(block, null_mut());
        list.add_block(block);
        assert_eq!(list.total_size(), Bytes(total_size));
        list.sanity_check();
    }
}
