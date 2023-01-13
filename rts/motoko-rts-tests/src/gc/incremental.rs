mod array_slicing;
mod mark_bit;
mod mark_stack;
mod partitioned_heap;

pub unsafe fn test() {
    println!("Testing incremental GC ...");
    array_slicing::test();
    mark_bit::test();
    mark_stack::test();
    partitioned_heap::test();
}
