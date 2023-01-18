mod array_slicing;
mod mark_bit;
mod mark_stack;
mod partitioned_heap;
mod roots;
mod time;

pub fn test() {
    println!("Testing incremental GC ...");
    unsafe {
        array_slicing::test();
        mark_bit::test();
        mark_stack::test();
        partitioned_heap::test();
        roots::test();
        time::test();
    }
}
