mod array_slicing;
mod mark_bitmap;
mod mark_stack;
mod partitioned_heap;
mod roots;
mod sort;
mod time;

pub fn test() {
    println!("Testing incremental GC ...");
    unsafe {
        array_slicing::test();
        mark_bitmap::test();
        mark_stack::test();
        partitioned_heap::test();
        sort::test();
        roots::test();
        time::test();
    }
}
