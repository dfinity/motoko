mod mark_bit;
mod mark_stack;

pub unsafe fn test() {
    println!("Testing incremental GC ...");
    mark_bit::test();
    mark_stack::test();
}
