mod mark_bit;
mod mark_stack;
mod remembered_set;

pub unsafe fn test() {
    println!("Testing incremental GC ...");
    mark_bit::test();
    mark_stack::test();
    remembered_set::test();
}
