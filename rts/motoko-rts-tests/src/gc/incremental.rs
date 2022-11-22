mod free_list;
mod mark_bit;
mod mark_stack;

pub unsafe fn test() {
    println!("Testing incremental GC ...");
    free_list::test();
    mark_bit::test();
    mark_stack::test();
}
