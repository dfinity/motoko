mod mark_bit;
mod mark_stack;
mod object_table;

pub fn test() {
    println!("Testing incremental GC components ...");
    unsafe {
        mark_bit::test();
        mark_stack::test();
        object_table::test();
    }
}
