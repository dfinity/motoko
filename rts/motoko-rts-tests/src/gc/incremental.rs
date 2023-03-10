mod array_slicing;
mod mark_bit;
mod mark_stack;
mod object_table;
mod roots;
mod state;
mod time;

pub fn test() {
    println!("Testing incremental GC components ...");
    unsafe {
        array_slicing::test();
        mark_bit::test();
        mark_stack::test();
        object_table::test();
        roots::test();
        state::test();
        time::test();
    }
}
