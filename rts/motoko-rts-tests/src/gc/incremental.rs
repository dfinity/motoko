mod mark_bit;
mod object_table;

pub fn test() {
    println!("Testing incremental GC components ...");
    unsafe {
        mark_bit::test();
        object_table::test();
    }
}
