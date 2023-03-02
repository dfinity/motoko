mod object_table;

pub fn test() {
    println!("Testing incremental GC components ...");
    unsafe {
        object_table::test();
    }
}
