mod mark_stack;

pub fn test() {
    println!("Testing compacting GC components ...");
    unsafe {
        mark_stack::test();
    }
}
