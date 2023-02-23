mod mark_stack;

pub fn test() {
    println!("Testing generational GC components ...");
    unsafe {
        mark_stack::test();
    }
}
