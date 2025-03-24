mod bitmap;
mod mark_stack;

pub fn test() {
    println!("Testing compacting GC components ...");
    unsafe {
        bitmap::test();
        mark_stack::test();
    }
}
