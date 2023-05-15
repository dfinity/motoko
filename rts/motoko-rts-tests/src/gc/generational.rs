mod mark_stack;
mod remembered_set;

pub fn test() {
    println!("Testing generational GC ...");
    unsafe {
        mark_stack::test();
        remembered_set::test();
    }
}
