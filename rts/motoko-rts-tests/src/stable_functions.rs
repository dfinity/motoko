mod mark_stack;
mod visited_set;

pub unsafe fn test() {
    println!("Testing stable functions ...");
    mark_stack::test();
    visited_set::test();
}
