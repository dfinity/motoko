mod buffered_access;
mod stable_memory;

pub unsafe fn test() {
    println!("Testing stabilization ...");
    buffered_access::test();
}
