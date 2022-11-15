fn main() {
    println!("Crash...");
    #[allow(deref_nullptr)]
    unsafe {
        *(0 as *mut usize) = 0;
    }
}
