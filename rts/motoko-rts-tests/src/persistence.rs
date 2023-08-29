mod compatibility;

use motoko_rts::persistence::allocate_null;
use motoko_rts::types::TAG_NULL;

use crate::memory::{initialize_test_memory, reset_test_memory};

pub fn test() {
    println!("Testing orthogonal persistence ...");
    unsafe {
        compatibility::test();
        test_null_allocation();
    }
}

unsafe fn test_null_allocation() {
    let mut heap = initialize_test_memory();
    let value = allocate_null(&mut heap);
    assert_eq!(value.tag(), TAG_NULL);
    reset_test_memory();
}
