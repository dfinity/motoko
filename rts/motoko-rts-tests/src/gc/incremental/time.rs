use motoko_rts::gc::incremental::time::BoundedTime;

pub unsafe fn test() {
    println!("  Testing time...");

    test_tick();
    test_advance();
}

fn test_tick() {
    let mut time = BoundedTime::new(2);
    assert!(!time.is_over());
    time.tick(); // 1
    assert!(!time.is_over());
    time.tick(); // 2
    assert!(!time.is_over());
    time.tick(); // 3
    assert!(time.is_over());
    time.tick(); // 4
    assert!(time.is_over());
}

fn test_advance() {
    let mut time = BoundedTime::new(15);
    assert!(!time.is_over());
    time.advance(10); // 10
    assert!(!time.is_over());
    time.advance(4); // 14
    assert!(!time.is_over());
    time.tick(); // 15
    assert!(!time.is_over());
    time.advance(2); // 17
    assert!(time.is_over());
    time.tick(); // 18
    assert!(time.is_over());
}
