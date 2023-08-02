use motoko_rts::gc::incremental::time::BoundedTime;

pub unsafe fn test() {
    println!("  Testing time...");

    test_tick();
    test_advance();
    test_saturation();
}

fn test_tick() {
    let mut time = BoundedTime::new(3);
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
    let mut time = BoundedTime::new(16);
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

fn test_saturation() {
    let mut time = BoundedTime::new(usize::MAX);
    assert!(!time.is_over());
    time.advance(usize::MAX - 1);
    assert!(!time.is_over());
    time.tick();
    assert!(time.is_over());
    time.tick();
    assert!(time.is_over());
    time.advance(100);
    assert!(time.is_over());

    time = BoundedTime::new(usize::MAX);
    time.advance(usize::MAX / 2 + 1);
    assert!(!time.is_over());
    time.advance(usize::MAX / 2 + 1);
    assert!(time.is_over());
    time.tick();
    assert!(time.is_over());
}
