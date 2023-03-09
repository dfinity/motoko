use motoko_rts::gc::incremental::time::Time;

pub unsafe fn test() {
    println!("  Testing time...");

    test_tick();
    test_advance();
    test_unlimited();
}

fn test_tick() {
    let mut time = Time::limited(2);
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
    let mut time = Time::limited(15);
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

fn test_unlimited() {
    let mut time = Time::unlimited();
    assert!(!time.is_over());
    time.advance(usize::MAX);
    assert!(!time.is_over());
    time.tick();
    assert!(!time.is_over());

    let mut time = Time::unlimited();
    assert!(!time.is_over());
    time.advance(usize::MAX - 10);
    assert!(!time.is_over());
    time.advance(10);
    assert!(!time.is_over());
    time.tick();
    assert!(!time.is_over());

    let mut time = Time::unlimited();
    assert!(!time.is_over());
    time.advance(usize::MAX - 1);
    assert!(!time.is_over());
    time.tick();
    assert!(!time.is_over());
    time.tick();
    assert!(!time.is_over());
    time.advance(10);
    assert!(!time.is_over());
    time.advance(usize::MAX);
    assert!(!time.is_over());
}
