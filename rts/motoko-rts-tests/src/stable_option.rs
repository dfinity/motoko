use motoko_rts::stable_option::StableOption;

pub fn test() {
    println!("Testing StableOption ...");

    let mut none: StableOption<usize> = StableOption::None;
    assert!(none.is_none());
    assert!(!none.is_some());
    assert!(none.as_ref().is_none());
    assert!(!none.as_ref().is_some());
    assert!(none.as_mut().is_none());
    assert!(!none.as_mut().is_some());

    const TEST_VALUE: usize = 123456;
    let mut some = Some(TEST_VALUE);
    assert!(!some.is_none());
    assert!(some.is_some());
    assert_eq!(some.unwrap(), TEST_VALUE);
    assert_eq!(some.as_ref().unwrap(), &TEST_VALUE);
    assert_eq!(*some.as_mut().unwrap(), TEST_VALUE);

    *some.as_mut().unwrap() = 0;
    assert_eq!(some.unwrap(), 0);
}
