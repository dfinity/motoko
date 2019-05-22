module Wrong = {
  func f() { assert false;};
  let _ = f(); // non_static!
};

