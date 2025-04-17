let MT = module M {
  public type T = Int;
};

let _ = switch (MT) {
  case ({ type T }) {};
};
