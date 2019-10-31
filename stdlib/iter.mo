module {
  public func forIn<A>(
    f : (A, Nat) -> (),
    xs : { next : () -> ?A; }
  ) {
    var i = 0;
    label l loop {
      switch (xs.next()) {
        case (?next) {
          f(next, i);
        };
        case (null) {
          break l;
        };
      };
      i += 1;
      continue l;
    };
  };
}
