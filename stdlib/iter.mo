module {
  public func forIn<A>(
    f : (A, Nat) -> (),
    xs : Iter<A>
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

  public func pure<A>(x : A) : Iter<A> = object {
    public func next() : ?A {
      ?x;
    };
  };
}
