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

  public func map<A, B>(f : A -> B, xs : Iter<A>) : Iter<B> = object {
    var i = 0;
    public func next() : ?B {
      label l loop {
        switch (xs.next()) {
          case (?next) {
            return ?f(next);
          };
          case (null) {
            break l;
          };
        };
        i += 1;
        continue l;
      };
      null;
    };
  };

  public func pure<A>(x : A) : Iter<A> = object {
    public func next() : ?A {
      ?x;
    };
  };
}
