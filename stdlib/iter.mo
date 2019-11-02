import Array "array.mo";

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

  func length<A>(xs : Iter<A>) : Nat {
    var len = 0;
    forIn<A>(func (x, i) { len += 1; }, xs);
    len;
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

  public func toArray<A>(xs : Iter<A>) : [A] {
    Array.freeze<A>(toArrayMut<A>(xs));
  };

  public func toArrayMut<A>(xs : Iter<A>) : [var A] {
    let first = xs.next();
    switch (first) {
      case null { [var]; };
      case (?x0) {
        // FIXME: `length` consumes the iterator
        let len = length<A>(xs) + 1;
        let array = Array_init<A>(len, x0);
        forIn<A>(func (x : A, i : Nat) {
          array[i + 1] := x;
        }, xs);
        array;
      };
    };
  };
}
