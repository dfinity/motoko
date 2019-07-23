module {
  public func append<A>(xs : [A], ys : [A]) : [A] {
    switch(xs.len(), ys.len()) {
      case (0, 0) { []; };
      case (0, _) { ys; };
      case (_, 0) { xs; };
      case (xsLen, ysLen) {
        Array_tabulate<A>(xsLen + ysLen, func (i : Nat) : A {
          if (i < xsLen) {
            xs[i];
          } else {
            ys[i - xsLen];
          };
        });
      };
    };
  };

  public func apply<A, B>(fs : [A -> B], xs : [A]) : [B] {
    let fsLen = fs.len();
    let xsLen = xs.len();
    var ys : [B] = [];
    for (f in fs.vals()) {
      ys := append<B>(ys, map<A, B>(f, xs));
    };
    ys;
  };

  public func bind<A, B>(xs : [A], f : A -> [B]) : [B] {
    var ys : [B] = [];
    for (i in xs.keys()) {
      ys := append<B>(ys, f(xs[i]));
    };
    ys;
  };

  public func foldl<A, B>(f : (B, A) -> B, initial : B, xs : [A]) : B {
    var acc = initial;
    let len = xs.len();
    var i = 0;
    while (i < len) {
      acc := f(acc, xs[i]);
      i += 1;
    };
    acc;
  };

  public func foldr<A, B>(f : (A, B) -> B, initial : B, xs : [A]) : B {
    var acc = initial;
    let len = xs.len();
    var i = len;
    while (i > 0) {
      i -= 1;
      acc := f(xs[i], acc);
    };
    acc;
  };

  public func find<A>(f : A -> Bool, xs : [A]) : ?A {
    for (x in xs.vals()) {
      if (f(x)) {
        return ?x;
      }
    };
    return null;
  };

  public func freeze<A>(xs : [var A]) : [A] {
    Array_tabulate<A>(xs.len(), func (i : Nat) : A {
      xs[i];
    });
  };

  public func join<A>(xs : [[A]]) : [A] {
    bind<[A], A>(xs, func (x : [A]) : [A] {
      x;
    });
  };

  public func map<A, B>(f : A -> B, xs : [A]) : [B] {
    Array_tabulate<B>(xs.len(), func (i : Nat) : B {
      f(xs[i]);
    });
  };

  public func pure<A>(x: A) : [A] {
    [x];
  };

  public func thaw<A>(xs : [A]) : [var A] {
    let xsLen = xs.len();
    if (xsLen == 0) {
      return [var];
    };
    let ys = Array_init<A>(xsLen, xs[0]);
    for (i in ys.keys()) {
      ys[i] := xs[i];
    };
    ys;
  };
}
