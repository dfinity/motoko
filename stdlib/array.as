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
}
