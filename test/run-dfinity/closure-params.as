let a = actor {
  private var c = 0;
  incn(n : Nat) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  readCounter(f : shared Nat -> ()) : () {
    f(c);
  };
};

a.incn(1);
a.readCounter(shared func (n : Nat) = { printInt n; print("\n") });
a.incn(2);
a.readCounter(shared func (n : Nat) = { printInt n; print("\n") });
a.incn(3);
a.readCounter(shared func (n : Nat) = { printInt n; print("\n") });
a.incn(4);
a.readCounter(shared func (n : Nat) = { printInt n; print("\n") });
a.incn(5);
a.readCounter(shared func (n : Nat) = { printInt n; print("\n") });
