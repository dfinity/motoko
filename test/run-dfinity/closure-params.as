let a = actor {
  private var c = 0;
  incn(n : Nat) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  readCounter(f : Nat -> ()) : () {
    f(c);
  };
};

a.incn(1);
a.readCounter(func (n : Nat) = { printInt n; print("\n") });
a.incn(2);
a.readCounter(func (n : Nat) = { printInt n; print("\n") });
a.incn(3);
a.readCounter(func (n : Nat) = { printInt n; print("\n") });
a.incn(4);
a.readCounter(func (n : Nat) = { printInt n; print("\n") });
a.incn(5);
a.readCounter(func (n : Nat) = { printInt n; print("\n") });
