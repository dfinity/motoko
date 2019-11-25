let a = actor {
  var c = 0;
  public func incn(n : Nat) : () {
    c += n;
    debugPrintNat(c);
  };
  public func readCounter(f : shared Nat -> ()) : () {
    f(c);
  };
};

a.incn(1);
a.readCounter(shared func (n : Nat) = { debugPrintNat n });
a.incn(2);
a.readCounter(shared func (n : Nat) = { debugPrintNat n });
a.incn(3);
a.readCounter(shared func (n : Nat) = { debugPrintNat n });
a.incn(4);
a.readCounter(shared func (n : Nat) = { debugPrintNat n });
a.incn(5);
a.readCounter(shared func (n : Nat) = { debugPrintNat n });
