let a = actor {
  private var c = 0;
  incn(n : Nat) : () {
    c += n;
    printInt(c);
    print("\n");
  };
  incnn(n1 : Nat, n2 : Nat) : () {
    c += n1 + n2;
    printInt(c);
    print("\n");
  };
  incnested(n1 : Nat, (n2 : Nat, n3: Nat)) : () {
    c += n1 + n2 + n3;
    printInt(c);
    print("\n");
  };
  incarray(a : Nat[]) : () {
    for (i in a.vals()) { c += i };
    printInt(c);
    print("\n");
  };
  printCounter () {
    printInt(c);
    print("\n");
  };
  readCounter(f : Nat -> ()) : () {
    f(c);
  };
};

a.incn(1);
a.incn(2);
a.incn(3);
a.incn(4);
a.incnn(5,6);
a.incnested(7,(8,9));
a.incarray([10,11,12,13]);
a.printCounter();
// a.readCounter(func (n : Nat) = { printInt n; print("\n") });
