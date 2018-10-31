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
a.printCounter();
// a.readCounter(func (n : Nat) = { printInt n; print("\n") });
