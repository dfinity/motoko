import Prim "mo:prim";
let a = actor {
  flexible var c = 1;
  public func inc() {
    c += 1;
    Prim.debugPrintNat c;
  };
  public func printCounter () {
    Prim.debugPrintNat c;
  };

  public func go() {
    a.inc();
    a.inc();
    a.inc();
    a.printCounter();

    var i : Int = 10;
    while (i  > 0) {
      a.inc();
      i -= 1;
    };
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
