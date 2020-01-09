import Prim "mo:prim";
let a = actor {
  let aa = actor {
    var c = 1;
    public func inc() {
      c += 1;
      Prim.debugPrintNat(c)
    };
    public func Prim.debugPrint() {
      Prim.debugPrintNat(c)
    };
  };
  public func inc() { aa.inc() };
  public func Prim.debugPrint() { aa.Prim.debugPrint() };
};

a.inc();
a.inc();
a.inc();
a.Prim.debugPrint()

//SKIP comp
