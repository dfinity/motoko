import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed 1");
  stable let c = "a";
  stable var i : Nat = c.len();
  public func inc() { i += 1; };
  public query func check(n : Int) : async () {
    Prim.debugPrintNat(i);
    Prim.debugPrint(c);
    assert (i == n);
    assert (c.len() == 3);
    assert (c.len() <= i);
  };
}

