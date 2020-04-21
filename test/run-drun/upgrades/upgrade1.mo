import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed 1");
  stable var c = "a";
  stable var i : Nat = c.len();
  public func inc() { i += 1; };
  public query func check(n : Int) : async () {
    Prim.debugPrintNat(i);
    assert (i == n);
  };
}

