import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed 2");
  stable let c : Text = { assert false; loop {}};
  stable var i : Nat = { assert false; loop {}};
  public func inc() { i += 1; };
  public query func check(n : Int) : async () {
    assert (c.size() == 3);
    assert (i == n);
  };
}

