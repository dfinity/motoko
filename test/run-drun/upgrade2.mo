import Prim "mo:prim";
actor {
  Prim.debugPrint ("init'ed 2");
  // stable var c = "a";
  stable var i : Nat = { assert false; loop {}};
  public func inc() { i += 1; };
  public query func check(n : Int) : async () {
    assert (i == n);
  };
}

