import Prim "mo:⛔";
actor {
  Prim.debugPrint ("init'ed 0");
  stable var c = "a";
  public func inc() { c #= "a"; };
  public query func check(n : Int) : async () {
    Prim.debugPrint(c);
    assert (c.size() == n);
  };
}

