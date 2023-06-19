import Prim "mo:⛔";
actor {
  Prim.debugPrint ("init'ed 0");
  Prim.debugPrint ("initial version: " # debug_show Prim.canisterVersion());
  stable var c = "a";
  public func inc() { c #= "a"; };
  public query func check(n : Int) : async () {
    Prim.debugPrint(c);
    assert (c.size() == n);
  };
}
