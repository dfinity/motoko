import Prim "mo:prim";

persistent actor {

  var blob = Prim.Array_init<Nat64>(3, 1);
  var ref : Weak[var Nat64] = Prim.allocWeakRef(blob);

  public func test1() : async () {
    // Should be live.
    assert (Prim.isLive(ref));
  };

};
