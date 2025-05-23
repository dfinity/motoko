import Prim "mo:⛔";

actor Counter {

  stable var count : Nat = 0;

  public func inc() : async Nat {
    count += 1;
    Prim.debugPrint (debug_show(count));
    count
  };

  system func preupgrade() {
    Prim.debugPrint (debug_show({pre=count}));
  };

  flexible let f = func(){};
  func g() {};
  class D() {};
  type T = ?T;
  ignore 1;

}
