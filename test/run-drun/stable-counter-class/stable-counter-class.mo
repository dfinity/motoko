import Prim "mo:⛔";

actor class Counter() stable {

  var count : Nat = 0;

  public func inc() : async Nat {
    count += 1;
    Prim.debugPrint (debug_show(count));
    count
  };

  system func preupgrade() {
    Prim.debugPrint (debug_show({pre=count}));
  }

  // let f = func(){}; // rejected as unstable

}


