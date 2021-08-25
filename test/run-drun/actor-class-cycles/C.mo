import Prim = "mo:⛔";
import Cycles = "../cycles/cycles"

actor class C() = c {
  let initial_ = Cycles.balance();
  Prim.debugPrint(debug_show({ initial_C = initial_}));
  public func balance() : async {initial: Nat64; current: Nat64} {
     Prim.debugPrint(debug_show({ Principal = Prim.principalOfActor c }));
     return {
       initial = initial_;
       current = Cycles.balance()
     };
  };
}
