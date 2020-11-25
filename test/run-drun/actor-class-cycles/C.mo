import Prim = "mo:prim";
import Cycles = "../cycles/cycles"

actor class C() {
  let initial_ = Cycles.balance();
  Prim.debugPrint(debug_show({ initial = initial_}));
  public func balance() : async {initial: Nat64; current: Nat64} {
     ignore Cycles.accept(Cycles.available());
     return {
       initial = initial_;
       current = Cycles.balance()
     };
  };
}