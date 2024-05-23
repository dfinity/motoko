import Prim "mo:⛔";
actor {
  flexible var c = 1;
  public func inc() {
    c += 1;
    Prim.debugPrintNat c
  };
  public func printCounter () {
    Prim.debugPrintNat c
  };
  public func get() : async Nat {
    return c
  };
}
//CALL ingress inc RElETAAA
//CALL ingress inc RElETAAA
//CALL ingress inc RElETAAA
//CALL ingress printCounter RElETAAA
//CALL ingress get RElETAAA
