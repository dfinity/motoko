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
  public query func read() : async Nat {
    let tmp = c;
    c += 1;
    Prim.debugPrintNat c;
    return tmp;
  };

}
//CALL ingress inc RElETAAA
//CALL ingress inc RElETAAA
//CALL ingress inc RElETAAA
//CALL ingress printCounter RElETAAA
//CALL ingress get RElETAAA
//CALL query read 0x4449444C0000
//CALL ingress printCounter RElETAAA
//CALL query read 0x4449444C0000
//CALL ingress printCounter RElETAAA

//SKIP run
//SKIP run-ir
//SKIP run-low
