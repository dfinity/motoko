import Prim "mo:prim";
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
//CALL ingress inc 0x4449444C0000
//CALL ingress inc 0x4449444C0000
//CALL ingress inc 0x4449444C0000
//CALL ingress printCounter 0x4449444C0000
//CALL ingress get 0x4449444C0000
//CALL query read 0x4449444C0000
//CALL ingress printCounter 0x4449444C0000
//CALL query read 0x4449444C0000
//CALL ingress printCounter 0x4449444C0000

//SKIP run
//SKIP run-ir
//SKIP run-low
