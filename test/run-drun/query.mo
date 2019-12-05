actor {
  var c = 1;
  public func inc() {
    c += 1;
    debugPrintNat c
  };
  public func printCounter () {
    debugPrintNat c
  };
  public func get() : async Nat {
    return c
  };
  public query func read() : async Nat {
    let tmp = c;
    c += 1;
    debugPrintNat c;
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
