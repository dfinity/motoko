actor a {
  var c = 1;
  public func inc() {
    c += 1;
    debugPrintNat c;
  };
  public func printCounter () {
    debugPrintNat c;
  };
  public func get() : async Nat {
    return c
  };
}
//CALL ingress inc 0x4449444C0000
//CALL ingress inc 0x4449444C0000
//CALL ingress inc 0x4449444C0000
//CALL ingress printCounter 0x4449444C0000
//CALL ingress get 0x4449444C0000
