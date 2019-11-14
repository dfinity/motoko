actor {
  var c = 1;
  public func inc() {
    c += 1;
    Debug.printNat c; Debug.print "\n";
  };
  public func printCounter () {
    Debug.printNat c; Debug.print "\n";
  };
  public func get() : async Nat {
    return c
  };
  public query func read() : async Nat {
    let tmp = c;
    c += 1;
    Debug.printNat c; Debug.print "\n";
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
