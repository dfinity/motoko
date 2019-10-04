actor {
  public func absolute(x:Int) : async Nat {
    abs x
  };
}

// with Nat
//CALL query absolute "DIDL\x00\x01\x7d\x2a"
// with Int
//CALL query absolute "DIDL\x00\x01\x7c\x56"
