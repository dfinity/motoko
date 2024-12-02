persistent actor Counter {

  var value = 0; // implicity `stable`

  public func inc() : async Nat {
    value += 1;
    return value;
  };
}
