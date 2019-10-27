actor {

  public func c1 () with ctxt  : async () {
    let c : EntityId = ctxt.caller;
    return;
  };

  public func c2 () with ({caller = c}) : async () {
    let c1 : EntityId = c;
    return;
  };

  public func c3 () with _ : async () {
    return;
  };

  public func c5 () with {} {
  };

  public func c6 (c:Bool) with {caller = c} : async () {
    let c1 : EntityId = c;
    return;
  };

/*
  public query func c4 () with _ : async () {
    return;
  };
*/
}

//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c2 0x4449444C0000
//CALL ingress c3 0x4449444C0000


