actor {

  public func c1 () with ctxt  : async Int64 {
    return ctxt.caller;
  };

  public func c2 () with ({caller = c}) : async Int64 {
    return c;
  };

  public func c3 () with _ : async Int64 {
    return 0;
  };

  public func c5 () with ({}) {
  };

  public func c6 (c:Bool) with ({caller = c}) : async Int64 {
    return c;
  };

/*
  public query func c4 () with _ : async Int64 {
    return c;
  };
*/
}

//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c1 0x4449444C0000
//CALL ingress c2 0x4449444C0000
//CALL ingress c3 0x4449444C0000


