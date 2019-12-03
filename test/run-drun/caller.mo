actor {

  public shared ctxt func c1 ()  : async () {
    let c : Blob = ctxt.caller;
    return;
  };

  public shared {caller = c} func c2 () : async () {
    let c1 : Blob = c;
    return;
  };

  public shared _ func c3 () : async () {
    return;
  };

  public shared {} func c4 () : async () {
  };

/*
  public shared {caller = c} func c5 (c:Bool) : async () { // reject re-rebinding
    let c1 : Bool = c;
    return;
  };

  public query func c6 () with _ : async () {
    return;
  };
*/
}

//CALL ingress c1 0x4449444C0000
//CALL ingress c2 0x4449444C0000
//CALL ingress c3 0x4449444C0000
//CALL ingress c4 0x4449444C0000



