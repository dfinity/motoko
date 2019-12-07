actor a {

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

  public shared {caller = c} func c5 (c:Bool) : async () { // allow shadowing
    let c1 : Bool = c;
    return;
  };

  public shared query {caller = c} func c6 () : async () {
    let c1 : Blob = c;
    return;
  };

  public shared { caller = c } func getCaller()  : async Blob {
    return c;
  };

  public shared {caller} func c7 () : async () {
    let self1 = await getCaller();
    let self2 = await getCaller();
    assert caller != self1; // assuming this is an ingress message
    assert self1 == self2;
  };

};

ignore a.c1(); //OR-CALL ingress c1 0x4449444C0000
ignore a.c2(); //OR-CALL ingress c2 0x4449444C0000
ignore a.c3(); //OR-CALL ingress c3 0x4449444C0000
ignore a.c4(); //OR-CALL ingress c4 0x4449444C0000
ignore a.c6(); //OR-CALL query c6 0x4449444C0000
ignore a.c7(); //OR-CALL ingress c7 0x4449444C0000



