shared {caller = c} actor class () {
  let c = 1;

  public shared ctxt func c1 () : async Nat {
     assert (c == 1);
     return c;
  };

};

(); //OR-CALL ingress c1 0x4449444C0000




