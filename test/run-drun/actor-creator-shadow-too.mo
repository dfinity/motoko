shared {caller = c} actor class () = c {

  public shared ctxt func c1 () : async actor {} {
     return c;
  };

};

(); //OR-CALL ingress c1 0x4449444C0000




