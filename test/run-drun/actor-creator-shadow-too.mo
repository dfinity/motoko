shared {.caller = c} actor class C() = c {

  public shared ctxt func c1 () : async actor {} {
     return c;
  };

};

ignore C().c1(); //OR-CALL ingress c1 0x4449444C0000
