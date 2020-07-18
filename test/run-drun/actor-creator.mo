shared {caller = creator} actor class () {

  let c : Principal = creator;
  let d = creator;

  public shared ctxt func c1 () : async Principal {
     return c;
  };

  public shared ctxt func c2 () : async Principal {
     return d;
  };


};

// no way to test an anonymous class in the interpreter
(); //OR-CALL ingress c1 0x4449444C0000
(); //OR-CALL ingress c2 0x4449444C0000


