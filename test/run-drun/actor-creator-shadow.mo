import Prim "mo:prim";
shared {caller = c} actor c {
  let c = 1;

  public shared ctxt func c1 () : async Nat {
     return c;
  };

};

ignore c.c1(); //OR-CALL ingress c1 0x4449444C0000




