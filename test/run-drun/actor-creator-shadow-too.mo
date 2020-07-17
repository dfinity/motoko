import Prim "mo:prim";
shared {caller = c} actor c { // desugaring implies context lexically within actor name

  public shared ctxt func c1 () : async Principal {
     let b = Prim.blobOfPrincipal c;
     return c;
  };

};

ignore c.c1(); //OR-CALL ingress c1 0x4449444C0000




