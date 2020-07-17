import Prim "mo:prim";
shared {caller = creator} actor a {

  let c : Principal = creator;
  let d = creator;

  public shared ctxt func c1 () : async Principal {
     return c;
  };

  public shared ctxt func c2 () : async Principal {
     return d;
  };


};


ignore a.c1(); //OR-CALL ingress c1 0x4449444C0000
ignore a.c2(); //OR-CALL ingress c2 0x4449444C0000


