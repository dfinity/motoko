import Prim "mo:prim";
actor ({caller = creator}) a {

  let c : Principal = creator;
  let d = creator;

  public shared ctxt func c1 () : async Principal {
     return c;
  };


};

ignore a.c1(); //OR-CALL ingress c1 0x4449444C0000


