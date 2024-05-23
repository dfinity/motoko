shared({caller = creator}) actor class C() {

  let c : Principal = creator;
  let d = creator;

  public shared ctxt func c1 () : async Principal {
     return c;
  };

  public shared ctxt func c2 () : async Principal {
     return d;
  };


};

let c = await C(); ignore c.c1(); //OR-CALL ingress c1 RElETAAA
ignore c.c2(); //OR-CALL ingress c2 RElETAAA
