import Prim "mo:prim";
actor a {

  public shared(c) func getCaller() : async Principal {
    return c.caller;
  };

  public shared query({caller}) func getCallerHash() : async Word32 {
    Prim.hashBlob(Prim.blobOfPrincipal(caller));
  };

  public shared query({caller}) func getCallerSize() : async Nat {
    Prim.blobOfPrincipal(caller).size();
  };

  public shared({caller}) func c() : async () {
    let self1 = await getCaller();
    let self2 = await getCaller();
    assert caller != self1; // assuming this is an ingress message
    assert self1 == self2;
  };

};

ignore a.c(); //OR-CALL ingress c 0x4449444C0000
ignore a.getCallerHash(); //OR-CALL ingress getCallerHash 0x4449444C0000
ignore a.getCallerSize(); //OR-CALL ingress getCallerSize 0x4449444C0000
