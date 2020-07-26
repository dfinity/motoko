import Prim "mo:prim";
actor a {

  public shared { caller = c } func getCaller() : async Principal {
    return c;
  };

  public shared query { caller = c } func getCallerHash() : async Word32 {
    Prim.hashBlob (Prim.blobOfPrincipal c);
  };

  public shared query { caller = c } func getCallerSize() : async Nat {
      (Prim.blobOfPrincipal c).size();
  };

  public shared {caller} func c () : async () {
    let self1 = await getCaller();
    let self2 = await getCaller();
    assert caller != self1; // assuming this is an ingress message
    assert self1 == self2;
  };

};

ignore a.c(); //OR-CALL ingress c 0x4449444C0000
ignore a.getCallerHash(); //OR-CALL ingress getCallerHash 0x4449444C0000
ignore a.getCallerSize(); //OR-CALL ingress getCallerSize 0x4449444C0000
