actor a {

  public shared { caller = c } func getCaller() : async Blob {
    return c;
  };

  public shared query { caller = c } func getCallerHash() : async Word32 {
    hashBlob c;
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
