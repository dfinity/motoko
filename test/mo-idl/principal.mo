import Prim "mo:prim";
actor {
  public shared { caller = c } func getCaller() : async (Principal, Word32) {
    return (c, Prim.hashBlob (Prim.blobOfPrincipal c));
  };
}
