import Prim "mo:prim";
actor {
  public shared { .caller = c } func getCaller() : async (Principal, Blob) {
    return (c, Prim.blobOfPrincipal c);
  };
}
