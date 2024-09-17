import Prim "mo:⛔";

actor a {
  public shared ({ caller }) func test() : async Nat32 {
    let value = Prim.hashBlob(Prim.blobOfPrincipal caller);
    Prim.debugPrint(debug_show (value));
    Prim.debugPrint(debug_show (value + 1));
    value;
  };
};

ignore a.test(); //OR-CALL ingress test 0x4449444C0000
