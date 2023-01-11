import Prim "mo:⛔";

actor a {
  public func go() : async Blob {
    let b = await Prim.rand();
    b
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
