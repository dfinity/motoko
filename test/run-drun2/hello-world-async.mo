import Prim "mo:⛔";
actor a {
  public func world() : async () {
    Prim.debugPrint("World!");
  };
  public func go() : async () {
    let x = a.world();
    Prim.debugPrint("Hello ");
    await x;
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
