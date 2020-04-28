import Prim "mo:prim";
actor a {
  public func world() {
    Prim.debugPrint("World!");
  };
  public func go() : async () {
    a.world();
    Prim.debugPrint("Hello ");
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
