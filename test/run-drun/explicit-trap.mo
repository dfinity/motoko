import Prim "mo:⛔";
actor a {
  public func go() {
    Prim.trap("This is an explicit trap");
  }
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
