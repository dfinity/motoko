import Prim "mo:prim";

actor {
  stable let value: Null = null;

  public func print() : async () {
    Prim.debugPrint(debug_show (value));
  };
};
