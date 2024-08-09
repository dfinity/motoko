import Prim "mo:prim";

actor {
  stable let value : {
  } = {
    stableField = "Version 1";
  };

  public func print() : async () {
    Prim.debugPrint(debug_show (value));
  };
};
