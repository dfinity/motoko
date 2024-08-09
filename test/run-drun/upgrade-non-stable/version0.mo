import Prim "mo:prim";

actor {
  let temporary = 1;

  stable let value : {
    stableField : Text;
  } = {
    stableField = "Version 0";
    nonStableField = func() {
      Prim.debugPrint(debug_show (temporary));
    };
    unreachableField = -123;
  };

  public func print() : async () {
    Prim.debugPrint(debug_show (value));
  };
};
