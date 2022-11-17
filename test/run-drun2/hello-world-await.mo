import Prim "mo:⛔";
let a = actor {
  public func hello() : async Text {
    "Hello ";
  };
  public func world() : async Text {
    "World!"
  };
  public func go() : async () {
    Prim.debugPrint((await hello()) # (await world()));
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
