import Prim "mo:prim";
let a = actor {
  public func hello() : async Text {
    "Hello ";
  };
  public func world() : async Text {
    "World!"
  };
  public func go() = ignore async  {
    Prim.debugPrint((await hello()) # (await world()));
  };
};
a.go(); //OR-CALL ingress go "DIDL\x00\x00"
