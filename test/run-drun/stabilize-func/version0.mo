import Prim "mo:prim";

actor {
  type Variant = {
    #one : Nat;
    #two : { key : Nat; var name : Text };
  };

  stable var root : Variant = #two { key = 1; var name = "TEST" };

  stable func getRoot() : Variant { root };

  stable let record = { getRoot = getRoot };

  public func modify() : async () {
    switch root {
      case (#two record) record.name #= " TEST";
      case _ assert false;
    };
  };

  public func print() : async () {
    Prim.debugPrint(debug_show (getRoot()));
    Prim.debugPrint(debug_show (record.getRoot()));
  };
};
