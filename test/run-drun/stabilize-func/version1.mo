//MOC-FLAG --stable-types
import Prim "mo:prim";

// Incompatible upgrade
actor {
  type Variant = {
    #two : { key : Nat; var name : Text };
    #one : Nat;
  };

  stable var root: Variant = #one 0;

  stable func getRoot() : Variant { root };

  stable let record = { getRoot = getRoot };

  public func modify() : async () {
    switch root {
      case (#two record) record.name #= " TEST";
      case _ assert false;
    };
  };

  public func print() : async () {
    Prim.debugPrint(debug_show(getRoot()));
    Prim.debugPrint(debug_show(record.getRoot()));
  };
};
