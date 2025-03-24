import Prim "mo:⛔";

actor class TestActor(number : Nat, text : Text, array : [Nat]) {
  stable var stableNat = number;
  stable var stableInt = -number;
  stable var stableText = text;
  stable var stableArray = array;

  public func print() : async () {
    Prim.debugPrint(debug_show (number));
    Prim.debugPrint(debug_show (text));
    Prim.debugPrint(debug_show (array.size()));
    Prim.debugPrint(debug_show (stableNat));
    Prim.debugPrint(debug_show (stableInt));
    Prim.debugPrint(debug_show (stableText));
    Prim.debugPrint(debug_show (stableArray.size()));
  };

  system func preupgrade() {
    Prim.debugPrint("PRE-UPGRADE HOOK!");
  };

  system func postupgrade() {
    Prim.debugPrint("POST-UPGRADE HOOK!");
  };
};
