import Prim "mo:prim";
import Migration "Migration2";

// Rename stable field `three` to `four`
actor [Migration.run] {

   Prim.debugPrint("Version 2");

   stable var zero : Nat = 0; // inherited

   stable var four : [var (Nat, Text)] = [var (1, "1")];

   public func check(): async() {
     Prim.debugPrint(debug_show{zero; four});
   }
};
