import Prim "mo:prim";
import Migration "Migration3";

// Swap nested pairs in `four`, changing type
actor (with migration = Migration.run) {

   Prim.debugPrint("Version 3");

   stable var zero : Nat = Prim.trap "unreachable"; // inherited
   assert zero == 0;

   stable var four : [var (Text, Nat)] = [var];

   public func check(): async() {
     Prim.debugPrint(debug_show{zero; four});
   }
};
