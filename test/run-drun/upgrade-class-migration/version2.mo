import Prim "mo:prim";
import Migration "Migration2";

// Rename stable field `three` to `four`
actor (with migration = Migration.run) class C() {

   Prim.debugPrint("Version 2");

   stable var zero : Nat = Prim.trap "unreachable"; // inherited
   assert zero == 0;

   stable var four : [var (Nat, Text)] = [var];

   public func check(): async() {
     Prim.debugPrint(debug_show{zero; four});
   }
};
