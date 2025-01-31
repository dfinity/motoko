//MOC-FLAG --enhanced-orthogonal-persistence
import Prim "mo:prim";
import Migration "Migration1";

// test merging fields `one` and `two` into field `three`, dropping `one` and `two` and preserving `zero`.
actor (with migration = Migration.run) {

   Prim.debugPrint("Version 1");

   stable var zero : Nat = Prim.trap "unreachable"; // inherited
   assert zero == 0;

   stable var three : [var (Nat, Text)] = [var];

   public func check(): async() {
     Prim.debugPrint (debug_show {zero; three});
   }
};
