import Prim "mo:prim";
import Migration "Migration2";

actor [Migration.run] {

   Prim.debugPrint("Version 2");

   stable var version = 2;
   stable var four : [var (Nat, Text)] = [var];

   public func check(): async() {
     Prim.debugPrint(debug_show{four});
   }
};
