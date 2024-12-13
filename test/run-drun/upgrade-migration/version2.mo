import Prim "mo:prim";
import Migration "Migration2";

actor [Migration.run] {

   Prim.debugPrint("Version 2");

   stable var version = 2;
   stable var four : [var (Nat,Nat)] = [var];

   public func check(): async() {
   }
};
