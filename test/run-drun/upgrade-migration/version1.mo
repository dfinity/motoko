import Prim "mo:prim";
import Migration "Migration1";

actor [Migration.run] {

   Prim.debugPrint("Version 1");

   stable var version = 0;

   stable var three : [var (Nat,Nat)] = [var ];

   public func check(): async() {
   }
};
