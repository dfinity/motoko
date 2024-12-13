import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 0");

   stable var one : [var Nat] = [var 1,2];
   stable var two : [var Nat] = [var 1,2];

   public func check(): async() {
   };
};
