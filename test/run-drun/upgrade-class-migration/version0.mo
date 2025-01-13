import Prim "mo:prim";

actor class C() {
   Prim.debugPrint("Version 0");

   stable var one : [var Nat] = [var 1, 2, 3, 4];
   stable var two : [var Text] = [var "1", "2", "3", "4"];

   public func check(): async() {
   };
};
