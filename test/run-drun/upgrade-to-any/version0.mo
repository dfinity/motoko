import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 0");

   let allocationSize = 40_000_000;

   func largeAllocation(name: Text): [var Nat] {
      Prim.debugPrint("Initialize " # name);
      Prim.Array_init<Nat>(allocationSize / 4, 0);
   };

   stable var firstVariable : [var Nat] = largeAllocation("first variable");
};
