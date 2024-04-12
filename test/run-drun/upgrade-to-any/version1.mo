import Prim "mo:prim";

actor {
   Prim.debugPrint("Version 1");

   let allocationSize = 64 * 1024 * 1024;

   func largeAllocation(name: Text): [var Nat] {
      Prim.debugPrint("Initialize " # name);
      Prim.Array_init<Nat>(allocationSize / 8, 0);
   };

   stable var firstVariable : Any = largeAllocation("first variable");
   stable var secondVariable : [var Nat] = largeAllocation("second variable");
   
   public func check(): async() {
      // Extra GC increments.
      await async {};
      await async {};
      await async {};
      // Check that first variable has been cleared and the first array has been reclaimed.
      assert(Prim.rts_heap_size() >= allocationSize);
      assert(Prim.rts_heap_size() < 2 * allocationSize);
   };
};
