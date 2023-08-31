import Prim "mo:⛔";
import Cycles = "cycles/cycles";
import M1 "class-import/one";
import M2 "class-import/trap";
// Too many class imports exceed the maximum data segment size of 2MB,
// in particular if they are compiled with sanity checks.

actor a {
 public func go() : async () {
   // To get lots of cycles in both drun and ic-ref-run
   if (Cycles.balance() == 0)
     await Cycles.provisional_top_up_actor(a, 100_000_000_000_000);

   // test single arg class
   Cycles.add(2_000_000_000_000);
   let one : M1.One = await M1.One("one");
   await one.test();

   // test non-trapping install
   try {
     Cycles.add(2_000_000_000_000);
     let trap : M2.Trap = await M2.Trap(false);
   }
   catch _ {
     assert false;
   };

   // test trapping install
   try {
     Cycles.add(2_000_000_000_000);
     let trap : M2.Trap = await M2.Trap(true);
     assert false;
   }
   catch _ {
     Prim.debugPrint("caught trap");
   };

 }
};

a.go() //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low

// check exactly 2 embedded wasms
//CHECK:  canister_update __motoko_async_helper
//CHECK:  canister_update __motoko_async_helper
//CHECK:  canister_update __motoko_async_helper
//CHECK-NOT:  canister_update __motoko_async_helper
