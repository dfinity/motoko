import Prim "mo:⛔";
import Cycles = "cycles/cycles";
import M2 "class-import/two";
import M3 "class-import/trap";

// split into two actors due to too big debug WASM size

actor a {
 public func go() : async () {
   // To get lots of cycles in both drun and ic-ref-run
   if (Cycles.balance() == 0)
     await Cycles.provisional_top_up_actor(a, 100_000_000_000_000);

   // test two arg class
   Cycles.add(2_000_000_000_000);
   let two : M2.Two = await M2.Two("one","two");
   await two.test();

   // test non-trapping install
   try {
     Cycles.add(2_000_000_000_000);
     let trap : M3.Trap = await M3.Trap(false);
   }
   catch _ {
     assert false;
   };

   // test trapping install
   try {
     Cycles.add(2_000_000_000_000);
     let trap : M3.Trap = await M3.Trap(true);
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
