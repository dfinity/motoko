import Prim "mo:⛔";
import Cycles = "cycles/cycles";
import M0 "class-import/empty";
import M1 "class-import/one";

// split into two actors due to too big debug WASM size

actor a {
 public func go() : async () {
   // To get lots of cycles in both drun and ic-ref-run
   if (Cycles.balance() == 0)
     await Cycles.provisional_top_up_actor(a, 100_000_000_000_000);

   // test no arg class
   Cycles.add(2_000_000_000_000);
   let empty : M0.Empty = await M0.Empty();
   await empty.test();

   // test single arg class
   Cycles.add(2_000_000_000_000);
   let one : M1.One = await M1.One("one");
   await one.test();
 }
};

a.go() //OR-CALL ingress go "DIDL\x00\x00"

//SKIP run
//SKIP run-ir
//SKIP run-low
