import P "mo:prim";
// This test would fail with OOM if stable serialization did not preserve sharing of mutable arrays.
// Note that it does fail with OOM if we replace 'a' with an immutable array, for which sharing is not preserved. 
// Our users may not thank us that we only preserve sharing for mutable data, but nothing else.
actor {
  stable let a = P.Array_init(65536, 0);
  // stable let a = P.Array_tabulate(65536,func (_:Nat) : Nat { 0 });  // leads to explosion due to exponential serialized size
  stable let a1 = [a, a];
  stable let a2 = [a1, a1];
  stable let a3 = [a2, a2];
  stable let a4 = [a3, a3];
  stable let a5 = [a4, a4];
  stable let a6 = [a5, a5];
  stable let a7 = [a6, a6];
  stable let a8 = [a7, a7];

  system func preupgrade() {
   P.debugPrint("upgrading...");
  };

  system func postupgrade() {
   // verify (some) aliasing
   a[0] += 1;
   let v = a[0];

   for (a in a1.vals())
     { assert a[0] == v };

   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a3 in a4.vals())
   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a4 in a5.vals())
   for (a3 in a4.vals())
   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a5 in a6.vals())
   for (a4 in a5.vals())
   for (a3 in a4.vals())
   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a6 in a7.vals())
   for (a5 in a6.vals())
   for (a4 in a5.vals())
   for (a3 in a4.vals())
   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   for (a7 in a8.vals())
   for (a6 in a7.vals())
   for (a5 in a6.vals())
   for (a4 in a5.vals())
   for (a3 in a4.vals())
   for (a2 in a3.vals())
   for (a1 in a2.vals())
   for (a in a1.vals())
     { assert a[0] == v };

   P.debugPrint("...upgraded");
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL upgrade ""
//CALL upgrade ""

