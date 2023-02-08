import Prim = "mo:prim";

// test desabilization of stabilized data, without rts stack overflow.
actor a {

   stable let x = Prim.stableMemoryGrow(1);
   assert Prim.stableMemorySize() == 1;


   system func preupgrade() {
     Prim.debugPrint "preupgrade!";
     fillMB(768);
   };

   system func postupgrade() {
     // if we get here, destabilization has succeeded
     Prim.debugPrint "postupgrade!";
     assert false; // trap to avoid saving state to disk
   }

}
