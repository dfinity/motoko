//MOC-FLAG --incremental-gc
// This would let us test that destablization of >2GB works,
// if only drun could execute it properly.
import P "mo:⛔";

actor {
  stable let p = P.stableMemoryGrow(1);
  assert (p == 0);
  stable var a : [Blob] = [];

  system func preupgrade() {
    a := P.Array_tabulate(32768+16384,func (i:Nat) : Blob {
      P.stableMemoryLoadBlob(0, 65535) });  // use ca. 3GB main memory
    P.debugPrint("upgrading...");
  };

  system func postupgrade() {
   P.debugPrint("...upgraded");
   assert false;
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL upgrade ""

