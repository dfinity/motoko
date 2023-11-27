//MOC-FLAG --incremental-gc
import P "mo:⛔";

actor {
  stable let p = P.stableMemoryGrow(1);
  assert (p == 0);
  stable var a : [Blob] = [];

  system func preupgrade() {
    a := P.Array_tabulate(32768,func (i:Nat) : Blob {
      P.debugPrint(debug_show {i; p=P.performanceCounter(0)});
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

