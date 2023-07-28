//MOC-FLAG --stable-regions
import P "mo:⛔";
import {new; size } "stable-region/Region";

// test region allocation is finite
actor {

  public func go() : async() {
    var n : Nat32 = 16; // first 16 regions are reserved
//    assert P.regionId(P.stableMemoryRegion()) == 0;
    loop {
      let r1 = new();
//      assert P.regionId(r) == n;
      assert size(r1) == 0;
      n += 1;
    } while (n < 32768);
    P.debugPrint(debug_show {alloced = n - 1});
    var c = 0;
    while (c < 3) {
      try {
        await async {
          let r = new();
          P.debugPrint("new failed to fail");
          assert false;
        }
      }
      catch e {
        P.debugPrint("caught:" # P.errorMessage(e));
      };
      c += 1;
    }
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress go "DIDL\x00\x00"

