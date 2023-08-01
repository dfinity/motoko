//MOC-FLAG --stable-regions
import P "mo:⛔";
import {new; size } "stable-region/Region";

// test region allocation is finite
actor {

  public func go() : async() {
    var n = 16; // first 16 regions are reserved
    assert P.regionId(P.stableMemoryRegion()) == 0;
    loop {
      let r = new();
      assert P.regionId(r) == n;
//      P.debugPrint(debug_show {n; id = P.regionId(r)});
      assert size(r) == 0;
      n += 1;
    } while (n < 32767);
    P.debugPrint(debug_show {alloced = n});
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

