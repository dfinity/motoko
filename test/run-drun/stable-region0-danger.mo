//MOC-FLAG --stable-regions

import P "mo:⛔";
import Region "stable-region/Region";
import Region0 "stable-mem/StableMemory";

// test shows why we can't expose region0 without further steps to ensure
// correct aliasing of all region0 objects, pre and post upgrade
actor {

  stable let r0 = Region0.region();
  ignore Region.grow(r0, 1);

  P.debugPrint "grow three big regions: done.";

  system func preupgrade() {
  };

  public func sanityTest() {
    let r01 = Region0.region();
    P.debugPrint(debug_show {s0 = Region.size(r0);  s01 = Region.size(r01)});
    assert Region.size(r0) == Region.size(r01);
    ignore Region.grow(r0, 1);
    P.debugPrint(debug_show {s0 = Region.size(r0);  s01 = Region.size(r01)});
    assert Region.size(r0) == Region.size(r01);
  };
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
//CALL ingress sanityTest "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress sanityTest "DIDL\x00\x00"
