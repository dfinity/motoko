import P "mo:⛔";
import Region "stable-region/Region";
actor {
  stable var n = 0;

  public func testBounds() {
    P.debugPrint("testBounds");
  };

  system func preupgrade() {
    n += 1;
  };

  system func postupgrade() {
    P.debugPrint("...upgraded" # debug_show n);
  };

}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
// TO DO -- pass run on run-drun
//xSKIP drun-run

//xCALL upgrade ""
//xCALL ingress testBounds "DIDL\x00\x00"
//xCALL upgrade ""
//xCALL ingress testBounds "DIDL\x00\x00"
//xCALL upgrade ""
//xCALL ingress testBounds "DIDL\x00\x00"
//xCALL upgrade ""
