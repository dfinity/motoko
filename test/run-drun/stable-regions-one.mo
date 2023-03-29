import P "mo:⛔";
import Region "stable-region/Region";
import Region0 "stable-mem/StableMemory";

actor {
  stable var r0 = Region0.region();
  stable var r1 = Region.new();

  system func preupgrade() {
    P.debugPrint("pre");
  };

  system func postupgrade() {
    P.debugPrint("post");
  };

  public func sanityTest() {
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
