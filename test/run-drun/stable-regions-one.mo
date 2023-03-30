import P "mo:⛔";
import Region "stable-region/Region";
import Region0 "stable-mem/StableMemory";

actor {
  stable var r1 = Region.new();
  stable var id : Nat32 = 0xFFFF;
  stable var size : Nat32 = 0xFFFF;
  ignore Region.grow(r1, 8);

  system func preupgrade() {
    P.debugPrint("pre");
    id := Region.id(r1);
    size := Region.id(r1);
    P.debugPrint(debug_show (#pre{id;size}));
  };

  system func postupgrade() {
    assert Region.size(r1) == size;
    assert Region.id(r1) == id;
    assert Region.id(Region.new()) != id;
    P.debugPrint(debug_show (#post{id;size}));
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
