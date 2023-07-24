//MOC-FLAG --stable-regions
import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";

actor {
  let max : Nat64 = 65536;
  public func testGrow() : async () {
  
    let o = StableMemory.grow(max - 1); P.debugPrint(debug_show(o)); assert (o == 0);


    assert (max - 1) == StableMemory.grow(1);
    assert max == StableMemory.size();
    assert max == StableMemory.grow(0);
    assert 0xFFFF_FFFF_FFFF_FFFF == StableMemory.grow(1);
    assert 0xFFFF_FFFF_FFFF_FFFF == StableMemory.grow(1024);
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress testGrow "DIDL\x00\x00"


