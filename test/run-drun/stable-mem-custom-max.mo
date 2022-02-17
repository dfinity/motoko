//MOC-FLAG --max-stable-pages 16384
import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";

actor {
  let max : Nat64 = 16384;
  public func testGrow() : async () {
    assert 0  == StableMemory.grow(max - 1);
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
//CALL upgrade ""


