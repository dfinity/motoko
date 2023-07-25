//MOC-FLAG --stable-regions --max-stable-pages 16777216
import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";

// tests that allocation failure is reported when the replica fails to allocate
// (even when --max-stable-pages not exceeded)
actor {
  assert (167777216 > 128*65536);
  let m = StableMemory.grow(128*65536); //128 GB, should fail!
  if (m != 0xFFFF_FFFF_FFFF_FFFF) {
    // force IC stable memory out of bounds error
    P.debugPrint(debug_show m);
    ignore StableMemory.loadNat8(128*65536-1);
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
