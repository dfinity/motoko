import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";
actor {

  stable var n : Nat32 = 0;
  assert (n == StableMemory.size());

  var i : Nat32 = 0;
  let max = n * 65536;
  while (i < max) {
    assert (StableMemory.loadNat32(i) == 0);
    StableMemory.storeNat32(i, i);
    assert (StableMemory.loadNat32(i) == i);
    StableMemory.storeNat32(i, 0);
    i += 4
  };

  system func preupgrade() {
    P.debugPrint("upgrading..." # debug_show n);
    let m = StableMemory.grow(1);

    assert (n == m);

    // check new page is clear
    var i : Nat32 = m * 65536;
    let max = i + 65536;
    while (i < max) {
      assert (StableMemory.loadNat32(i) == 0);
      i += 4
    };

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

//CALL upgrade ""
//CALL upgrade ""
//CALL upgrade ""
//CALL upgrade ""

