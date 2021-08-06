import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";
actor {

  stable var n : Nat32 = 0;
  assert (n == StableMemory.size());

  var i : Nat32 = 0;
  let max = n * 65536;
  while (i < max) {
    assert (StableMemory.loadNat32(i) == i);
    StableMemory.storeNat32(i, ^i);
    assert (StableMemory.loadNat32(i) == ^i);
    StableMemory.storeNat32(i, i);
    i += 4
  };

  system func preupgrade() {
    P.debugPrint("upgrading..." # debug_show n);
    let m = StableMemory.grow(1);

    assert (n == m);

    n += 1;
    P.debugPrint(debug_show {old = m; new = n; size = StableMemory.size()});

    assert (n == StableMemory.size());

    // check new page is clear
    var i : Nat32 = m * 65536;
    let max = i + 65536;
    while (i < max) {
      assert (StableMemory.loadNat32(i) == 0);
      StableMemory.storeNat32(i, i);
      i += 4
    };

  };

  func log(e : Error) {
    P.debugPrint(debug_show({code = P.errorCode(e); message = P.errorMessage(e)}));
  };

  public func testBounds() : async () {

    if (n == 0) return;
    assert (n == StableMemory.size());
    P.debugPrint (debug_show {testBounds=n});
    // test bounds check
    var i : Nat32 = n * 65536 - 3;
    let max = i + 16;
    while (i < max) {
      try {
        await async {
          ignore StableMemory.loadNat32(i);
        };
        P.debugPrint (debug_show {oops=i});
      }
      catch e { log(e) };
      try {
        await async StableMemory.storeNat32(i, i);
        assert false;
      }
      catch e { log(e) };
      i += 4
    };
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
//CALL ingress testBounds "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress testBounds "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress testBounds "DIDL\x00\x00"
//CALL upgrade ""

