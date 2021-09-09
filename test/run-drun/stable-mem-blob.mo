import P "mo:⛔";
import StableMemory "stable-mem/StableMemory";
actor {

  stable var n : Nat32 = 0;
  assert (n == StableMemory.size());

  func valOfNat32(n : Nat32) : Blob {
    let size = P.nat32ToNat(n);
    let a = P.Array_tabulate<Nat8>(size, func i { P.natToNat8(i % 256) });
    P.arrayToBlob(a);
  };

  func zeroOfNat32(n : Nat32) : Blob {
    let size = P.nat32ToNat(n);
    let a = P.Array_tabulate<Nat8>(size, func i { 0 });
    P.arrayToBlob(a);
  };

  let inc : Nat32 = 8;

  var i : Nat32 = 0;
  var size : Nat32 = 0;
  let max = n * 65536;
  while (i + size < max) {
    let v = valOfNat32(size);
    StableMemory.storeBlob(i, v);
    assert (StableMemory.loadBlob(i, P.nat32ToNat(size)) == v);
    i += size;
    size += 1;
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
    var size : Nat32 = 0;
    let max = i + 65536;
    while (i + size < max) {
      assert (StableMemory.loadBlob(i, P.nat32ToNat(size)) == zeroOfNat32(size));
      StableMemory.storeBlob(i, valOfNat32(size));
      i += size;
      size += 1;
    };

  };

  public func testBounds() : async () {
    if (n == 0) return;
    assert (n == StableMemory.size());
    P.debugPrint (debug_show {testBounds=n});
    // test bounds check
    var i : Nat32 = n * 65536 - 7;
    let max = i + 16;
    while (i < max) {
      try {
        await async {
          ignore StableMemory.loadBlob(i, 8);
        };
        assert false;
      }
      catch e {
        assert P.errorCode e == #canister_error;
      };
      try {
        await async StableMemory.storeBlob(i, valOfNat32(8));
        assert false;
      }
      catch e {
        assert P.errorCode e == #canister_error;
      };
      i += 1;
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

