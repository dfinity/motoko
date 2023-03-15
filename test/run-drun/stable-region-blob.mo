import P "mo:⛔";
import Region "stable-region/Region";
actor {

  P.debugPrint("Begin");

  Region.metaLogLines();

  let nextId = Region.nextId();
  assert nextId == 2;

  let r2 = Region.new();
  let r2_id = Region.id r2;

  P.debugPrint("Created region " # (debug_show r2_id));

  Region.metaLogLines();

  assert r2_id == 2;
  assert (Region.size r2) == 0;

  let (r3, r4) = (Region.new(), Region.new());

  P.debugPrint("Created regions " # (debug_show (Region.id r3)) # " and " # (debug_show (Region.id r4)));

  Region.metaLogLines();

  assert Region.grow(r2, 137 * 17) == 0;
  assert Region.grow(r3, 137) == 0;
  assert Region.grow(r4, 17) == 0;

  P.debugPrint("Grew all regions.");
  Region.metaLogLines();

  assert Region.grow(r2, 137 * 17) == 137 * 17;
  assert Region.grow(r3, 137) == 137;
  assert Region.grow(r4, 17) == 17;

  P.debugPrint("Grew all regions, again.");
  Region.metaLogLines();

  P.debugPrint("Storing data into region 2.");
  let addr : Nat64 = 137 << 16 + 137;
  Region.storeNat8(r2, addr, 137);

  P.debugPrint("Loading data from region 2.");
  let data = Region.loadNat8(r2, addr);
  P.debugPrint(debug_show data);
  assert data == 137;
  P.debugPrint("Done.");

/*
  assert (n == Region.size(r));

  func valOfNat64(n : Nat64) : Blob {
    let size = P.nat64ToNat(n);
    let a = P.Array_tabulate<Nat8>(size, func i { P.natToNat8(i % 256) });
    P.arrayToBlob(a);
  };

  func zeroOfNat64(n : Nat64) : Blob {
    let size = P.nat64ToNat(n);
    let a = P.Array_tabulate<Nat8>(size, func i { 0 });
    P.arrayToBlob(a);
  };

  let inc : Nat64 = 8;

  var i : Nat64 = 0;
  var size : Nat64 = 0;
  let max = n * 65536;
  while (i + size < max) {
    let v = valOfNat64(size);
    Region.storeBlob(r, i, v);
    assert (Region.loadBlob(r, i, P.nat64ToNat(size)) == v);
    i += size;
    size += 1;
  };
*/

  system func preupgrade() {
/*
    P.debugPrint("upgrading..." # debug_show n);
    let m = Region.grow(r, 1);

    assert (n == m);

    n += 1;

    P.debugPrint(debug_show {old = m; new = n; size = Region.size(r)});

    assert (n == Region.size(r));

    // check new page is clear
    var i : Nat64 = m * 65536;
    var size : Nat64 = 0;
    let max = i + 65536;
    while (i + size < max) {
      assert (Region.loadBlob(r, i, P.nat64ToNat(size)) == zeroOfNat64(size));
      Region.storeBlob(r, i, valOfNat64(size));
      i += size;
      size += 1;
    };
*/
  };

  public func testBounds() : async () {
/*
    if (n == 0) return;
    assert (n == Region.size(r));
    P.debugPrint (debug_show {testBounds=n});
    // test bounds check
    var i : Nat64 = n * 65536 - 7;
    let max = i + 16;
    while (i < max) {
      try {
        await async {
          ignore Region.loadBlob(r, i, 8);
        };
        assert false;
      }
      catch e {
        assert P.errorCode e == #canister_error;
      };
      try {
        await async Region.storeBlob(r, i, valOfNat64(8));
        assert false;
      }
      catch e {
        assert P.errorCode e == #canister_error;
      };
      i += 1;
    };
*/
  };

  system func postupgrade() {
/*
    P.debugPrint("...upgraded" # debug_show n);
*/
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
