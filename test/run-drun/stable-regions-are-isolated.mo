import P "mo:⛔";
import Region "stable-region/Region";
actor {
  var r1 = Region.new();
  var r2 = Region.new();

  let block_size_in_pages = 128 : Nat64;

  // Interleave growing regions by a block each:
  do {
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);
  };

  func blobOfNat64(n : Nat64) : Blob {
    let size = P.nat64ToNat(n);
    let a = P.Array_tabulate<Nat8>(size, func i { P.natToNat8(i % 256) });
    P.arrayToBlob(a);
  };

  // A blob that is the size of two region blocks.
  let big_len = block_size_in_pages * 2 * 65536;
  let big_blob = blobOfNat64(big_len);

  Region.storeBlob(r1, 0, big_blob);
  Region.storeBlob(r2, 137, big_blob);

  assert(Region.loadBlob(r1, 0, P.nat64ToNat(big_len)) == big_blob);
  assert(Region.loadBlob(r2, 137, P.nat64ToNat(big_len)) == big_blob);

  P.debugPrint "done."
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
//xCALL ingress test "DIDL\x00\x00"
