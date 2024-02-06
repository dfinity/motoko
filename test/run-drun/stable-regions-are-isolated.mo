//MOC-FLAG --stable-regions
// NB: this test runs 5x slower with 24 bit compact Nats, since since only 23 bit Nat are unboxed
// so half of the array tabulate entries below require bigint arithmetic (and boxing).

import P "mo:⛔";
import Region "stable-region/Region";
import Region0 "stable-mem/StableMemory";

actor {

  var r0 = Region.new();
  var r1 = Region.new();
  var r2 = Region.new();

  let block_size_in_pages = 128 : Nat64;

  P.debugPrint "grow three big regions (including region0).";

  // Interleave growing regions by a block each:
  do {
    ignore Region.grow(r0, block_size_in_pages);
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);

    ignore Region.grow(r0, block_size_in_pages);
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);

    ignore Region.grow(r0, block_size_in_pages);
    ignore Region.grow(r1, block_size_in_pages);
    ignore Region.grow(r2, block_size_in_pages);
  };

  P.debugPrint "grow three big regions: done.";

  func blobOfNat64(n : Nat64) : Blob {
    let size = P.nat64ToNat(n);
    var v : Nat8 = 0;
    let a = P.Array_tabulate<Nat8>(size, func _ { v +%= 1; v }); //<- expensive when i boxed
    let b = P.arrayToBlob(a);
    b
  };

  // A blob that is the size of two region blocks.
  let big_len = block_size_in_pages * 2 * 65536;
  let big_blob = blobOfNat64(big_len);

  P.debugPrint "storing a big blob in each region.";

  Region.storeBlob(r0, 1, big_blob);
  Region.storeBlob(r1, 0, big_blob);
  Region.storeBlob(r2, 137, big_blob);

  P.debugPrint "loading the big blob back from each region.";

  assert(Region.loadBlob(r0, 1, P.nat64ToNat(big_len)) == big_blob);
  assert(Region.loadBlob(r1, 0, P.nat64ToNat(big_len)) == big_blob);
  assert(Region.loadBlob(r2, 137, P.nat64ToNat(big_len)) == big_blob);

  P.debugPrint "success. done.";

  P.trap "rolling back to avoid state commit";
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref
//xCALL ingress test "DIDL\x00\x00"
