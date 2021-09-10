import Prim "mo:⛔";
import StableMemory "stable-mem/StableMemory";

actor {

  func ensure(offset : Nat32) {
    let pages = (offset + 65536) >> 16;
      if (pages > StableMemory.size()) {
        let oldsize = StableMemory.grow(pages - StableMemory.size());
        assert (oldsize != 0xFFFF);
     };
  };

  stable var base : Nat32 = 0;

  public func log(t : Text) {
    let blob = Prim.encodeUtf8(t);
    let size = Prim.natToNat32(blob.size());
    let next = base + 4 + size;
    ensure(next);
    StableMemory.storeNat32(base, size);
    StableMemory.storeBlob(base + 4, blob);
    base := next;
  };

  public query func read(offset : Nat32) : async ?(Text, nextOffset : Nat32) {
    if (offset >= base) return null;
    let size = StableMemory.loadNat32(offset);
    let blob = StableMemory.loadBlob(offset + 4, Prim.nat32ToNat(size));
    let ?text = Prim.decodeUtf8(blob);
    ?(text, offset + 4 + size)
  };


  // testing
  stable var count = 0;

  public func populate() : async () {
    // populate
    let limit = count + 100;
    while (count < limit) {
      log(debug_show(count));
      count += 1;
    };
  };

  public func readAll() : async () {
    var cur : Nat32 = 0;
    loop {
      switch (await read(cur)) {
        case (?(text, next)) {
          Prim.debugPrint(text);
          cur := next;
        };
        case null {
          return;
        }
      }
    }
  }
}

//SKIP run
//SKIP run-low
//SKIP run-ir
// too slow on ic-ref-run:
//SKIP comp-ref

//CALL ingress populate "DIDL\x00\x00"
//CALL ingress readAll "DIDL\x00\x00"
//CALL upgrade ""
//CALL ingress populate "DIDL\x00\x00"
//CALL ingress readAll "DIDL\x00\x00"