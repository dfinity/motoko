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
    ensure(base + size + 4);
    StableMemory.storeBlob(base, blob);
    base += size;
    StableMemory.storeNat32(base, size);
    base += 4;
  };

  public query func readLast(count : Nat) : async ?[Text] {
    do ? {
      let a = Prim.Array_init<Text>(count, "");
      var offset = base;
      for (k in a.keys()) {
        if (offset == 0) return ?Prim.Array_tabulate<Text>(k, func i { a[i] });
        offset -= 4;
        let size = StableMemory.loadNat32(offset);
        offset -= size;
        let blob = StableMemory.loadBlob(offset, Prim.nat32ToNat(size));
        a[k] := Prim.decodeUtf8(blob)!;
      };
      return ?Prim.Array_tabulate<Text>(count, func i { a[i] });
    };
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
    switch (await readLast(count)) {
      case (?ts) {
        for (t in ts.vals()) {
          Prim.debugPrint(t);
        }
      };
      case null {}
    };
  };

  public func readMore() : async () {
    switch (await readLast(2*count)) {
      case (?ts) {
        for (t in ts.vals()) {
          Prim.debugPrint(t);
        }
      };
      case null {}
    };
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
//CALL ingress readMore "DIDL\x00\x00"
