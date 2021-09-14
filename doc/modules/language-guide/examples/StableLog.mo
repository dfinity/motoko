import Nat32 "mo:base/Nat32";
import Text "mo:base/Text";
import Array "mo:base/Array";
import StableMemory "mo:base/StableMemory";

actor StableLog {

  func ensure(offset : Nat32) {
    let pages = (offset + 65536) >> 16;
    if (pages > StableMemory.size()) {
      let oldsize = StableMemory.grow(pages - StableMemory.size());
      assert (oldsize != 0xFFFF);
    };
  };

  stable var base : Nat32 = 0;

  public func log(t : Text) {
    let blob = Text.encodeUtf8(t);
    let size = Nat32.fromNat(blob.size());
    ensure(base + size + 4);
    StableMemory.storeBlob(base, blob);
    base += size;
    StableMemory.storeNat32(base, size);
    base += 4;
  };

  public query func readLast(count : Nat) : async [Text] {
    let a = Array.init<Text>(count, "");
    var offset = base;
    for (k in a.keys()) {
      if (offset == 0) return Array.tabulate<Text>(k, func i { a[i] });
      offset -= 4;
      let size = StableMemory.loadNat32(offset);
      offset -= size;
      let blob = StableMemory.loadBlob(offset, Nat32.toNat(size));
      switch (Text.decodeUtf8(blob)) {
        case (?t) { a[k] := t };
        case null { assert false };
      }
    };
    return Array.tabulate<Text>(count, func i { a[i] });
  };

};

