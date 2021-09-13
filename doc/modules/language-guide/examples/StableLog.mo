import Nat32 "mo:base/Nat32";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
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
    let next = base + 4 + size;
    ensure(next);
    StableMemory.storeNat32(base, size);
    StableMemory.storeBlob(base + 4, blob);
    base := next;
  };

  public query func read(offset : Nat32) :
    async ?(Text, nextOffset : Nat32)
  {
    if (offset >= base) return null;
    let size = StableMemory.loadNat32(offset);
    let blob = StableMemory.loadBlob(offset + 4, Nat32.toNat(size));
    do ? {
      (Text.decodeUtf8(blob)!, offset + 4 + size)
    }
  };

};

