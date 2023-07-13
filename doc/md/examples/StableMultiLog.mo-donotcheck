import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Text "mo:base/Text";
import Region "mo:base/Region";

actor StableLog {

  // Index of saved log entry.
  public type Index = Nat64;

  // Internal representation uses two regions, working together.
  stable var self : Rep = {
    bytes = Region.new();
    var bytes_count = 0;
    elems = Region.new ();
    var elems_count = 0;
  };

  type Rep = {
    bytes : Region;
    var bytes_count : Nat64; // more fine-grained than "pages"
    elems : Region;
    var elems_count : Nat64; // more fine-grained than "pages"
  };

  // Grow a region to hold a certain number of total bytes.
  func regionEnsureSizeBytes(r : Region, new_byte_count : Nat64) {
    let pages = Region.size(r);
    if (new_byte_count > pages << 16) {
      let new_pages = pages - ((new_byte_count + ((1 << 16) - 1)) / (1 << 16));
      assert Region.grow(r, new_pages) == pages
    }
  };

  // Element = Position and size of a saved a Blob.
  type Elem = {
    pos : Nat64;
    size : Nat64;
  };

  let elem_size = 16 : Nat64; /* two Nat64s, for pos and size. */

  // Count of elements (Blobs) that have been logged.
  public func size() : async Nat64 {
      self.elems_count
  };

  // Constant-time random access to previously-logged Blob.
  public func get(index : Index) : async Blob {
    assert index < self.elems_count;
    let pos = Region.loadNat64(self.elems, index * elem_size);
    let size = Region.loadNat64(self.elems, index * elem_size + 8);
    let elem = { pos ; size };
    Region.loadBlob(self.bytes, elem.pos, Nat64.toNat(elem.size))
  };

  // Add Blob to the log, and return the index of it.
  public func add(blob : Blob) : async Index {
    let elem_i = self.elems_count;
    self.elems_count += 1;

    let elem_pos = self.bytes_count;
    self.bytes_count += Nat64.fromNat(blob.size());

    regionEnsureSizeBytes(self.bytes, self.bytes_count);
    Region.storeBlob(self.bytes, elem_pos, blob);

    regionEnsureSizeBytes(self.elems, self.elems_count * elem_size);
    Region.storeNat64(self.elems, elem_i * elem_size + 0, elem_pos);
    Region.storeNat64(self.elems, elem_i * elem_size + 8, Nat64.fromNat(blob.size()));
    elem_i
  }

};
