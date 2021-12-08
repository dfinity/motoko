/// Binary blobs

import Prim "mo:⛔";
module {

  /// An immutable, possibly empty sequence of bytes.
  /// Given `b : Blob`:
  ///
  /// * `b.size() : Nat` returns the number of bytes in the blob;
  /// * `b.vals() : Iter.Iter<Nat8>` returns an iterator to enumerate the bytes of the blob.
  ///
  /// (Direct indexing of Blobs is not yet supported.)
  public type Blob = Prim.Types.Blob;

  /// Returns a (non-cryptographic) hash of 'b'
  public let hash : (b : Blob) -> Nat32 = Prim.hashBlob;

  /// Returns `x == y`.
  public func equal(x : Blob, y : Blob) : Bool { x == y };

  /// Returns `x != y`.
  public func notEqual(x : Blob, y : Blob) : Bool { x != y };

  /// Returns `x < y`.
  public func less(x : Blob, y : Blob) : Bool { x < y };

  /// Returns `x <= y`.
  public func lessOrEqual(x : Blob, y : Blob) : Bool { x <= y };

  /// Returns `x > y`.
  public func greater(x : Blob, y : Blob) : Bool { x > y };

  /// Returns `x >= y`.
  public func greaterOrEqual(x : Blob, y : Blob) : Bool { x >= y };

  /// Returns the order of `x` and `y`.
  public func compare(x : Blob, y : Blob) : { #less; #equal; #greater } {
    if (x < y) { #less }
    else if (x == y) { #equal }
    else { #greater }
  };

  /// Creates a blob from an array of bytes, by copying each element.
  public let fromArray : [Nat8] -> Blob = Prim.arrayToBlob;

  /// Creates a blob from a mutable array of bytes, by copying each element.
  public let fromArrayMut : [var Nat8] -> Blob = Prim.arrayMutToBlob;

  /// Creates an array of bytes from a blob, by copying each element.
  public let toArray : Blob -> [Nat8] = Prim.blobToArray;

  /// Creates a mutable array of bytes from a blob, by copying each element.
  public let toArrayMut : Blob -> [var Nat8] = Prim.blobToArrayMut;

}
