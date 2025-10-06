/// Byte-level access to isolated, virtual stable memory regions.
///
/// This is a moderately lightweight abstraction over IC _stable memory_ and supports persisting
/// regions of binary data across Motoko upgrades.
/// Use of this module is fully compatible with Motoko's use of
/// _stable variables_, whose persistence mechanism also uses (real) IC stable memory internally, but does not interfere with this API.
/// It is also fully compatible with existing uses of the `ExperimentalStableMemory` library, which has a similar interface, but,
/// only supported a single memory region, without isolation between different applications.
///
/// The `Region` type is stable and can be used in stable data structures.
///
/// A new, empty `Region` is allocated using function `new()`.
///
/// Regions are stateful objects and can be distinguished by the numeric identifier returned by function `id(region)`.
/// Every region owns an initially empty, but growable sequence of virtual IC stable memory pages.
/// The current size, in pages, of a region is returned by function `size(region)`.
/// The size of a region determines the range, [ 0, ..., size(region)*2^16 ), of valid byte-offsets into the region; these offsets are used as the source and destination of `load`/`store` operations on the region.
///
/// Memory is allocated to a region, using function `grow(region, pages)`, sequentially and on demand, in units of 64KiB logical pages, starting with 0 allocated pages.
/// A call to `grow` may succeed, returning the previous size of the region, or fail, returning a sentinel value. New pages are zero initialized.
///
/// A size of a region can only grow and never shrink.
/// In addition, the stable memory pages allocated to a region will *not* be reclaimed by garbage collection, even
/// if the region object itself becomes unreachable.
///
/// Growth is capped by a soft limit on physical page count controlled by compile-time flag
/// `--max-stable-pages <n>` (the default is 65536, or 4GiB).
///
/// Each `load` operation loads from region relative byte address `offset` in little-endian
/// format using the natural bit-width of the type in question.
/// The operation traps if attempting to read beyond the current region size.
///
/// Each `store` operation stores to region relative byte address `offset` in little-endian format using the natural bit-width of the type in question.
/// The operation traps if attempting to write beyond the current region size.
///
/// Text values can be handled by using `Text.decodeUtf8` and `Text.encodeUtf8`, in conjunction with `loadBlob` and `storeBlob`.
///
/// The current region allocation and region contents are preserved across upgrades.
///
/// NB: The IC's actual stable memory size (`ic0.stable_size`) may exceed the
/// total page size reported by summing all regions sizes.
/// This (and the cap on growth) are to accommodate Motoko's stable variables and bookkeeping for regions.
/// Applications that plan to use Motoko stable variables sparingly or not at all can
/// increase `--max-stable-pages` as desired, approaching the IC maximum (initially 8GiB, then 32Gib, currently 64Gib).
/// All applications should reserve at least one page for stable variable data, even when no stable variables are used.
///
/// Usage:
/// ```motoko no-repl name=import
/// import Region "mo:core/Region";
/// ```

import Prim "mo:⛔";

module {

  /// A stateful handle to an isolated region of IC stable memory.
  /// `Region` is a stable type and regions can be stored in stable variables.
  public type Region = Prim.Types.Region;

  /// Allocate a new, isolated Region of size 0.
  ///
  /// Example:
  ///
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     assert Region.size(region) == 0;
  ///   }
  /// }
  /// ```
  public let new : () -> Region = Prim.regionNew;

  /// Return a Nat identifying the given region.
  /// May be used for equality, comparison and hashing.
  /// NB: Regions returned by `new()` are numbered from 16
  /// (regions 0..15 are currently reserved for internal use).
  /// Allocate a new, isolated Region of size 0.
  ///
  /// Example:
  ///
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     assert Region.id(region) == 16;
  ///   }
  /// }
  /// ```
  public let id : Region -> Nat = Prim.regionId;

  /// Current size of `region`, in pages.
  /// Each page is 64KiB (65536 bytes).
  /// Initially `0`.
  /// Preserved across upgrades, together with contents of allocated
  /// stable memory.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let beforeSize = Region.size(region);
  ///     ignore Region.grow(region, 10);
  ///     let afterSize = Region.size(region);
  ///     assert afterSize - beforeSize == 10;
  ///   }
  /// }
  /// ```
  public let size : (region : Region) -> (pages : Nat64) = Prim.regionSize;

  /// Grow current `size` of `region` by the given number of pages.
  /// Each page is 64KiB (65536 bytes).
  /// Returns the previous `size` when able to grow.
  /// Returns `0xFFFF_FFFF_FFFF_FFFF` if remaining pages insufficient.
  /// Every new page is zero-initialized, containing byte 0x00 at every offset.
  /// Function `grow` is capped by a soft limit on `size` controlled by compile-time flag
  ///  `--max-stable-pages <n>` (the default is 65536, or 4GiB).
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// import Error "mo:core/Error";
  ///
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let beforeSize = Region.grow(region, 10);
  ///     if (beforeSize == 0xFFFF_FFFF_FFFF_FFFF) {
  ///       throw Error.reject("Out of memory");
  ///     };
  ///     let afterSize = Region.size(region);
  ///     assert afterSize - beforeSize == 10;
  ///   }
  /// }
  /// ```
  public let grow : (region : Region, newPages : Nat64) -> (oldPages : Nat64) = Prim.regionGrow;

  /// Within `region`, load a `Nat8` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat8 = 123;
  ///     Region.storeNat8(region, offset, value);
  ///     assert Region.loadNat8(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadNat8 : (region : Region, offset : Nat64) -> Nat8 = Prim.regionLoadNat8;

  /// Within `region`, store a `Nat8` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat8 = 123;
  ///     Region.storeNat8(region, offset, value);
  ///     assert Region.loadNat8(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeNat8 : (region : Region, offset : Nat64, value : Nat8) -> () = Prim.regionStoreNat8;

  /// Within `region`, load a `Nat16` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat16 = 123;
  ///     Region.storeNat16(region, offset, value);
  ///     assert Region.loadNat16(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadNat16 : (region : Region, offset : Nat64) -> Nat16 = Prim.regionLoadNat16;

  /// Within `region`, store a `Nat16` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat16 = 123;
  ///     Region.storeNat16(region, offset, value);
  ///     assert Region.loadNat16(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeNat16 : (region : Region, offset : Nat64, value : Nat16) -> () = Prim.regionStoreNat16;

  /// Within `region`, load a `Nat32` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat32 = 123;
  ///     Region.storeNat32(region, offset, value);
  ///     assert Region.loadNat32(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadNat32 : (region : Region, offset : Nat64) -> Nat32 = Prim.regionLoadNat32;

  /// Within `region`, store a `Nat32` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat32 = 123;
  ///     Region.storeNat32(region, offset, value);
  ///     assert Region.loadNat32(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeNat32 : (region : Region, offset : Nat64, value : Nat32) -> () = Prim.regionStoreNat32;

  /// Within `region`, load a `Nat64` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat64 = 123;
  ///     Region.storeNat64(region, offset, value);
  ///     assert Region.loadNat64(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadNat64 : (region : Region, offset : Nat64) -> Nat64 = Prim.regionLoadNat64;

  /// Within `region`, store a `Nat64` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Nat64 = 123;
  ///     Region.storeNat64(region, offset, value);
  ///     assert Region.loadNat64(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeNat64 : (region : Region, offset : Nat64, value : Nat64) -> () = Prim.regionStoreNat64;

  /// Within `region`, load a `Int8` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int8 = 123;
  ///     Region.storeInt8(region, offset, value);
  ///     assert Region.loadInt8(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadInt8 : (region : Region, offset : Nat64) -> Int8 = Prim.regionLoadInt8;

  /// Within `region`, store a `Int8` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int8 = 123;
  ///     Region.storeInt8(region, offset, value);
  ///     assert Region.loadInt8(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeInt8 : (region : Region, offset : Nat64, value : Int8) -> () = Prim.regionStoreInt8;

  /// Within `region`, load a `Int16` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int16 = 123;
  ///     Region.storeInt16(region, offset, value);
  ///     assert Region.loadInt16(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadInt16 : (region : Region, offset : Nat64) -> Int16 = Prim.regionLoadInt16;

  /// Within `region`, store a `Int16` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int16 = 123;
  ///     Region.storeInt16(region, offset, value);
  ///     assert Region.loadInt16(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeInt16 : (region : Region, offset : Nat64, value : Int16) -> () = Prim.regionStoreInt16;

  /// Within `region`, load a `Int32` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int32 = 123;
  ///     Region.storeInt32(region, offset, value);
  ///     assert Region.loadInt32(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadInt32 : (region : Region, offset : Nat64) -> Int32 = Prim.regionLoadInt32;

  /// Within `region`, store a `Int32` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int32 = 123;
  ///     Region.storeInt32(region, offset, value);
  ///     assert Region.loadInt32(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeInt32 : (region : Region, offset : Nat64, value : Int32) -> () = Prim.regionStoreInt32;

  /// Within `region`, load a `Int64` value from `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int64 = 123;
  ///     Region.storeInt64(region, offset, value);
  ///     assert Region.loadInt64(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let loadInt64 : (region : Region, offset : Nat64) -> Int64 = Prim.regionLoadInt64;

  /// Within `region`, store a `Int64` value at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value : Int64 = 123;
  ///     Region.storeInt64(region, offset, value);
  ///     assert Region.loadInt64(region, offset) == 123;
  ///   }
  /// }
  /// ```
  public let storeInt64 : (region : Region, offset : Nat64, value : Int64) -> () = Prim.regionStoreInt64;

  /// Within `region`, loads a `Float` value from the given `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value = 1.25;
  ///     Region.storeFloat(region, offset, value);
  ///     assert Region.loadFloat(region, offset) == 1.25;
  ///   }
  /// }
  /// ```
  public let loadFloat : (region : Region, offset : Nat64) -> Float = Prim.regionLoadFloat;

  /// Within `region`, store float `value` at the given `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value = 1.25;
  ///     Region.storeFloat(region, offset, value);
  ///     assert Region.loadFloat(region, offset) == 1.25;
  ///   }
  /// }
  /// ```
  public let storeFloat : (region : Region, offset : Nat64, value : Float) -> () = Prim.regionStoreFloat;

  /// Within `region,` load `size` bytes starting from `offset` as a `Blob`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// import Blob "mo:core/Blob";
  ///
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value = Blob.fromArray([1, 2, 3]);
  ///     let size = value.size();
  ///     Region.storeBlob(region, offset, value);
  ///     assert Blob.toArray(Region.loadBlob(region, offset, size)) == [1, 2, 3];
  ///   }
  /// }
  /// ```
  public let loadBlob : (region : Region, offset : Nat64, size : Nat) -> Blob = Prim.regionLoadBlob;

  /// Within `region, write `blob.size()` bytes of `blob` beginning at `offset`.
  /// Traps on an out-of-bounds access.
  ///
  /// Example:
  /// ```motoko no-repl include=import
  /// import Blob "mo:core/Blob";
  ///
  /// persistent actor {
  ///   public func example() : async () {
  ///     let region = Region.new();
  ///     let offset : Nat64 = 0;
  ///     let value = Blob.fromArray([1, 2, 3]);
  ///     let size = value.size();
  ///     Region.storeBlob(region, offset, value);
  ///     assert Blob.toArray(Region.loadBlob(region, offset, size)) == [1, 2, 3];
  ///   }
  /// }
  /// ```
  public let storeBlob : (region : Region, offset : Nat64, value : Blob) -> () = Prim.regionStoreBlob;

}
