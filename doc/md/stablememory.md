# The `ExperimentalStableMemory` library

:::danger

The `ExperimentalStableMemory` library is experimental, subject to change and may be replaced by safer alternatives in later versions of Motoko. Use at your own risk and discretion.

:::

Motoko stable variables, while convenient to use, require serialization and deserialization of all stable variables on upgrade (see [Stable variables and upgrade methods](upgrades.md)). During an upgrade, the current values of stable variables are first saved to IC stable memory, then restored from stable memory after the new code is installed. Unfortunately, this mechanism does not scale to canisters that maintain *large* amounts of data in stable variables: there may not be enough cycle budget to store then restore all stable variables within an upgrade, resulting in failed upgrades.

To avoid this upgrade hazard, actors can elect to use a lower-level `ExperimentalStableMemory` library. The library allows the programmer to incrementally allocate pages of (64-bit) IC stable memory and use those pages to incrementally read and write data in a user-defined binary format.

The Motoko runtime system ensures there is no interference between the abstraction presented by the `ExperimentalStableMemory` library and an actor’s stable variables, even though the two abstractions ultimately use the same underlying (concrete) stable memory facilities available to all IC canisters. This runtime support means that is safe for a Motoko program to exploit both stable variables and `ExperimentalStableMemory`, within the same application.

## The Library

Support for stable memory is provided by the [ExperimentalStableMemory](../../../../references/motoko-ref/ExperimentalStableMemory.md) library in package `base`.

The interface to the `ExperimentalStableMemory` library consists of functions for querying and growing the currently allocated set of stable memory pages, plus matching pairs of `load`, `store` operations for most of Motoko’s fixed-size scalar types.

More general `loadBlob` and `storeBlob` operations are also available for reading/writing binary blobs and other types that can be encoded as `Blob`s (e.g. `Text` values) of arbitrary sizes, using Motoko supplied or user-provided encoders and decoders.

``` motoko no-repl
module {

  // Current size of the stable memory, in pages.
  // Each page is 64KiB (65536 bytes).
  // Initially `0`.
  size : () -> (pages : Nat64);

  // Grow current `size` of stable memory by `pagecount` pages.
  // Each page is 64KiB (65536 bytes).
  // Returns previous `size` when able to grow.
  // Returns `0xFFFF_FFFF_FFFF_FFFF` if remaining pages insufficient.
  grow : (new_pages : Nat64) -> (oldpages : Nat64);

  loadNat8 : (offset : Nat64) -> Nat8;
  storeNat8 : (offset : Nat64, value: Nat8) -> ();

  // ... and similar for Nat16, Nat32, Nat64,
  // Int8, Int16, Int32 and Int64 ...

  loadFloat : (offset : Nat64) -> Float;
  storeFloat : (offset : Nat64, value : Float) -> ();

  // Load `size` bytes starting from `offset` as a `Blob`.
  // Traps on out-of-bounds access.
  loadBlob : (offset : Nat64, size : Nat) -> Blob;

  // Write bytes of `blob` beginning at `offset`.
  // Traps on out-of-bounds access.
  storeBlob : (offset : Nat64, value : Blob) -> ()

  // Returns a query that, when called, returns the number of bytes of
  // (real) IC stable memory that would be occupied by persisting its
  // current stable variables before an upgrade.
  stableVarQuery : () -> (shared query () -> async {size : Nat64})
}
```

## Example

To demonstrate the `ExperimentalStableMemory` library, we present a dead simple implementation of a logging actor that records text messages in a scalable, persistent log.

The example illustrates the simultaneous use of stable variables and stable memory. It uses a single stable variable to keep track of the next available offset, but stores the contents of the log directly in stable memory.

``` motoko no-repl
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Text "mo:base/Text";
import Array "mo:base/Array";
import StableMemory "mo:base/ExperimentalStableMemory";

actor StableLog {

  func ensure(offset : Nat64) {
    let pages = (offset + 65536) >> 16;
    if (pages > StableMemory.size()) {
      let oldsize = StableMemory.grow(pages - StableMemory.size());
      assert (oldsize != 0xFFFF_FFFF_FFFF_FFFF);
    };
  };

  stable var base : Nat64 = 0;

  public func log(t : Text) {
    let blob = Text.encodeUtf8(t);
    let size = Nat64.fromNat(blob.size());
    ensure(base + size + 4);
    StableMemory.storeBlob(base, blob);
    base += size;
    StableMemory.storeNat32(base, Nat32.fromNat(blob.size()));
    base += 4;
  };

  public query func readLast(count : Nat) : async [Text] {
    let a = Array.init<Text>(count, "");
    var offset = base;
    var k = 0;
    while (k < count and offset > 0) {
      offset -= 4;
      let size = StableMemory.loadNat32(offset);
      offset -= Nat64.fromNat(Nat32.toNat(size));
      let blob = StableMemory.loadBlob(offset, Nat32.toNat(size));
      switch (Text.decodeUtf8(blob)) {
        case (?t) { a[k] := t };
        case null { assert false };
      };
      k += 1;
    };
    return Array.tabulate<Text>(k, func i { a[i] });
  };

};
```

The auxiliary function `ensure(offset)` is used to grow `ExerimentalStableMemory` as necessary to accommodate more data. It computes the 64KiB page of a given offset and ensures enough pages have been allocated to guarantee that offset is within bounds.

The shared `log(t)` function encodes its `Text` argument as a `Blob`, allocates enough stable memory to store it, and writes both the blob contents and its size at the next available offset in `ExperimentalStableMemory`, updating `base`.

The shared `readLast(count)` query reads up to `count` messages from the log, traversing the log in reverse from `base`.

Because `StableLog` allocates and maintains its (potentially large) log data directly in stable memory and uses just a small and fixed amount of storage for actual stable variables (here `base`), upgrading `StableLog` to a new implementation (perhaps to provide more functionality) should not consume too many cycles, regardless of the current size of the log.
