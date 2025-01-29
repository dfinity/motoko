---
sidebar_position: 1
---

# Stable regions



The `Region` library provides low-level access to ICP stable memory feature.

Stable regions were originally introduced with [classical orthogonal persistence](../canister-maintenance/orthogonal-persistence/classical.md) to allow larger scaled data to be retained across upgrades. For this purpose, programmers could explicitly store persistent data in stable memory, with regions helping to isolate different instances using stable memory.

This is superseded by [enhanced orthogonal persistence](../canister-maintenance/orthogonal-persistence/enhanced.md). Nevertheless, regions are still offered for backwards-compatibility and for specific use cases where developers prefer to manage data explicitly in a persistent linear memory.

## The `Region` library

The [`Region`](../base/Region.md) library in package `base` allows the programmer to incrementally allocate pages of 64-bit stable memory and use those pages to incrementally read and write data in a user-defined binary format.

Several pages may be allocated at once, with each page containing 64KiB. Allocation may fail due to resource limits imposed by ICP. Pages are zero-initialized.

While the user allocates at the granularity of 64KiB pages, the implementation will allocate at the coarser granularity of a block, currently 128 of physical stable memory pages.

The Motoko runtime system ensures there is no interference between the abstraction presented by the `Region` library and an actor’s stable variables, even though the two abstractions ultimately use the same underlying stable memory facilities available to all ICP canisters. This runtime support means that is safe for a Motoko program to exploit both stable variables and `Region`, within the same application.

Further, distinct `Region`s use distinct pages of stable memory, ensuring that two distinct `Region`s can not interfere with each other's data representations during normal operation, or during an upgrade.

## Using `Regions`

The interface to the `Region` library consists of functions for querying and growing the currently allocated set of stable memory pages, plus matching pairs of `load`, `store` operations for most of Motoko’s fixed-size scalar types.

More general `loadBlob` and `storeBlob` operations are also available for reading and writing binary blobs and other types that can be encoded as [`Blob`](../base/Blob.md)s of arbitrary sizes, using Motoko supplied or user-provided encoders and decoders.

``` motoko no-repl
module {

  // A stateful handle to an isolated region of IC stable memory.
  //  `Region` is a stable type and regions can be stored in stable variables.
  type Region = Prim.Types.Region;

  // Allocate a new, isolated `Region` of size 0.
  new : () -> Region;

  // Current size of the region `r` in pages.
  // Each page is 64KiB (65536 bytes).
  // Initially `0`.
  size : (r : Region) -> (pages : Nat64);

  // Grow current `size` of region `r` by `pagecount` pages.
  // Each page is 64KiB (65536 bytes).
  // Returns previous `size` when able to grow.
  // Returns `0xFFFF_FFFF_FFFF_FFFF` if remaining pages of physical stable memory insufficient.
  // Please note that there is no way to shrink the size of a region.
  grow : (r : Region, new_pages : Nat64) -> (oldpages : Nat64);

  // read ("load") a byte from a region, by offset.
  loadNat8 : (r : Region, offset : Nat64) -> Nat8;

  // write ("store") a byte into a region, by offset.
  storeNat8 : (r : Region, offset : Nat64, value: Nat8) -> ();

  // ... and similar for Nat16, Nat32, Nat64,
  // Int8, Int16, Int32 and Int64 ...

  loadFloat : (r : Region, offset : Nat64) -> Float;
  storeFloat : (r : Region, offset : Nat64, value : Float) -> ();

  // Load `size` bytes starting from `offset` in region `r` as a [`Blob`](../base/Blob.md).
  // Traps on out-of-bounds access.
  loadBlob : (r : Region, offset : Nat64, size : Nat) -> Blob;

  // Write all bytes of [`Blob`](../base/Blob.md) to region `r` beginning at `offset`.
  // Traps on out-of-bounds access.
  storeBlob : (r : Region, offset : Nat64, value : Blob) -> ()

}
```

:::danger
A stable region exposes low-level linear memory and it is the programmer's task to properly manipulate and interpret this data.
This can be very error-prone when managing data in a stable region.
However, the safety of Motoko's native values heap objects is always guaranteed, independent of the stable region content.
:::

:::note
The cost of accessing stable regions is significantly higher than using Motoko's native memory, i.e. regular Motoko values and objects.
:::

## Example

To demonstrate the `Region` library, the following is a simple implementation of a logging actor that records text messages in a scalable, persistent log.

The example illustrates the simultaneous use of stable variables and stable memory. It uses a single stable variable, `state`, to keep track of the two regions and their size in bytes, but stores the contents of the log directly in stable memory.

``` motoko no-repl file=../examples/StableMultiLog.mo
```

The shared `add(blob)` function allocates enough stable memory to store the given blob and writes the blob contents, its size, and its position into the pre-allocated regions.  One region is dedicated to storing the blobs of varying sizes, and the other is dedicated to storing their fixed-sized metadata.

The shared `get(index)` query reads anywhere from the log without traversing any unrelated memory.

`StableLog` allocates and maintains its potentially large log data directly in stable memory and uses a small and fixed amount of storage for actual stable variables. Upgrading `StableLog` to a new implementation should not consume many cycles, regardless of the current size of the log.

## Mops packages for stable regions

- [`memory-region`](https://mops.one/memory-region): A library for abstraction over the `Region` type that supports reusing deallocated memory.

- [`stable-enum`](https://mops.one/stable-enum): Enumerations implemented in stable regions.

- [`stable-buffer`](https://mops.one/stable-buffer): Buffers implemented in stable regions.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />