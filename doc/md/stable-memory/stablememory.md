---
sidebar_position: 2
---


# Stable memory

## Overview

The `ExperimentalStableMemory` library provides low-level access to Internet Computer stable memory.

<!--
TODO: extend example to illustrate stableVarQuery
-->

:::danger

The `ExperimentalStableMemory` library is experimental, subject to change and may be replaced by safer alternatives in later versions of Motoko. Use at your own risk and discretion.

:::

The current implementation of Motoko stable variables is not able to maintain very large amounts of data.

The ICP upgrade process currently requires stable variables to be copied from 32-bit main memory  to 64-bit stable memory and then back again - for copious data, this can exceed the cycle limits allowed for upgrade, causing an upgrade to fail.

Moreover, a 32-bit Motoko canister and its stable variables can fundamentally store at most 4GB of data, while ICP stable memory is 64-bit and currently supports up to 400GB of data.

## The `ExperimentalStableMemory` library

To avoid the current limitations of stable variables, developers can use the recommended [`Region`](stable-regions.md) library or the older [`ExperimentalStableMemory`](.../base/ExperimentalStableMemory.md) library described here. The `ExperimentalStableMemory` library allows the programmer to incrementally allocate pages of 64-bit ICP stable memory and use those pages to incrementally read and write data in a user-defined binary format.

The main difference between the two libraries is that `ExperimentalStableMemory` provides a single memory, a global resource, that must be shared by all clients, using, requiring coordination and trust. 
The `Region` library instead provides multiple, isolated memories that can only be accessed by the owner(s) of a particular memory.

Similar to the `Regions` library, Motoko runtime system ensures there is no interference between the abstraction presented by the `ExperimentalStableMemory` library and an actor’s stable variables, even though the two abstractions ultimately use the same underlying  stable memory facilities available to all IC canisters. This runtime support means that is safe for a Motoko program to exploit both stable variables and `ExperimentalStableMemory`, within the same application.

## Using `ExperimentalStableMemory`

The interface to the `ExperimentalStableMemory` library consists of functions for querying and growing the currently allocated set of stable memory pages, plus matching pairs of `load`, `store` operations for most of Motoko’s fixed-size scalar types.

More general `loadBlob` and `storeBlob` operations are also available for reading and writing binary blobs and other types that can be encoded as `Blob`s of arbitrary sizes, using Motoko supplied or user-provided encoders and decoders.


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

``` motoko no-repl file=../examples/StableLog.mo
```

The auxiliary function `ensure(offset)` is used to grow `ExerimentalStableMemory` as necessary to accommodate more data. It computes the 64KiB page of a given offset and ensures enough pages have been allocated to guarantee that offset is within bounds.

The shared `log(t)` function encodes its `Text` argument as a `Blob`, allocates enough stable memory to store it, and writes both the blob contents and its size at the next available offset in `ExperimentalStableMemory`, updating `base`.

The shared `readLast(count)` query reads up to `count` messages from the log, traversing the log in reverse from `base`.

Because `StableLog` allocates and maintains its (potentially large) log data directly in stable memory and uses just a small and fixed amount of storage for actual stable variables (here `base`), upgrading `StableLog` to a new implementation (perhaps to provide more functionality) should not consume too many cycles, regardless of the current size of the log.
