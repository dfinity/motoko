Stable Region Allocator Design for Motoko
20230209
Matthew Hammer

## Purpose of this document

This document gives a design for a new “stable region” allocator for Motoko’s runtime system, and accompanying `base` library.

The role of this feature is summarized in the next section,
followed by design considerations and details.


## Role for "stable regions" in Motoko

The current stable memory module in `base` has been "experimental" for a long time, and requires a more composable API to graduate from this status.

Stable regions address the problem that today's `ExperimentalStableMemory` module only provides a single, monolithic memory that makes it unsuitable for directly building composable software parts.

Stable regions permit a new API that supports composable use cases.

Stable regions also bring Motoko closer to parity with Rust canister development support today, by giving a run-time-system-based analogue of a special Rust library for stable data structures that allocates “pages” for them from stable memory in separate memory regions.


## Design space

The design space for the page allocator is defined by at least two
tensions:

 1. fully-stable representation of allocator meta data **versus** fast load/store operations.

 2. Total scaling capacity **versus** minimum footprint for meta data.


**Tension 1** is introduced because we want to avoid relying on the Motoko heap as the "ground truth" about the allocator's state.  If this heap is lost, as it is during an upgrade, then a developer may still want to recover all of the regions' data and meta data.

On the other hand, during ordinary canister execution, we *do* want to rely on the heap (not stable memory) for meta data to avoid its higher access costs for load and store operations, and thus we need meta data in two places, both heap and stable memory.

**Tension 1** is resolved by storing certain meta data twice, just as with the Rust implemetation that serves as our basis.

The main difference is that in our case, we store enough extra meta data to permit:

 - Regions whose page blocks are in arbitrary order, not
   necessarily in order of smallest to highest address.

 - 32767 Regions max (instead of 255 Regions max).  

We want to permit arbitrary page block orders to make a smooth
transition to region reclaimation and re-allocation in the near
future, with potential integration into the Motoko GC.  The 
extra complexity is modest, and seems "worth" the cost.

We change the maximum region limit because 255 may be too small in
some extreme cases.  

We address the question of whether the new limit of 32k regions is
"enough" in the Q&A section (it is, for all practical purposes)


**Tension 2** is introduced because we want a design that will continue
to work even when canisters can store more stable data than today (32GB).

Tension 2 is resolved by making prudent representation choices.  

The representations we choose for regions and region identifiers
permit a scaling to 256GB of stable data while still permitting meta
data to be repeated in both stable and non-stable arenas.  These are
the same limits imposed by the Rust implementation, for the same
reasons.  See Q&A for more discussion.



## Questions and answers

### Q: What determines the 8MB non-empty region mimimum?

This size comes from wanting to grow a region by more than one page at
a time (in terms of the way that the canister interacts with the
system API, at least).  Rather than actually grow by a single page,
the implementation grows by a "page block" (8MB) at a time.

This choice means that there are 120 pages per page block, and that
the maximum number of regions and region blocks are eacg relatively
small (32k each).  Consequently, they can each be identified with a
2-byte identifier, and we can pre-allocate tables to store certain
relations about them, which is critical.

### Q: Are 32767 regions enough?

A: Permitting more than 32k regions may seem theoretically
interesting, but is not practical given other parameters today that
control the minimal footprint of a region (8MB) and dictate the
maximum size of stable memory for a canister today (32GB).  With 32k
regions at 8MB each, well over the maximum stable memory size is used
(256GB compared to 32GB, the limit today)


## Design details

