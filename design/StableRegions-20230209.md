# Stable Region Allocator Design

- 20230209
- Matthew Hammer

## Purpose of this document

This document gives a design for a new “stable region” allocator for Motoko’s runtime system, and accompanying `base` library.

The role of this feature is summarized in the next section,
followed by design considerations and details.


## Role for "stable regions" in Motoko

The current stable memory module in `base` has been "experimental" for a long time, and requires a more composable API to graduate from this status.

Stable regions address the problem that today's `ExperimentalStableMemory` module only provides a single, monolithic memory that makes it unsuitable for directly building composable software parts.

Stable regions permit a new API that supports composable use cases.

Stable regions also bring Motoko closer to parity with Rust canister development support today, by giving a run-time-system-based analog of a special Rust library for stable data structures that allocates “pages” for them from stable memory in separate memory regions.


## Design space

The design space for the page allocator is defined by at least two
tensions:

 1. fully-stable representation of allocator meta data **versus** fast load/store operations.

 2. Total scaling capacity **versus** minimum footprint for meta data.


**Tension 1** is introduced because we want to avoid relying on the Motoko heap as the "ground truth" about the allocator's state.  If this heap is lost, as it is during an upgrade, then a developer may still want to recover all of the regions' data and meta data.

On the other hand, during ordinary canister execution, ~we *do* want to rely on the heap (not stable memory) for meta data to avoid its higher access costs for load and store operations, and thus we need meta data in two places, both heap and stable memory.~

Tension 1 is resolved by ~storing certain meta data twice, just as with the Rust implementation that serves as our basis.~ storing the data only in stable memory, and relying on those accesses becoming faster in the near future. (looking into the near-future roadmap for canister stable memory, it seems this is very likely.)

Compared with the Rust version, we store enough extra meta data to permit:

 - Regions whose page blocks are in arbitrary order, not
   necessarily in order of smallest to highest address.

 - 32767 Regions max (instead of 255 Regions max).

We want to permit arbitrary page block orders to make a smooth
transition to region reclamation and re-allocation in the near
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

### Q: What determines the 8MB non-empty region minimum?

This size comes from wanting to grow a region by more than one page at
a time (in terms of the way that the canister interacts with the
system API, at least).  Rather than actually grow by a single page,
the implementation grows by a "page block" (8MB) at a time.

This choice means that there are 128 pages per page block, and that
the maximum number of regions and region blocks are each relatively
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

### Q: When is stable memory becoming less costly?

TBA

### Q: How does the cheaper stable memory access cost compare with ordinary heap memory access cost?

TBA


## Design details

### API

Internal region allocator operations:


 - `initialize` -- called by the RTS, not by the Motoko developer.
 - `rebuild` -- also called by the RTS, during an post-upgrade.


User-facing region allocator operations:

 - `region_new` -- create a dynamic region.
 - `region_grow` -- grow region by a specified number of pages.
 - `region_load` -- read some data from the region.
 - `region_store` -- store some data into the region.

And a special operation, for testing our design for future GC integration (bonus):

- `region_release` -- release region and reuse region's page blocks.

The `_release` operation is *not* part of the user-facing API nor part of the MVP,
but supporting it is important because it means we can transition quickly to an integration
with the ambient Motoko GC if we can support it.

## Internal footprint

The state of the allocator is stored in a combination of stable memory and heap memory.

The stable memory state is sufficient to reconstitute the heap objects
(`rebuild` operation, described in a subsection below).  That means
that even if the heap is lost (like during an upgrade that fails) the
stable memory state can fully describe the parts that should be
rebuilt into the heap when the upgrade succeeds.

### stable memory fields

 - total allocated blocks, `Nat16`, max value is `32768`.
 - total allocated regions, `Nat16`, max value is `32767` (one region is reserved for "no region").
 - The `block-region` table (fixed size, about 131kb).
 - The `region-blocks` table (fixed size, about 524kb).

### representation of values of type `Region`

 - We use the region ID, a `Nat16`, to represent the region as a value.
 - To compute a load or store access, this requires consulting stable memory for (almost all) meta data.
 - We rely on the cost of these stable memory accesses going down in the near future to support this design decision.

### block-region table

 - purpose:
   - relate a block ID ("page block ID") to its region (if any) and its position in that region (see `rebuild`).

   - NB: The organization of this table is not useful for efficient
     access calculations of load or store (they require a linear
     search that would be prohibitively slow). Rather, the
     `region-blocks` table is designed for that purpose.  OTOH, this
     table is suitable to do a "batch" rebuild of the dynamic heap-allocated vectors
     in that table, which get lost during upgrades.

 - 32768 entries (statically sized).
 - 4 bytes per entry.
 - entry type = `BlockRegion { region : Nat16, position : Nat16 }`
 - the location of each entry gives its corresponding block ID.

### region-blocks table

 - purpose:
  - relate a region ID to its vector of block IDs, to compute an access efficiently (load or store).

  - NB: The organization of this table is meant to support O(1) load
    and store operations, but in so doing, it needs to use a
    dynamically-sized vector for each region.  There is no a priori
    way to know how to allocate these, and if we naively preallocate
    them to each be the potential "maximal region" (with all blocks
    allocated to it), the resulting preallocated table requires
    gigabytes of space, and thus is prohibitively large.

    At the same time, having dynamically-sized stable vectors is kind
    of the point of regions (each is such a vector, in a sense), and
    so requiring these first to implement regions' meta data seems
    potentially circular in concept, if not also very complex.

    So, we seem forced to use dynamically-sized heap vectors, and to
    deal the compilations of integrating them with GC (how involved is
    that?) and rebuilding them on upgrade.


 - 32768 entries (statically sized, ignoring the `vec_ptr` objects referenced therein).
 - 16 bytes per entry.
 - entry type = `RegionBlocks { size_in_pages: Nat64; vec_capacity : Nat32; vec_ptr: Nat32 }`
 - the location of each entry in the table gives its corresponding region ID.
 - the `size_in_pages` field measures the size of the region in terms of _pages_, not blocks. 
 - the `vec_capacity` and `vec_ptr` fields work with `size_in_pages`
   to represent a growable vector that we call the region's **"access
   vector"** (because "blocks vector" sounds a bit strange, and its
   used to support O(1) access operations): 
   - the access vector's address is held in `vec_ptr` and it has `vec_capacity` slots.
   - the first `size_in_pages / 128` slots of `vec_ptr` contain a valid page block ID for the region.
   - to support dynamic growth, _the access vector is held in the Motoko **heap** rather than **stable memory**._
   - the access vector doubles when it grows.
   - no region has more than 32k page blocks, so a `Nat16` suffices for `capacity`,
   - but we use a `Nat32` for `capacity` to make all fields things word-aligned (does that matter?).
   - during an upgrade, the access vectors are "rebuilt" in a batch (see `rebuild`).


### Overview of `rebuild`

When a canister upgrades, its heap is lost and the access vectors for each region are lost too (see `region-blocks` table section).

We use the `block-region` and `region-blocks` tables in stable memory to rebuild these vectors in the latter table:

 - The `region-blocks` table gives a size for each region, saying how large each vector for each region needs to be.
 - The `block-region` table gives a relative position and region ID for each block ID.

Once each regions' vectors have been allocated, the block-region table says how to fill them, one entry at a time.
