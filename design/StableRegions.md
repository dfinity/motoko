# Stable Region API

See StableMemory.md for context of the current experimental API.

This document aims to specify the API and memory representations for a generalization
of this API that permits multiple _regions_ of stable memory, where each can be
grown independently.

The **region manager** is the state and logic to support this generalization.

## Definitions and constants

 - a **page** is 65536 bytes.
 - a **page block** is a contiguous sequence of 128 pages (~8MB).
 - a **page block index** is a 16 bit, index-based identifier for a page block.
 - a **region** is a sequence of (generally non-contiguous) **page blocks**.
 - the maximum number of pages is 32768.
 - the maximum amount of stable memory for all regions is 256GB.
 - there is no bound on number of regions.

More design decisions:

 - each region is addressable as if it were an independent, contiguous stable memory.
 - the API for a single region resembles that of the current `ExperimentalStableMemory` API.
 - each region is represented as a special Motoko heap object (see representation details).
 - the region manager's state consists of all regions in the Motoko heap, and
   a special header within stable memory.

## Region representation

Each region lives in the Motoko heap, and eventually, will be
collected by the ambient GC algorithm (requiring its own special GC
header).

Avoid reclaimation by storing each region in a stable variable.

Eventually, but not initially, the GC reclaiming a Region will release
its page blocks to the region manager for future reuse by another
region.  Initially, this reclaimed region's page blocks are simply
leaked.

Each region heap object consists of the following fields:

 - (special GC header).
 - number of allocated pages (4 bytes).
 - P = number of allocated page blocks (2 bytes).
 - capacity of allocated page blocks (2 bytes).
 - pointer to page block index vector (P * 2 bytes long).

Vector of indices (growable by doubling):

 - page block index 0 (2 bytes).
 - ...
 - page block index P-1 (2 bytes).

Notes:

 - Each page block index is 16 bits.
 - No more than 32k page blocks total, for all regions.
 - ==> No more than ~64k bytes for any one region.
 - the "capacity" field helps us double the existing region capacity to grow it.
   (avoid quadratic corner case when user grows one page block at a time, 32k times)


## Region manager state
 
Within a special header of stable memory, the region manager stores:

 - (Region manager layout version).
 - N = total number of allocated page blocks.
 - page block 0.
 - ...
 - page block N-1.
 




