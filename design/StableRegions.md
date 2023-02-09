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




###### TO DO -- finish this document.
