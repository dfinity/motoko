---
sidebar_position: 2
---

# Stable memory



The [`Region` library](stable-regions.md) can be used to interact with stable memory on ICP.

The  library provides low-level access to Internet Computer stable memory.

:::danger
The `ExperimentalStableMemory` library has been deprecated.

New applications should use the `Region` library: it offers additional isolation between different libraries using stable memory.
:::

## Mops packages for stable memory

- [`memory-buffer`](https://mops.one/memory-buffer): Persistent buffer implementation.

- [`memory-hashtable`](https://mops.one/memory-hashtable): A library for storing, updating, deleting, and retrieving a single blob-value per key.

- [`StableTrie`](https://mops.one/stable-trie): A key-value map data structure that has its main data living permanently in stable memory using Regions.

## Samples

- [motoko-bucket](https://github.com/PrimLabs/Bucket): A key value database library that uses stable memory.

- [motoko-cdn](https://github.com/gabrielnic/motoko-cdn): An auto-scaling storage solution.

- [motoko-dht](https://github.com/enzoh/motoko-dht): A distributed hash table sample.

- [motoko-document-db](https://github.com/DepartureLabsIC/motoko-document-db): A document database sample.

<img src="https://github.com/user-attachments/assets/844ca364-4d71-42b3-aaec-4a6c3509ee2e" alt="Logo" width="150" height="150" />