---
sidebar_position: 12
---

# Garbage collection

Garbage collection is an automatic process that manages memory usage by detecting unreachable objects and freeing up their allocated heap memory.

[Motoko’s default garbage collection](https://dl.acm.org/doi/10.1145/3623507.3627672) method uses a copying approach that depends on the amount of heap memory currently in use. Another supported garbage collector, the incremental GC, uses a marking approach, which instead considers the amount of free heap memory. These garbage collectors are triggered when enough changes have been made to heap memory since the last collection cycle. 

The default garbage collector is limited by the ICP instruction limit per message, preventing it from collecting the entire heap memory pool in a single execution. Since the entire GC run needs to be performed in one message, it is too big for larger-scaled heaps (depending on the constellation of the heap). 

The incremental garbage collector is recommended to be used for larger heaps, which distributes the workload across multiple messages as needed. It can be enabled by specifying the `--incremental-gc` compiler flag in the project's `dfx.json` file.

```json
{
  "canisters": {
    "my_dapp": {
       "main": "src/my-dapp.mo",
       "type": "motoko",
       "args" : "--incremental-gc"
    }
  }
}
```


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />
