---
sidebar_position: 12
---

# Garbage collection

Garbage collection is an automatic process that manages memory usage by removing unreferenced or dead objects and freeing up allocated heap memory.

Motoko’s default garbage collection method uses a copying approach that depends on the amount of heap memory currently in use. An alternative garbage collector uses a marking approach, which instead considers the amount of free heap memory. These garbage collectors are triggered when enough changes have been made to heap memory since the last collection cycle. Garbage collection can be forced to run after every message using the `--force-gc` flag in the project's `dfx.json` file:

```json
"defaults": {
  "build": {
    "packtool": "",
    "args": "--force-gc"
  }
}
```

Both garbage collectors are limited by the ICP instruction limit per message, preventing them from collecting the entire heap memory pool in a single execution. As a result, [canisters](https://internetcomputer.org/docs/building-apps/essentials/canisters) cannot fully utilize the entire 4GiB memory pool, as some free space must be reserved for the garbage collector to operate.

A beta incremental garbage collection process is available, which distributes the workload across multiple messages as needed. This approach allows canisters to allocate up to three times more heap space after collection while consuming fewer cycles on average. With incremental garbage collection, canisters can take advantage of the entire 4GiB heap memory pool.

The incremental garbage collector can be enabled by specifying the `--incremental-gc` compiler flag in the project's `dfx.json` file.

:::caution

This garbage collector is still in beta and should be used with caution.

:::


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
