---
sidebar_position: 13
---

# Orthogonal persistence

Motoko features two implementations for orthogonal persistence, enhanced orthogonal persistence and classical orthogonal persistence.

| Feature | Enhanced persistence | Classical persistence |
|---------|-----------------------------------|-----------------------------------|
| Upgrade process | Retains entire WebAssembly (Wasm) memory. | Serializes and deserializes stable data. |
| Scalability | Scales beyond 4GiB. | Limited to 2GiB. |
|Performance | No serialization overhead. | Serialization impacts performance. |
| [State](https://internetcomputer.org/docs/motoko/fundamentals/state) management | All variables are stable by default. | Stable variables must be explicitly declared using `stable`. |
| Handling non-persistent data | `transient` keyword used for exceptions. | Requires manual migration logic (`preupgrade`/`postupgrade`). |

During upgrades, enhanced persistence retains the entire WebAssembly (Wasm) memory, while classical persistence relies on serializing and deserializing stable data. Enhanced persistence scales beyond 4GiB, whereas classical persistence is limited to 2GiB.

Regarding performance, enhanced persistence avoids serialization overhead. Classical persistence uses serialization overhead, which can make it slower.

Enhanced persistence makes all variables stable by default, while classical persistence requires explicitly declaring stable variables using the `stable` keyword. For handling non-persistent data, enhanced persistence uses the `transient` keyword for exceptions, whereas classical persistence depends on manual migration logic via `preupgrade` and `postupgrade` functions.

## Enhanced orthogonal persistence

Enhanced orthogonal persistence improves upgrade efficiency by retaining the Wasm memory of a canister across upgrades. This removes the need for serialization and deserialization, making upgrades faster and reducing performance overhead through:

- **Stable heap**: The entire program memory persists across upgrades.
- **64-bit address space**: Expands beyond the 2GiB limit of classical persistence. Currently the maximum heap size is 6GiB.
- **No explicit stable memory**: Developers work directly with Motoko's object structures, which persist automatically.

Enhanced orthogonal persistence is blessed for use in production.

### Enabling EOP with `moc`

```sh
moc --enhanced-orthogonal-persistence
```

### Enabling EOP in `dfx.json`**

```json
{
    "type": "motoko",
    "args": "--enhanced-orthogonal-persistence"
}
```

Before an upgrade, the system verifies that the new program version is memory-compatible with the old one. The following changes are allowed:

- Adding or removing [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) fields.
- Changing mutability (`let` ↔ `var`).
- Removing object fields.
- Adding variant fields.
- Changing [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) to [`Int`](https://internetcomputer.org/docs/motoko/base/Int).
- Adjustments to shared function parameters and return types.

If an upgrade is incompatible, the system automatically rolls it back to prevent data loss. More complex changes require explicit migration.

When switching to enhanced persistence, the system performs one final deserialization from stable memory before fully transitioning to memory retention. After this transition, a canister cannot revert to classical persistence. However, the legacy stable memory remains available as secondary storage, giving developers the flexibility to combine traditional stable regions with enhanced orthogonal persistence if needed.

The ICP runtime environment provides options for Wasm memory retention during upgrades:

1. `wasm_memory_persistence = opt keep`

   - Retains Wasm memory across upgrades.
   - Cannot be used with classical persistence.

2. `wasm_memory_persistence = null`

   - Uses classical persistence.
   - Replaces Wasm memory on upgrade.

3. `wasm_memory_persistence = opt replace`

   - Not recommended for enhanced persistence.
   - Completely erases Wasm memory, leading to data loss.

For new projects, enhanced persistence is recommended for efficiency, scalability, and ease of use.

## Classical orthogonal persistence

Classical orthogonal persistence is the original implementation of Motoko's orthogonal persistence system. During an upgrade, the classical orthogonal persistence mechanism serializes all stable data to stable memory and deserializes it back to main memory after the upgrade.

This process occurs through `postupgrade` and `preupgrade` hooks, which:

- Must copy the entire heap [state](https://internetcomputer.org/docs/motoko/fundamentals/state) during upgrades regardless of what actually changed.

- Force developers to serialize and deserialize all stable data, even when only small portions are modified.

- Create a significant performance bottleneck as the data size increases.

This approach has several significant downsides:

- A maximum of 2GiB of heap data can be persisted across upgrades due to implementation restrictions. In practice, the actual supported amount of stable data may be smaller.

- Shared immutable heap objects can be duplicated, potentially causing [state](https://internetcomputer.org/docs/motoko/fundamentals/state) conflicts during upgrades.

- Deeply nested data structures can lead to call stack overflows during serialization/deserialization.

- The serialization and deserialization processes are computationally expensive and may exceed the ICP's instruction limits.

- The runtime system does not include built-in stable compatibility verification. If users disregard upgrade warnings from [`dfx`](https://internetcomputer.org/docs/building-apps/getting-started/install), data may be lost or the upgrade might fail completely.

These issues can lead to a canister that can no longer be upgraded. To prevent this, it's essential to thoroughly test your application's upgrade process and carefully limit the amount of data each canister holds. Additionally, implementing backup mechanisms is strongly recommended. For example, controller-privileged data query calls can be used to recover data in the event of an upgrade failure, helping ensure data is not permanently lost.


<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />