---
sidebar_position: 13
---

# Orthogonal persistence

Motoko features two implementations for orthogonal persistence:

| Feature | Enhanced persistence | Classical persistence |
|---------|-----------------------------------|-----------------------------------|
| Upgrade process | Retains entire WebAssembly (Wasm) memory. | Serializes and deserializes stable data. |
| Scalability | Scales beyond 4GiB. | Limited to 2GiB. |
|Performance | No serialization overhead. | Serialization impacts performance. |
| [State](https://internetcomputer.org/docs/motoko/fundamentals/state) management | All variables are stable by default. | Stable variables must be explicitly declared using `stable`. |
| Handling non-persistent data | `transient` keyword used for exceptions. | Requires manual migration logic (`preupgrade`/`postupgrade`). |

**Note: Enhanced orthogonal persistence is in beta**.
<!--Will enhanced persistence be ready by the time the new docs is published?--->

## Enhanced orthogonal persistence

Enhanced orthogonal persistence improves upgrade efficiency by **retaining the Wasm memory** of a canister across upgrades. This removes the need for serialization and deserialization, making upgrades faster and reducing performance overhead through:

- **Stable heap**: The entire program memory persists across upgrades.
- **64-bit address space**: Expands beyond the 2GiB limit of classical persistence.
- **No explicit stable memory**: Developers work directly with Motoko’s object structures, which persist automatically.

Enhanced orthogonal persistence has finished beta testing and is blessed for use in production.

**Using `moc`**

```sh
moc --enhanced-orthogonal-persistence
```

**Enabling in `dfx.json`**

```json
{
    "type": "motoko",
    "args": "--enhanced-orthogonal-persistence"
}
```

Before an upgrade, the system verifies that the new program version is **memory-compatible** with the old one. The following changes are allowed:

- Adding or removing [actor](https://internetcomputer.org/docs/motoko/fundamentals/actors-async) fields.
- Changing mutability (`let` ↔ `var`).
- Removing object fields.
- Adding variant fields.
- Changing [`Nat`](https://internetcomputer.org/docs/motoko/base/Nat) to [`Int`](https://internetcomputer.org/docs/motoko/base/Int).
- Adjustments to shared function parameters and return types.

If an upgrade is incompatible, the system **automatically rolls it back** to prevent data loss. More complex changes require explicit migration.

When switching to enhanced persistence, the system performs one final deserialization from stable memory before transitioning fully to memory retention. Once a canister is using enhanced persistence, it cannot be downgraded back to classical persistence. Legacy stable memory remains accessible as secondary storage, allowing developers to combine stable regions with orthogonal persistence if needed.

ICP provides options for Wasm memory retention during upgrades:

1. `wasm_memory_persistence = opt keep`

   - Retains Wasm memory across upgrades.
   - Cannot be used with classical persistence.

2. `wasm_memory_persistence = null`

   - Uses classical persistence.
   - Replaces Wasm memory on upgrade.

3. `wasm_memory_persistence = opt replace`

   - Not recommended for enhanced persistence.
   - Completely erases Wasm memory, leading to data loss.

For new projects, **enhanced persistence is recommended** for efficiency, scalability, and ease of use.

## Classical orthogonal persistence

Classical orthogonal persistence is the original implementation of Motoko's orthogonal persistence system.

During an upgrade, the classical orthogonal persistence mechanism serializes all stable data to stable memory and deserializes it back to main memory after the upgrade.

This process occurs through post-upgrade and pre-upgrade hooks, which:

- Must copy the entire heap [state](https://internetcomputer.org/docs/motoko/fundamentals/state) during upgrades regardless of what actually changed.

- Force developers to serialize and deserialize all stable data, even when only small portions are modified.

- Create a significant performance bottleneck as the data size increases.

This approach has several significant downsides:

- A maximum of 2GiB of heap data can be persisted across upgrades due to implementation restrictions. In practice, the actual supported amount of stable data may be lower.

- Shared immutable heap objects can be duplicated, potentially causing [state](https://internetcomputer.org/docs/motoko/fundamentals/state) explosion during upgrades.

- Deeply nested data structures can lead to call stack overflows during serialization/deserialization.

- The serialization and deserialization processes are computationally expensive and may exceed the ICP's instruction limits.

- The runtime system does not include built-in stable compatibility verification. If users disregard upgrade warnings from [`dfx`](https://internetcomputer.org/docs/building-apps/getting-started/install), data may be lost or the upgrade might fail completely.

These issues can result in a "stuck" canister that becomes impossible to upgrade further. Therefore it is absolutely necessary to thoroughly test the upgrade capacity of your application and conservatively limit the data held by each canister. Implementing backup mechanisms is strongly recommended to rescue data even when upgrades fail, such as controller-privileged data query calls.

<img src="https://cdn-assets-eu.frontify.com/s3/frontify-enterprise-files-eu/eyJwYXRoIjoiZGZpbml0eVwvYWNjb3VudHNcLzAxXC80MDAwMzA0XC9wcm9qZWN0c1wvNFwvYXNzZXRzXC8zOFwvMTc2XC9jZGYwZTJlOTEyNDFlYzAzZTQ1YTVhZTc4OGQ0ZDk0MS0xNjA1MjIyMzU4LnBuZyJ9:dfinity:9Q2_9PEsbPqdJNAQ08DAwqOenwIo7A8_tCN4PSSWkAM?width=2400" alt="Logo" width="150" height="150" />