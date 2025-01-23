---
sidebar_position: 1
---

# Persistence modes

Motoko features two implementations for orthogonal persistence:

* [Enhanced orthogonal persistence](enhanced.md), currently in beta stage, provides very fast upgrades, scaling independently of the heap size. This is realized by retaining the entire Wasm main memory on an upgrade and simply performing a type-driven upgrade safety check. By using 64-bit address space, it is designed to scale beyond 4 GiB and in the future, offer the same capacity like stable memory.

* [Classical orthogonal persistence](classical.md) is the old implementation of orthogonal persistence that will be superseded by enhanced orthogonal persistence. On an upgrade, the runtime system first serializes the persistent data to stable memory and then deserializes it back again to main memory. While this is both inefficient and unscalable, it exhibits problems on shared immutable data (potentially leading to state explosion), deep structures (call stack overflow) and larger heaps (the implementation limits the stable data to at most 2 GiB).
