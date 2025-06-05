---
sidebar_position: 3
---

# Classical orthogonal persistence

Classical orthogonal persistence is the legacy implementation of Motoko's orthogonal persistence. Classical persistence will be deprecated in the future in favor of enhanced orthogonal persistence.

Upon upgrade, the classical orthogonal persistence mechanism serializes all stable data to the stable memory and then deserializes it back to the main memory. This has several downsides:

* At maximum, 2 GiB of heap data can be persisted across upgrades. This is because of an implementation restriction. Note that in practice, the supported amount of stable data can be way lower.
* Shared immutable heap objects can duplicated, leading to potential state explosion on upgrades.
* Deeply nested structures can lead to a call stack overflow.
* The serialization and deserialization is expensive and can hit ICP's instruction limits.
* There is no built-in stable compatibility check in the runtime system. If users ignore the `dfx` upgrade warning, data may be lost or an upgrade fails.

:::danger
The above-mentioned issues can lead to a stuck canister that can no longer be upgraded.
Therefore, it is absolutely necessary to thoroughly test how much data an upgrade of your application can handle and then conservatively limit the data held by that canister.
Moreover, it is ideal to have a backup plan to rescue data even if upgrades fail, e.g. by controller-privileged data query calls.
:::

These issues are solved by [enhanced orthogonal persistence](enhanced.md).
