---
sidebar_position: 5
---

# Enhanced orthogonal persistence

This implements the vision of efficient and scalable orthogonal persistence in Motoko that combines:
* **Stable heap**: Persisting the program main memory across canister upgrades.
* **64-bit heap**: Extending the main memory to 64-bit for large-scaled persistence.

As a result, the use of secondary storage (explicit stable memory, dedicated stable data structures, DB-like storage abstractions) will no longer be necessary: Motoko developers can directly work on their normal object-oriented program structures that are automatically persisted and retained across program version changes.

### Activation
Enhanced orthogonal persistence is currently offered for **beta testing** via the compiler flag `--enhanced-orthogonal-persistence`.

To activate enhanced orthogonal persistence under `dfx`, the following command-line argument needs to be specified in `dfx.json`:

```
...
    "type" : "motoko"
    ...
    "args" : "--incremental-gc"
...
```

:::tip
Despite the use of enhanced orthogonal persistence, it is strongly recommended to thoroughly test the upgrades of your application.
Moreover, it is advised to have a backup possibility for rescuing data even when upgrades fail, e.g. by controller-privileged data query calls.
:::

[Classical persistence](classical-persistence.md) with 32-bit main memory and Candid stabilization currently remains the default mode.
See [persistence modes](upgrades.md#persistence-modes) for more information.

## Advantages
Compared to the existing orthogonal persistence in Motoko, this design offers:
* **Performance**: New program versions directly resume from the existing main memory and have access to the memory-compatible data.
* **Scalability**: The upgrade mechanism scales with larger heaps and in contrast to serialization, does not hit IC instruction limits.

Compared to the explicit use of stable memory, this design improves:
* **Simplicity**: Developers do not need to deal with explicit stable memory.
* **Performance**: No copying to and from the separate stable memory is necessary.

## Design
The enhanced orthogonal persistence is based on the following main properties:
* Extension of the IC to retain main memory on upgrades.
* Supporting 64-bit main memory on the IC.
* A long-term memory layout that is invariant to new compiled program versions.
* A fast memory compatibility check performed on each canister upgrade.
* Incremental garbage collection using a partitioned heap.

### Compatibility Check
Upgrades are only permitted if the new program version is compatible with the old version, such that the runtime system guarantees a compatible memory structure.

Compatible changes for immutable types are largely analogous to the allowed Motoko subtype relation, e.g.
* Adding or removing actor fields.
* Removing object fields.
* Adding variant fields.
* `Nat` to `Int`.
* Shared function parameter contravariance and return type covariance.

The existing IDL-subtype functionality is reused with some adjustments to check memory compatibility: The compiler generates the type descriptor, a type table, that is recorded in the persistent metadata. Upon an upgrade, the new type descriptor is compared against the existing type descriptor, and the upgrade only succeeds for compatible changes.

This compatibility check serves as an additional safety measure on top of the DFX Candid subtype check that can be bypassed by users (when ignoring a warning). Moreover, in some aspects, the memory compatibility rules differ to the Candid sub-type check:
* Top-level actor fields (`stable` fields) can change mutability (`let` to `var` and vice-versa).
* Support of variable (MutBox) with type invariance.
* Types cannot be made optional (no insertion of Option).
* Same arity for function parameters and function return types (no removed optional parameters, no additional optional results).
* Records cannot introduce additional optional fields.
* Same arity for tuple types (no insertion of optional items).
* Records and tuples are distinct.

### Memory Capacity
The canister has no upfront knowledge of the maximum allocatable Wasm main memory in 64-bit address space, as there is no IC API call to query the main memory limit. This limit may also be increased in future IC releases.

Therefore, a mechanism is implemented to deal with an unknown and dynamically increasable main memory capacity offered by the IC. This is needed in two cases:

* GC reserve (strict): The runtime system ensures sufficient free space to allow garbage collection at all times, even if the heap is full. For this purpose, the runtime system already pre-allocates the reserve, to be sure that the reserve is available despite the unknown capacity. As an optimization, this pre-allocation is skipped when the memory demand including the GC reserve is below a guaranteed minimum Wasm memory limit of the IC, e.g. 4GB or 6GB.
* GC scheduling (heuristic): The GC schedules at high frequency when memory is becoming scarce. For this purpose, the GC maintains an assumption of the minimum memory limit and probes the supposed limit when the heap size approaches this limit. If the allocation succeeds, the assumed limit is increased. Otherwise, the critical high-frequency GC scheduling rate is activated.

In both cases, the runtime system tries to reduce Wasm memory allocations as much as possible, i.e. not pre-allocating memory for small heap sizes, and not probing an allocation in certain memory ranges by assuming that the IC only offers main memory of a certain granularity, e.g. multiples of 2GB. To save instructions, the critical GC scheduling is only activated when reaching the actual memory limit. Moreover, the mechanism can handle an increased memory capacity at runtime, e.g. when the IC is upgraded to a new release with a higher memory limit.

### Migration Path
When migrating from the old serialization-based stabilization to the new persistent heap, the old data is deserialized one last time from stable memory and then placed in the new persistent heap layout. Once operating on the persistent heap, the system should prevent downgrade attempts to the old serialization-based persistence. 

Assuming that the persistent memory layout needs to be changed in the future, the runtime system supports serialization and deserialization to and from stable memory in a defined data format using graph copy. 

### Graph Copy
The graph copy is an alternative persistence mechanism that will be only used in the rare situation when the persistent memory layout will be changed in the future. Arbitrarily large data can be serialized and deserialized beyond the instruction and working set limit of upgrades: Large data serialization and deserialization is split in multiple messages, running before and/or after the IC upgrade to migrate large heaps. Of course, other messages will be blocked during this process and only the canister owner or the canister controllers are permitted to initiate this process. 

Graph copying needs to be explicitly initiated before an upgrade to new Motoko version that is incompatible to the current enhanced orthogonal persistent layout. For large data, the graph copy needs to be manually completed after the actual upgrade.

```
dfx canister call CANISTER_ID __motoko_stabilize_before_upgrade "()"
dfx deploy CANISTER_ID
dfx canister call CANISTER_ID __motoko_destabilze_after_upgrade "()"
```

More detailed information and instructions are explained in the section on [graph copy](graph-copy-stabilization.md).

### Old Stable Memory
The old stable memory remains equally accessible as secondary (legacy) memory with the new support.
