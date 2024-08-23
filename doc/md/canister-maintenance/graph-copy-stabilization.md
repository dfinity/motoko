---
sidebar_position: 6
---

# Graph copy stabilization

This is part of the [enhanced orthogonal persistence](enhanced-orthogonal-persistence.md) support.

This is an alternative upgrade mechanism for enhanced orthogonal persistence to allow future potentially radical changes of the persistent memory layout, such as introducing a new GC, rearranging persistent metadata, or specializing arrays for small element types etc. 
It is only used in the rare case when the program needs to be upgraded to a substantially new compiler version that introduces a different memory layout.
The Motoko runtime system always checks whether the memory layout is compatible to the runtime version, to prevent any data corruption or data loss by new Motoko versions.

## Properties

Graph copy stabilization works by serializing stable data to the stable memory in long-term self-descriptive format. Contrary to classical persistence, it is scalable and can deal with arbitrarily large heaps. For this purpose, it implements a kind of unlimited Motoko-level deterministic time slicing that can even pass IC checkpoints.

More specifically, it has the following properties:
* Preserve sharing for all objects like in the heap.
* Allow the serialization format to be independent of the main memory layout.
* Limit the additional main memory needed during serialization and deserialization.
* Avoid deep call stack recursion (stack overflow).
* Allows arbitrarily long large stabilization/destabilization due to incremental mechanism (see below).

## Memory Compatibility Check
Apply a memory compatibility check analogous to the enhanced orthogonal persistence, since the upgrade compatibility of the graph copy is not identical to the Candid subtype relation.

## Incremental Upgrade
Graph copy stabilization supports arbitrarily large upgrades beyond the instruction limit:
* Splitting the stabilization/destabilization in multiple asynchronous messages.
* Limiting the stabilization work units to fit the update or upgrade messages.
* Blocking other messages during the explicit incremental stabilization.
* Restricting the upgrade functionality to the canister owner and controllers.
* Stopping the GC during the explicit incremental upgrade process.

:::note
Graph copying needs to be explicitly initiated as the usual upgrade engages enhanced orthogonal persistence, simply retaining main memory with compatibility check.
:::

### Usage
When upgrading to a Motoko version that is not compatible with the current enhanced orthogonal persistence:

1. Initiate the explicit stabilization before the upgrade:
    
```
dfx canister call CANISTER_ID __motoko_stabilize_before_upgrade "()"
```

2. Run the actual upgrade:

```
dfx deploy CANISTER_ID
```

3. Complete the explicit destabilization after the upgrade:

```
dfx canister call CANISTER_ID __motoko_destabilize_after_upgrade "()"
```

### Remarks
* When receiving the `dfx` error "The request timed out." during explicit stabilization or destabilization, one can simply repeat the call until it completes.
* Steps 2 (explicit destabilization) may not be needed if the corresponding operation fits into the upgrade message.
* Stabilization and destabilization steps are limited to the increment limits:

    Operation | Message Type | IC Instruction Limit | **Increment Limit**
    ----------|--------------|----------------------|--------------------
    **Explicit (de)stabilization step** | Update | 20e9 | **16e9**
    **Actual upgrade** | Upgrade | 200e9 | **160e9**

* The graph copy steps also limit the amount of processed stable data (read or write), in order not to exceed the 
IC's stable memory access limits.

    Operation | Message Type | IC Stable Access Limit | **Increment Limit**
    ----------|--------------|----------------------|--------------------
    **Explicit (de)stabilization step** | Update | 2 GB | **1 GB**
    **Actual upgrade** | Upgrade | 8 GB | **6 GB**
