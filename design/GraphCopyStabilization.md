# Graph-Copy-Based Stabilization

This is part of the enhanced orthogonal persistence support, see [Orthogonal Persistence](OrthogonalPersistence.md).

## Purpose
This allows future potentially radical changes of the persistent memory layout, such as introducing a new GC, rearranging persistent metadata, or specializing arrays for small element types etc. 
This also relies on precise value tagging to allow more advanced changes that require value metadata, e.g. specializing arrays for small value element types or even downgrading to 32-bit heap layouts (provided that the amount of live data fits into a 32-bit memory).

## Design
Graph copy of sub-graph of stable objects from main memory to stable memory and vice versa on upgrades.

## Properties
* Preserve sharing for all objects like in the heap.
* Allow the serialization format to be independent of the main memory layout.
* Limit the additional main memory needed during serialization and deserialization.
* Avoid deep call stack recursion (stack overflow).
* Allows arbitrarily long large stabilization/destabilization due to incremental mechanism (see below).

## Memory Compatibility Check
Apply a memory compatibility check analogous to the enhanced orthogonal persistence, since the upgrade compatibility of the graph copy is not identical to the Candid subtype relation.

## Incremental Upgrade
Supporting arbitrarily large upgrades beyond the instruction limit:
* Splitting the stabilization/destabilization in multiple asynchronous messages.
* Limiting the stabilization work units to fit the update or upgrade messages.
* Blocking other messages during the explicit incremental stabilization.
* Restricting the upgrade functionality to the canister owner and controllers.
* Stopping the GC during the explicit incremental upgrade process.

**Note**: Graph copying needs to be explicitly initiated as the usual upgrade engages enhanced orthogonal persistence, simply retaining main memory with compatibility check.

### Usage
When upgrading to a Motoko version that is not compatible with the current enhanced orthogonal persistence:

1. Initiate the explicit stabilization before the upgrade:
    
```
dfx canister call CANISTER_ID __motoko_stabilize_before_upgrade "()"
```

* An assertion first checks that the caller is the canister owner or a canister controller.
* All other messages to the canister will be blocked until the upgrade has been successfully completed.
* The GC is stopped.
* If defined, the actor's pre-upgrade function is called before the explicit stabilization.
* The stabilzation runs in possibly multiple asynchronous messages, each with a limited number of instructions.

2. Run the actual upgrade:

```
dfx deploy CANISTER_ID
```

* Completes the explicit stabilization if not yet done before this call.
* Perform the actual upgrade of the canister on the IC.
* Detects that graph-copying is in use.
* Clears the heap if enhanced orthogonal persistence is active.
* Start the destabilization with a limited number of steps to fit into the upgrade message.
* If destabilization cannot be completed, the canister does not start the GC and does not accept messages except step 3.

3. Complete the explicit destabilization after the upgrade:

```
dfx canister call CANISTER_ID __motoko_destabilize_after_upgrade "()"
```

* An assertion checks that the caller is the canister owner or a canister controller.
* All other messages remain blocked until the successful completion of the destabilization.
* The destabilzation runs in possibly multiple asynchronous messages, each with a limited number of instructions.
* If defined, the actor's post-upgrade function is called at the end of the explicit destabilization.
* The GC is restarted.

### Remarks
* When receiving the `dfx` error "The request timed out." during explicit stabilization, upgrade, or destabilization, one can simply repeat the call until it completes.
* Steps 3 (explicit destabilization) may not be needed if the corresponding operation fits into the upgrade message.
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

## Graph-Copy Algorithm
Applying Cheney’s algorithm [1, 2] for both serialization and deserialization:

### Serialization
* Cheney’s algorithm using main memory as from-space and stable memory as to-space: 
* Focusing on stable variables as root (sub-graph of stable objects).
* The target pointers and Cheney’s forwarding pointers denote the (skewed) offsets in stable memory.
* Using streaming reads for the `scan`-pointer and streaming writes for the `free`-pointer in stable memory.

### Deserialization
* Cheney’s algorithm using stable memory as from-space and main memory as to-space: 
* Starting with the stable root created during the serialization process.
* A scan stack is used in the main memory to remember the deserialized objects for later scanning.
* Objects are allocated in main memory using the default allocator.
* Using random read/write access on the stable memory.

## Stable Format
For a long-term perspective, the object layout of the serialized data in the stable memory is fixed and independent of the main memory layout.
* Pointers are represented in 64-bit like main memory in enhanced orthogonal persistence.
* The Brooks forwarding pointer used by the incremental GC is omitted.
* The pointers encode skewed stable memory offsets to the corresponding target objects.
* References to the null objects are encoded by a defined null sentinel value.
* `BigInt` are explicitly serialized in a defined portable little endian representation, without that the serialization or deserialization allocates temporary objects.
The format is also versioned to allow future refinements of the graph copy algorithm.

## Specific Aspects
* Field hashes in objects are serialized in a blob. On deserialization, the hash blob is allocated in the dynamic heap. Same-typed objects that have been created by the same program version share the same hash blob.
* Stable records can dynamically contain non-stable fields due to structural sub-typing. A dummy value can be serialized for such fields as a new program version can no longer access this field through the stable types.
* For backwards compatibility, old Candid destabilization is still supported when upgrading from a program that used older compiler version.
* Incremental GC: Serialization needs to consider Brooks forwarding pointers (not to be confused with the Cheney's forwarding information), while deserialization can deal with partitioned heap that can have internal fragmentation (free space at partition ends).
* The partitioned heap prevents linear scanning of the heap, especially in the presence of large objects that can be placed at a higher partition than subsequently allocated normal-sized objects. For this reason, a scan stack is allocated in the main memory, remembering the deserialized objects that still need to be scanned. With this, the deserialization does not need to make any assumptions of the heap structure (e.g. monotonically increasing allocations, free space markers, empty heap on deserialization start etc.).
* If actor fields are promoted to the `Any` type in a new program version, their content is released in that variable to allow memory reclamation.
* Both stabilization and destabilization read and write data linearly, which is beneficial for guarding a work set limit (number of accessed pages) per IC message. Destabilization is also linear because it deserializes objects in the same order back as they have been serialized.

## Open Aspects
* Unused fields in stable records that are no longer declared in a new program versions should be removed. This could be done during garbage collection, when objects are moved/evacuated. This scenario equally applies to enhanced orthogonal persistence.
* The scan stack used during destabilization involves dynamic allocations.

## References

[1] C. J. Cheney. A Non-Recursive List Compacting Algorithm. Communications of the ACM, 13(11):677-8, November 1970.

[2] R. Jones and R. Lins. Garbage Collection: Algorithms for Automatic Dynamic Memory Management. Wiley 2003. Algorithm 6.1: Cheney's algorithm, page 123.
