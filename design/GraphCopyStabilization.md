# Graph-Copy-Based Stabilization

Using graph copying instead of Candid-based serialization for stabilization, to save stable variables across upgrades. 

## Goals

* **Stop-gap solution until enhanced orthogonal persistence**: More scalable stabilization than the current Candid(ish) serialization.
* **With enhanced orthogonal persistence**: Upgrades in the presence of memory layout changes introduced by future compiler versions.

## Design

Graph copy of sub-graph of stable objects from main memory to stable memory and vice versa on upgrades.

## Properties
* Preserve sharing for all objects like in the heap.
* Allow the serialization format to be independent of the main memory layout.
* Limit the additional main memory needed during serialization and deserialization.
* Avoid deep call stack recursion (stack overflow).

## Memory Compatibility Check
Apply a memory compatibility check analogous to the enhanced orthogonal persistence, since the upgrade compatibility of the graph copy is not identical to the Candid subtype relation.

## Algorithm
Applying Cheney’s algorithm [1, 2] for both serialization and deserialization:

### Serialization
* Cheney’s algorithm using main memory as from-space and stable memory as to-space: 
* Focusing on stable variables as root (sub-graph of stable objects).
* The target pointers and Cheney’s forwarding pointers denote the (skewed) offsets in stable memory.
* Using streaming reads for the `scan`-pointer and streaming writes for the `free`-pointer in stable memory.

### Deserialization
* Cheney’s algorithm using stable memory as from-space and main memory as to-space: 
* Starting with the stable root created during the serialization process.
* Objects are allocated in main memory using the default allocator.
* Using random read/write access on the stable memory.

## Stable Format
For a long-term perspective, the object layout of the serialized data in the stable memory is fixed and independent of the main memory layout.
* Pointers support 64-bit representations, even if only 32-bit pointers are used in current main memory address space.
* The Brooks forwarding pointer is omitted (used by the incremental GC).
* The pointers encode skewed stable memory offsets to the corresponding target objects.
* References to the null objects are encoded by a sentinel value.

## Specific Aspects
* The null object is handled specifically to guarantee the singleton property. For this purpose, null references are encoded as sentinel values that are decoded back to the static singleton of the new program version.
* Field hashes in objects are serialized in a blob. On deserialization, the hash blob is allocated in the dynamic heap. Same-typed objects that have been created by the same program version share the same hash blob.
* Stable records can dynamically contain non-stable fields due to structural sub-typing. A dummy value can be serialized for such fields as a new program version can no longer access this field through the stable types.
* For backwards compatibility, old Candid destabilzation is still supported when upgrading from a program that used older compiler version.
* Incremental GC: Serialization needs to consider Brooks forwarding pointers (not to be confused with the Cheney's forwarding information), while deserialization can deal with partitioned heap that can have internal fragmentation (free space at partition ends).

## Allocator Rules
The destabilization requires scanning the heap which is more involved for the partitioned heap used by the incremental GC. The allocator is required to yield monotonically increasing addresses during deserialization. Free space gaps are allowed to complete partitions.

## Open Aspects
* Unused fields in stable records that are no longer declared in a new program versions should be removed. This could be done during garbage collection, when objects are moved/evacuated.
* The binary serialization and deserialization of `BigInt` entails dynamic allocations (cf. `mp_to_sbin` and `mp_from_sbin` of Tom's math library).

## Related PRs

* Motoko Enhanced Orthogonal Persistence: https://github.com/dfinity/motoko/pull/4225
* Motoko Incremental Garbage Collector: https://github.com/dfinity/motoko/pull/3837

## References

[1] C. J. Cheney. A Non-Recursive List Compacting Algorithm. Communications of the ACM, 13(11):677-8, November 1970.

[2] R. Jones and R. Lins. Garbage Collection: Algorithms for Automatic Dynamic Memory Management. Wiley 2003. Algorithm 6.1: Cheney's algorithm, page 123.
