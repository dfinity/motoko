# Graph-Copy-Based Stabilization

Using graph copying instead of Candid-based serialization for stabilization, to save stable variables across upgrades. 

## Goals

* **Stop gap solution until enhanced orthogonal persistence**: More scalable stabilization than the current Candid(ish) serialization.
* **With enhanced orthogonal persistence**: Upgrades in the presence of memory layout changes introduced by future compiler versions.

## Design

Graph copy of sub-graph of stable objects from main memory to stable memory and vice versa on upgrades.

## Properties
* Preserve sharing for all objects like in the heap.
* Allow the serialization format to be independent of the main memory layout.
* Minimize the amount of stable memory accesses (expensive API calls) through streaming read/writes.
* Limit the additional main memory needed during serialization and deserialization.
* Avoid deep call stack recursion (stack overflow).

## Memory Compatibility Check
Apply a memory compatibility check analogous to the enhanced orthogonal persistence, since the graph copy layout is not identical to the Candid subtype relation.

## Algorithm
Applying Cheney’s algorithm [1, 2] combined with multiple copying between stable and main memory (credits to Claudio for this idea):

### Serialization
* Cheney’s algorithm using main memory as from-space and stable memory as to-space: 
* Focusing on stable variables as root (sub-graph of stable objects).
* The target pointers and Cheney’s forwarding pointers denote the (skewed) offsets in stable memory.
* Using streaming reads for the `scan`-pointer and streaming writes for the `free`-pointer in stable memory.. 

### Deserialization
* Copy the entire stable memory image in main memory at the heap end.
* Apply Cheney’s algorithm in serialization, however with two differences:
  - Rebase the stable memory offsets inside the pointer fields of the image to corresponding absolute main memory addresses.
  - The Cheney’s forwarding pointers denote the simulated addresses in main memory, as if the objects would be allocated on top of the existing heap.
* Copy the entire stable memory image back in main memory at the heap and extend the heap.

## Specific Aspects
* A streamed reader/writer abstraction is provided that caches stable memory access and thus minimizes stable memory API calls.
* Currently, the object payload uses the same encoding for main memory and stable memory. On a longer term, the main memory layout may however evolve and would be mapped to the current stable format.
* The null object is handled specifically to guarantee the singleton property. For this purpose, null references are encoded as sentinel values that are decoded back to the static singleton of the new program version.
* Field hashes in objects are serialized in a blob. On deserialization, the hash blob is allocated in the dynamic heap. Same-typed objects that have been created by the same program version share the same hash blob.
* For backwards compatibility, old Candid destabilzation is still supported when upgrading from a program that used older compiler version.
* Incremental GC: Serialization needs to consider Brooks forwarding pointers (not to be confused with the Cheney's forwarding information), while deserialization can deal with partitioned heap that can have internal fragmentation (free space at partition ends).

## Stable Format
Currently, for ease of implementation, the object layout in stable memory is largely identical to the main memory with the following differences:
* The Brooks forwarding pointer is omitted (used by the incremental GC).
* The pointers encode skewed stable memory offsets to the corresponding target objects.
* References to the null objects are encoded by a sentinel value.

## Allocator Rules
The destabilzation uses bulk-copying to main memory which is quite invasive. For correctness, the following invariants need to be met:
* The main memory allocator used during deserialization must not write to the heap (only compute the target addresses).
* The allocator must yield monotonically growing addresses during deserialization. Free space gaps are allowed to complete partitions.

## Related PRs

* Motoko Enhanced Orthogonal Persistence: https://github.com/dfinity/motoko/pull/4225
* Motoko Incremental Garbage Collector: https://github.com/dfinity/motoko/pull/3837

## References

[1] C. J. Cheney. A Non-Recursive List Compacting Algorithm. Communications of the ACM, 13(11):677-8, November 1970.
[2] R. Jones and R. Lins. Garbage Collection: Algorithms for Automatic Dynamic Memory Management. Wiley 2003. Algorithm 6.1: Cheney's algorithm, page 123.
