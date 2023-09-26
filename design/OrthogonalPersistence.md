# Orthogonal Persistence (64-Bit, Scalable Upgrades)

This implements the vision of **enhanced orthogonal persistence** in Motoko that combines:
* **Stable heap**: Persisting the program main memory across canister upgrades.
* **64-bit heap**: Extending the main memory to 64-bit for large-scaled persistence.

As a result, the use of secondary storage (explicit stable memory, dedicated stable data structures, DB-like storage abstractions) will no longer be necessary: Motoko developers can directly work on their normal object-oriented program structures that are automatically persisted and retained across program version changes.

## Advantages
Compared to the existing orthogonal persistence in Motoko, this design offers:
* **Instantaneous upgrades**: New program versions immediately resume from the existing main memory and have access to the memory-compatible data.
* **Scalable upgrades**: The upgrade mechanism scales with larger heaps and in contrast to serialization, does not hit IC instruction limits.

Compared to the explicit use of stable memory, this design improves:
* **Simplicity**: Developers do not need to deal with explicit stable memory.
* **Performance**: No copying to and from the separate stable memory is necessary.

## Design
The stable heap is based on the following main properties:
* Extension of the IC to retain main memory on upgrades.
* Supporting 64-bit main memory on the IC.
* A long-term memory layout that is invariant to new compiled program versions.
* A fast memory compatibility check performed on each canister upgrade.
* Incremental garbage collection using a partitioned heap.

### IC Extension
The necessary IC extensions are implemented in a separate PR: https://github.com/dfinity/ic/pull/139
This PR is based on these extensions.

### Memory Layout
In a co-design between the compiler and the runtime system, the main memory is arranged in the following structure, invariant of the compiled program version:
* Lower 2MB: Rust call stack.
* Space between 2MB and 8MB: Reserved space Wasm data segments.
* Between 8MB and 8.25MB: Persistent metadata.
* Thereafter: Dynamic heap space. Fix start address at 8.25MB.

### Persistent Metadata
The persistent metadata describes all anchor information for the program to resume after an upgrade. 
More specifically, it comprises:
* A stable heap version that allows evolving the persistent memory layout in the future.
* The stable subset of the main actor, containing all stable variables declared in the main actor.
* A descriptor of the stable static types to check memory compatibility on upgrades.
* The reference to the null singleton object.
* The runtime state of the garbage collector, including the dynamic heap metadata and memory statistics.
* A reserve for future metadata extensions.

### Compatibility Check
Upgrades are only permitted if the new program version is compatible to the old version, such that the runtime system guarantees a compatible memory structure.

Compatible changes for immutable types are equivalent to the allowed Motoko subtype relation, e.g.
* Adding or removing actor fields.
* Removing object fields.
* Adding variant fields.
* `Nat` to `Int`.
* Shared function parameter contravariance and return type covariance.

The existing IDL-subtype functionality is reused with some adjustments to check memory compatibility: The compiler generates the type descriptor, a type table, that is recorded in the persistent metadata. Upon an upgrade, the new type descriptor is compared against the existing type descriptor, and the upgrade only succeeds for compatible changes.

This compatibility check serves as an additional safety measure on top of the DFX Candid subtype check that can be bypassed by users (when ignoring a warning). Moreoever, the memory compatibility rules is in some aspects different to the Candid sub-type check:
* Types cannot be made optional.
* Mutable types (aliases) are supported with type invariance.

### Garbage Collection
The implementation focuses on the incremental GC and abandons the other GCs because the GCs use different memory layouts. For example, the incremental GC uses a partitioned heap with objects carrying a forwarding pointer.

The incremental GC is chosen because it is designed to scale on large heaps and the stable heap design also aims to increase scalability.

The garbage collection state needs to be persisted and retained across upgrades. This is because the GC may not yet be completed at the time of an upgrade, such that object forwarding is still in use. The partition table is stored as part of the GC state.

The garbage collector uses two kinds of roots:
* Persistent roots: These refer to root objects that need to survive canister upgrades.
* Transient roots: These cover additional roots that are only valid in a specific version of a program and are discarded on an upgrade.

The persistent roots are registered in the persistent metadata and comprise:
* All stable variables of the main actor, only stored during an upgrade.
* The null singleton object.
* The stable type table.

The transient roots are referenced by the Wasm data segments and comprise:
* All canister variables of the current version, including flexible variables.

### Main Actor
On an upgrade, the main actor is recreated and existing stable variables are recovered from the persistent root. The remaining actor variables, the flexible fields as well as new stable variables, are (re)initialized.
As a result, the GC can collect unreachable flexible objects of previous canister versions. 
Unused stable variables of former versions can also be reclaimed by the GC.

### Wasm Data Segments
Wasm data segments are reinitialized on upgrade. This is necessary because data segments may contain text literals or transient GC roots that are bound to a specific new Wasm binary. The system reserves dedicated space for data segments, namely between 2MB and 8MB. Therefore, the data segments are limited to 6MB per canister Wasm in this design. Both the linker and the IC runtime system check that the data segments fit inside this reserved space.

There exist alternative design possibilities to handle data segments, see below.

### No Static Heap
The static heap is abandoned and former static objects need to be allocated in the dynamic heap.
This is because these objects may also need to survive upgrades and must not be not overwritten by new data segments. The incremental GC also operates on these objects, meaning that forwarding pointer resolution is also necessary for these objects. 

The runtime system avoids any global Wasm variables for state that needs to be preserved on upgrades. Instead, such global runtime state is stored in the persistent metadata.

Sharing optimization (pooling) is possible for compile-time-known objects, see below.

### Null Singleton
As an optimization, the top-level `null` singleton is allocated once in the dynamic heap and remembered in the persistent metadata across upgrades. This is necessary to implement null checks by pointer comparison (however, by first resolving pointer forwarding before the comparison). The null singleton needs to be part of the persistent root set.

### Migration Path
When migrating from the old serialization-based stabilization to the new stable heap, the old data is deserialized one last time from stable memory and then placed in the new stable heap layout. Once operating on the stable heap, the system prevents downgrade attempts to the old serialization-based persistence. Assuming that the persistent memory layout needs to be changed in the future, a future version of the compiler/runtime system can perform such upgrade by serializing/deserializing the heap to stable memory.

### Old Stable Memory
The old stable memory remains equally accessible as secondary memory with the new support.

## Current Limitations
* Limited data segments: Currently, data segments are limited to 6MB. This could be relaxed by using passive Wasm data segments and loading them at runtime to dynamically computed addresses in the heap. The IC would  need to be extended to support passive Wasm data segments.
* Optimization potential for statically known objects: Currently, compile-time-known objects are always dynamically allocated. For an improved performance and size, they could be shared in the dynamic heap by remembering them in an additional pool table. The pool table needs to be registered as a transient GC root and recreated on canister upgrades.
* The IC system call API is still 32-bit: We would either need to upgrade the API or copy Motoko objects to 32-bit reserved space before passing their pointers via the API. Currently, pointer arguments are only checked that they reside in the 32-bit address space.
* The incremental GC only allows 64 GB. Transitioning to a dynamic partition table would be necessary to go beyond this limit. This is to be be implemented in a separate PR.
* The floating point display format differs in Wasm64 for special values, e.g. `nan` becomes `NaN`. There is currently no support for hexadecimal floating point text formatting.
* Workaround for Rust needed to build PIC (position-independent code) libraries. Explicit use of `emscripten` via LLVM IR. 
* `ic-wasm` would need to be extended to Wasm64. The Wasm optimizations in `test/bench` are thus currently deactivated.
* The Wasm profiler is no longer applicable because the underlying `parity-wasm` crate is deprecated before Wasm64 support. A re-implementation of the profiler would be needed.

