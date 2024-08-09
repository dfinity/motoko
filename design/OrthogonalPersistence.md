# Orthogonal Persistence (Stable Heap)

This realizes the vision of keeping the canister main memory persistent even across upgrades and thus allows scalable upgrades.
Canister upgrades do no longer involve serialization and deserialization to and from secondary stable memory.

## Purpose
* **Instantenous upgrades**: New program versions simply resume on the existing main memory and have access to the memory-compatible data.
* **Scalable upgrades**: The upgrade mechanism scales with larger heaps and in contrast to serialization, does not hit IC instruction limits.

## Broader Vision
In the longer term, this approach aims to enable **true orthogonal persistence** that is simple, flexible, efficient, and scalable.
While this version implements the runtime support for 32-bit memory, this could be leveraged to 64-bit persistent main memory in future.
As a result, the use of secondary storage (explicit stable memory, dedicated stable data structures, DB-like storage abstractions) will no longer be necessary: 
Motoko developers could directly work on their normal object-oriented program structures that are automatically persisted and retained across program version changes.
With 64-bit main memory, large-scaled orthogonal persistence would be enabled, supported by the incremental GC that is designed to also scale in 64-bit.

## Design
The stable heap is based on the following main properties:
* Extension of the IC to retain main memory on upgrades.
* A long-term memory layout that is invariant to new compiled program versions.
* A fast memory compatibility check performed on each canister upgrade.
* Incremental garbage collection using a partitioned heap.

### IC Extension
As a prerequisite for the stable heap support, the IC runtime support has to be extended in order not to erase the main memory on upgrades.
This is realized in a specific IC PR (https://github.com/luc-blaeser/ic/tree/luc/stable-heap-on-release) that retains the main memory even on upgrades, similar to normal canister message execution. 

### Memory Layout
In a co-design between the compiler and the runtime system, the main memory is arranged in the following structure, invariant of the compiled program version:
* Lower 4MB: Rust call stack.
* Space between 4MB and 4.5MB: Limited reserved space Wasm data segments, only used for the Motoko runtime system.
* Between 4.5MB and 5MB: Persistent metadata.
* Thereafter: Dynamic heap space. Fix start address at 5MB.

### Persistent Metadata
The persistent metadata describes all anchor information for the program to resume after an upgrade. 
More specifically, it comprises:
* A stable heap version that allows evolving the persistent memory layout in the future.
* The stable subset of the main actor, containing all stable variables declared in the main actor.
* A descriptor of the stable static types to check memory compatibility on upgrades.
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

This compatibility check serves as an additional safety measure on top of the DFX Candid subtype check that can be bypassed by users (when ignoring a warning). Moreover, in some aspects, the memory compatibility rules differ to the Candid sub-type check:
* Top-level actor fields (`stable` fields) can change mutability (`let` to `var` and vice-versa).
* Support of variable (MutBox) with type invariance.
* Types cannot be made optional (no insertion of Option).
* Same arity for function parameters and function return types (no removed optional parameters, no additional optional results).
* Records cannot introduce additional optional fields.
* Same arity for tuple types (no insertion of optional items).
* Records and tuples are distinct.

### Garbage Collection
The implementation focuses on the incremental GC and abandons the other GCs because the GCs use different memory layouts. For example, the incremental GC uses a partitioned heap with objects carrying a forwarding pointer.

The incremental GC is chosen because it is designed to scale on large heaps and the stable heap design also aims to increase scalability. Moreover, it is suited to scale on 64-bit memory in future.

The garbage collection state needs to be persisted and retained across upgrades. 
This is because the GC may not yet be completed at the time of an upgrade, such that object forwarding is still in use. The partition table is stored as part of the GC state.

The garbage collector uses two kinds of roots:
* Persistent roots: These refer to root objects that need to survive canister upgrades.
* Transient roots: These cover additional roots that are only valid in a specific version of a program and are discarded on an upgrade.

The persistent roots are registered in the persistent metadata and comprise:
* All stable variables of the main actor, only stored during an upgrade.
* The stable type table.

The transient roots are referenced by the Wasm data segments and comprise:
* All canister variables of the current version, including flexible variables.

### Main Actor
On an upgrade, the main actor is recreated and existing stable variables are recovered from the persistent root.
The remaining actor variables, the flexible fields as well as new stable variables, are (re)initialized.
As a result, the GC can collect unreachable flexible objects of previous canister versions. 
Unused stable variables of former versions can also be reclaimed by the GC.

### No Static Heap
The static heap is abandoned and former static objects need to be allocated in the dynamic heap.
This is because these objects may also need to survive upgrades and must not be not overwritten by new data segments. 

The incremental GC also operates on these objects, meaning that forwarding pointer resolution is also necessary for these objects.

For memory and runtime efficiency, object pooling is implemented for compile-time-known constant objects (with side-effect-free initialization), i.e. those objects are already created on program initialization/upgrade in the dynamic heap and thereafter the reference to the corresponding prefabricated object is looked up whenever the constant value is needed at runtime.

The runtime systems avoids any global Wasm variables for state that needs to be preserved on upgrades.
Instead, such global runtime state is stored in the persistent metadata.

### Wasm Data Segments
Only passive Wasm data segments are used by the compiler and runtime system. In contrast to ordinary active data segments, passive segments can be explicitly loaded to a dynamic address.

This simplifies two aspects: 
* The generated Motoko code can contain arbitrarily large data segments which can be loaded to dynamic heap when needed.
* The IC can simply retain the main memory on an upgrade without needing to patch the active data segments of the new program version to the persistent memory.

However, more specific handling is required for the Rust-implemented runtime system:
The Rust-generated active data segments of the runtime system is changed to passive and loaded to the expected static address at the program start (canister initialization and upgrade).
The location and size of the RTS data segments is therefore limited to a defined reserve, see above. 
This is acceptable because the RTS only uses small size for data segments (e.g. 54KB) that is independent of the compiled Motoko program.

### Null Sentinel
As an optimization, the top-level `null` pointer is represented as a constant sentinel value pointing to the last unallocated Wasm page. This allows fast null tests without involving forwarding pointer resolution of potential non-null comparand pointers.

### Migration Path
When migrating from the old serialization-based stabilization to the new stable heap, the old data is deserialized one last time from stable memory and then placed in the new stable heap layout.
Once operating on the stable heap, the system prevents downgrade attempts to the old serialization-based persistence.

### Old Stable Memory
The old stable memory remains equally accessible as secondary memory with the new support.

## Current Limitations
* Freeing old object fields: While new program versions can drop object fields, the runtime system should also delete the redundant fields of persistent objects of previous program versions. This could be realized during garbage collection when objects are copied. For this purpose, the runtime system may maintain a set of field hashes in use and consult this table during garbage collection.
* Bounded Rust call stack size: The Rust call stack size needs to be bounded and can no longer be configured by the user.
* The Wasm profiler (only used for the flamegraphs) is no longer applicable because the underlying `parity-wasm` crate lacks full support of passive data segments. A re-implementation of the profiler would be needed.

## Related PRs

* IC with stable main memory support: https://github.com/luc-blaeser/ic/tree/luc/stable-heap-on-release
* Wasm64 Support for Motoko: https://github.com/dfinity/motoko/pull/4136
